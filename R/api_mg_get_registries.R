# Live access to MarineGEO registry tables from Smithsonian/marinegeo-metadata
#
# The metadata repo publishes generated JSON bundles under dist/, built from
# its authored CSV sources by R/generate-metadata-dist.R. This package reads
# those bundles.
#
# Types are read from the manifest's declared schema, never inferred.
#
# Four options control resolution, in precedence order:
#
#   marinegeo.utils.metadata_local   Path to a local marinegeo-metadata
#                                    checkout; read from disk with NO fallback.
#   marinegeo.utils.live_registry    FALSE (or MARINEGEO_UTILS_OFFLINE=true)
#                                    forces the offline object.
#   marinegeo.utils.metadata_ref     Git ref to fetch (default "main").
#   marinegeo.utils.strict_registry  TRUE turns a failed fetch into an error
#                                    instead of an offline fallback.
#
# The dist/ schema major version this package understands. A newer major means
# the bundle layout changed in a way this code cannot read, which must be a
# loud error
.mg_dist_schema_version <- 1L

# The oldest dist/ schema major version still readable.
.mg_dist_schema_version_min <- 1L

# The columns each published rule table must carry. A table read with a column
# missing or renamed is read downstream as "no rules", so it is caught on the
# way in rather than surfacing as a clean pass that checked nothing.
#
# Two entries differ from that repo's authored headers on purpose:
#   * `numeric_ranges` requires `rule_id`, which is NOT authored -- the metadata
#     generator derives it and publishes it, so it exists in the bundle but not
#     in the CSVs.

.mg_registry_required_cols <- list(
  database_structure = c(
    "protocol",
    "table_id",
    "column_name",
    "data_type",
    "missing_values",
    "uuid_identity",
    "column_id"
  ),
  column_ids = c("column_id", "label", "unit"),
  numeric_ranges = c(
    "rule_id",
    "column_id",
    "scope_type",
    "scope_value",
    "range_type",
    "max_fail",
    "min_fail",
    "max_warn",
    "min_warn"
  ),
  controlled_vocabularies = c("column_id", "value"),
  column_lookups = c("column_id", "registry_table", "registry_column")
)

.mg_manifest_rel <- "dist/manifest.json"

#' The git ref to read metadata from
#'
#' Defaults to `"main"`. Set `options(marinegeo.utils.metadata_ref = "my-branch")`
#' to resolve registries against an unmerged branch or a release tag.
#'
#' @return Character scalar. A branch name, tag, or commit SHA.
#' @noRd
.mg_metadata_ref <- function() {
  ref <- getOption("marinegeo.utils.metadata_ref", "main")
  if (!is.character(ref) || length(ref) != 1L || !nzchar(ref)) {
    stop("`marinegeo.utils.metadata_ref` must be a non-empty character scalar.")
  }
  ref
}

#' Path to a local marinegeo-metadata checkout, if one is configured
#'
#' Set `options(marinegeo.utils.metadata_local = "<path>")` to read registries
#' from a working tree instead of GitHub. This is the only way to validate
#' metadata changes before they are merged, since the network path resolves a
#' ref in the remote repo.
#'
#' @return Character scalar path, or `NULL` when unset.
#' @noRd
.mg_metadata_local <- function() {
  root <- getOption("marinegeo.utils.metadata_local", NULL)
  if (is.null(root)) {
    return(NULL)
  }
  if (!is.character(root) || length(root) != 1L || !nzchar(root)) {
    stop(
      "`marinegeo.utils.metadata_local` must be a non-empty character scalar."
    )
  }
  root
}

#' Whether a failed live fetch is an error rather than a fallback
#'
#'  Set `options(marinegeo.utils.strict_registry = TRUE)`.
#'
#' @return Logical scalar.
#' @noRd
.mg_strict_registry <- function() {
  isTRUE(getOption("marinegeo.utils.strict_registry", FALSE))
}

#' Whether to fetch registry tables live
#'
#' Defaults to `TRUE` (live). Set `options(marinegeo.utils.live_registry = FALSE)`
#' or the env var `MARINEGEO_UTILS_OFFLINE=true` to force every table to come
#' from the offline `marinegeo_metadata` object.
#'
#' @return Logical scalar.
#' @noRd
.mg_use_live_registry <- function() {
  isTRUE(getOption(
    "marinegeo.utils.live_registry",
    !identical(tolower(Sys.getenv("MARINEGEO_UTILS_OFFLINE")), "true")
  ))
}

#' Base URL for raw metadata file content
#'
#' @return Character scalar ending in "/".
#' @noRd
.mg_raw_base <- function(ref = .mg_metadata_ref()) {
  paste0(
    "https://raw.githubusercontent.com/Smithsonian/marinegeo-metadata/",
    ref,
    "/"
  )
}

#' URL of a repo-relative metadata file at a ref
#' @noRd
.mg_file_url <- function(rel, ref = .mg_metadata_ref()) {
  paste0(.mg_raw_base(ref), rel)
}

#' Read and parse one JSON document from a URL or path
#'
#' The only function that fetches bundle content. `simplifyVector = FALSE` is
#' deliberate: simplification would guess column types from the data, and an
#' all-null column would come back wrong. Columns are built from the manifest's
#' declared schema instead.
#'
#' @param src Character scalar. A URL or a local file path.
#' @return Parsed JSON as nested lists.
#' @noRd
.mg_read_json <- function(src) {
  # Fail fast when offline rather than hanging on the default timeout.
  old <- options(timeout = 30)
  on.exit(options(old), add = TRUE)
  jsonlite::fromJSON(src, simplifyVector = FALSE)
}

# Memoised wrapper around .mg_read_json, assigned in .onLoad (see R/zzz.R).
# Caches per source, so the manifest and each bundle are fetched once per
# session and a QC run touching several tables in one bundle costs one
# request.
.mg_fetch_json <- NULL

#' Check a bundle manifest's schema version
#'
#' @noRd
.mg_check_schema_version <- function(manifest, src) {
  ver <- manifest$schema_version
  if (is.null(ver) || !is.numeric(ver) && !is.character(ver)) {
    stop("'", src, "' has no schema_version", call. = FALSE)
  }
  ver <- suppressWarnings(as.integer(ver))
  if (is.na(ver)) {
    stop("'", src, "' has an unreadable schema_version", call. = FALSE)
  }
  if (ver < .mg_dist_schema_version_min) {
    stop(
      "'",
      src,
      "' publishes schema_version ",
      ver,
      " but this version of marinegeo.utils requires at least ",
      .mg_dist_schema_version_min,
      ". That metadata predates the `na_values:`/`fail` rename -- regenerate ",
      "dist/ from marinegeo-metadata.",
      call. = FALSE
    )
  }
  if (ver > .mg_dist_schema_version) {
    stop(
      "marinegeo-metadata publishes schema_version ",
      ver,
      " but this version of marinegeo.utils understands ",
      .mg_dist_schema_version,
      ". Upgrade marinegeo.utils -- continuing would risk running QC with ",
      "rules this package cannot read.",
      call. = FALSE
    )
  }
  invisible(ver)
}

#' Build a typed tibble from JSON rows and a declared column schema
#'
#' @param rows List of row objects from the bundle.
#' @param schema The manifest entry for this table (`rows`, `columns`).
#' @param table Character scalar, for error messages.
#' @return A tibble with exactly the declared columns and types.
#' @noRd
.mg_coerce_registry_table <- function(rows, schema, table) {
  if (is.null(schema) || is.null(schema$columns)) {
    stop("no column schema for table '", table, "'", call. = FALSE)
  }
  if (is.null(rows)) {
    stop("table '", table, "' is missing from its bundle", call. = FALSE)
  }

  declared <- as.integer(schema$rows)
  if (!is.na(declared) && length(rows) != declared) {
    stop(
      "table '",
      table,
      "' has ",
      length(rows),
      " rows but the manifest declares ",
      declared,
      call. = FALSE
    )
  }

  names_ <- vapply(
    schema$columns,
    function(c) as.character(c$name),
    character(1)
  )
  types_ <- vapply(
    schema$columns,
    function(c) as.character(c$type),
    character(1)
  )

  out <- lapply(seq_along(names_), function(i) {
    nm <- names_[[i]]
    pick <- function(r) r[[nm]]
    switch(
      types_[[i]],
      character = vapply(
        rows,
        function(r) {
          v <- pick(r)
          if (is.null(v)) NA_character_ else as.character(v)[[1]]
        },
        character(1)
      ),
      double = vapply(
        rows,
        function(r) {
          v <- pick(r)
          if (is.null(v)) NA_real_ else as.numeric(v)[[1]]
        },
        numeric(1)
      ),
      logical = vapply(
        rows,
        function(r) {
          v <- pick(r)
          if (is.null(v)) NA else as.logical(v)[[1]]
        },
        logical(1)
      ),
      date = as.Date(vapply(
        rows,
        function(r) {
          v <- pick(r)
          if (is.null(v)) NA_character_ else as.character(v)[[1]]
        },
        character(1)
      )),
      stop(
        "unknown declared type '",
        types_[[i]],
        "' for ",
        table,
        ".",
        nm,
        call. = FALSE
      )
    )
  })
  names(out) <- names_

  # A QC rule table missing a column is read downstream as "no rules", so the
  # mismatch is caught here instead.
  required <- .mg_registry_required_cols[[table]]
  if (!is.null(required) && !all(required %in% names_)) {
    stop(
      "table '",
      table,
      "' is missing required column(s): ",
      paste(setdiff(required, names_), collapse = ", "),
      call. = FALSE
    )
  }

  tibble::as_tibble(out)
}

#' Resolve one registry table from a dist/ source
#'
#' @param locate Function taking a repo-relative path and returning a URL or
#'   local path.
#' @return A tibble carrying the manifest's schema version in an
#'   `mg_schema_version` attribute, or `NULL` when the manifest does not
#'   distribute `table`.
#' @noRd
.mg_registry_from_dist <- function(table, locate) {
  man_src <- locate(.mg_manifest_rel)
  manifest <- .mg_fetch_json(man_src)
  ver <- .mg_check_schema_version(manifest, man_src)

  bname <- manifest$table_index[[table]]
  if (is.null(bname)) {
    # Not published in dist/; the offline copy is the correct source.
    return(NULL)
  }
  bname <- as.character(bname)[[1]]

  entry <- manifest$bundles[[bname]]
  if (is.null(entry)) {
    stop(
      "manifest names bundle '",
      bname,
      "' but does not describe it",
      call. = FALSE
    )
  }

  bundle <- .mg_fetch_json(locate(paste0("dist/", bname, ".json")))
  out <- .mg_coerce_registry_table(
    bundle$tables[[table]],
    entry$tables[[table]],
    table
  )

  # Ride the version back to the caller on the table itself rather than through
  # a second manifest read. .mg_get_registry_table() records it and strips the
  # attribute, so it never reaches a caller of the registry.
  attr(out, "mg_schema_version") <- ver
  out
}

# ---------------------------------------------------------------------------
# Provenance
#
# Which source each registry table was actually served from, recorded as a side
# effect of resolving it. Keyed by table name and overwritten on every resolution.
# ---------------------------------------------------------------------------
.mg_registry_env <- new.env(parent = emptyenv())
.mg_registry_env$source <- list()

# `source` is one of "local", "live" or "offline"; `detail` carries the path,
# ref or failure reason behind it.
#
# `fallback` separates the two ways a table ends up offline. Asking for offline
# mode, or reading a table the manifest simply does not publish, is a correct
# outcome.
#
# `schema_version` is the dist/ schema major version the manifest declared, or
# NA for an offline table -- no manifest was read, and the baked copy records no
# version of its own.
.mg_note_registry_source <- function(
  table,
  source,
  detail = NA_character_,
  fallback = FALSE,
  schema_version = NA_integer_
) {
  .mg_registry_env$source[[table]] <- list(
    source = source,
    detail = detail,
    fallback = fallback,
    schema_version = schema_version
  )
  invisible(NULL)
}

# The provenance of every registry table resolved so far this session, as a
# tibble. Empty before the first resolution.
.mg_registry_sources <- function() {
  recorded <- .mg_registry_env$source
  # USE.NAMES = FALSE throughout: the list is keyed by table name, and letting
  # those names ride along would give every column the same names attribute.
  pluck <- function(f, type) {
    vapply(recorded, f, type, USE.NAMES = FALSE)
  }
  tibble::tibble(
    table = if (length(recorded) == 0) character(0) else names(recorded),
    source = pluck(function(x) x$source, character(1)),
    fallback = pluck(function(x) isTRUE(x$fallback), logical(1)),
    # Tolerant of a record written before this field existed.
    schema_version = pluck(
      function(x) {
        if (is.null(x$schema_version)) {
          NA_integer_
        } else {
          as.integer(x$schema_version)
        }
      },
      integer(1)
    ),
    detail = pluck(function(x) as.character(x$detail), character(1))
  )
}

# The subset of `tables` whose rules fell back to the offline copy against the
# caller's intent.
.mg_stale_registry_tables <- function(tables) {
  recorded <- .mg_registry_env$source[intersect(
    tables,
    names(.mg_registry_env$source)
  )]
  stale <- vapply(recorded, function(x) isTRUE(x$fallback), logical(1))
  names(recorded)[stale]
}

# A one-line description of the dist/ schema version behind `tables`, for the
# QC console report: "rules schema v1, ref main", "rules schema v1, local
# checkout", or "rules schema: offline" when nothing read a manifest. NULL when
# none of `tables` has been resolved this session, so a caller that never
# touched the registry prints nothing at all.
#
# A session that switches ref or checkout mid-run can hold two versions at once;
# they are listed rather than hidden.
.mg_rules_schema_label <- function(tables) {
  recorded <- .mg_registry_env$source[intersect(
    tables,
    names(.mg_registry_env$source)
  )]
  if (length(recorded) == 0) {
    return(NULL)
  }

  vers <- vapply(
    recorded,
    function(x) {
      if (is.null(x$schema_version)) {
        NA_integer_
      } else {
        as.integer(x$schema_version)
      }
    },
    integer(1),
    USE.NAMES = FALSE
  )
  known <- !is.na(vers)
  if (!any(known)) {
    return("rules schema: offline")
  }

  label <- paste0(
    "rules schema v",
    paste(sort(unique(vers[known])), collapse = "/")
  )

  # Where those versioned tables came from. `detail` holds the ref for a live
  # table and the checkout path for a local one; the path is not worth printing,
  # so a local read is named rather than spelled out.
  srcs <- vapply(
    recorded[known],
    function(x) as.character(x$source),
    character(1),
    USE.NAMES = FALSE
  )
  refs <- vapply(
    recorded[known],
    function(x) as.character(x$detail),
    character(1),
    USE.NAMES = FALSE
  )
  origin <- unique(ifelse(
    srcs == "live",
    paste("ref", refs),
    "local checkout"
  ))
  label <- paste0(label, ", ", paste(origin, collapse = "/"))

  # Some rules versioned, some not: the report already names the offline tables
  # on their own line, so this only has to stop the version reading as though it
  # covered everything.
  if (any(!known)) {
    label <- paste0(label, "; some offline")
  }
  label
}

#' Where registry tables were loaded from this session
#'
#' @description
#' Reports, for every registry table resolved so far in this session, whether it
#' came from a local `marinegeo-metadata` checkout, the live `dist/` bundles on
#' GitHub, or the offline copy baked into the package.
#'
#'
#' @return A [tibble][tibble::tibble] with one row per resolved table:
#'   \describe{
#'     \item{`table`}{Registry table name, e.g. `"numeric_ranges"`.}
#'     \item{`source`}{`"local"`, `"live"` or `"offline"`.}
#'     \item{`fallback`}{`TRUE` only where live metadata was wanted and could
#'       not be read. Running offline on purpose, or reading a table the
#'       manifest does not publish, is `FALSE` — those are correct outcomes,
#'       not degraded ones.}
#'     \item{`schema_version`}{Integer. The `dist/` schema major version the
#'       manifest declared, or `NA` for an offline table — the baked copy
#'       records no version of its own.}
#'     \item{`detail`}{The checkout path, the git ref, or the reason the live
#'       fetch failed.}
#'   }
#'   Zero rows before any registry table has been resolved.
#'
#' @seealso [qc_run()], whose result carries a `stale_registries` attribute
#'   naming the rule tables that fell back.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' utl_mg_list_tables()
#' utl_mg_registry_status()
#' }
utl_mg_registry_status <- function() {
  .mg_registry_sources()
}

#' Resolve a registry table, preferring live data with offline fallback
#'
#' Reads the generated `dist/` bundles, falling back to the offline
#' `marinegeo_metadata` copy on any failure so the package keeps working
#' offline. A configured local checkout takes precedence over both and errors
#' rather than falling back; see `.mg_metadata_local()`.
#'
#' Every return path records its provenance via `.mg_note_registry_source()`,
#' so a silent fallback leaves a trace the caller can read back.
#'
#' @param table Character scalar. Registry table name.
#' @return A data frame for the requested table.
#' @noRd
.mg_get_registry_table <- function(table) {
  local_root <- .mg_metadata_local()
  if (!is.null(local_root)) {
    out <- .mg_registry_from_dist(
      table,
      function(rel) {
        path <- file.path(local_root, rel)
        if (!file.exists(path)) {
          stop(
            "'",
            path,
            "' not found. Run R/generate-metadata-dist.R in the ",
            "metadata repo to build dist/.",
            call. = FALSE
          )
        }
        path
      }
    )
    if (is.null(out)) {
      .mg_note_registry_source(
        table,
        "offline",
        paste0("not published in dist/ at '", local_root, "'")
      )
      return(marinegeo_metadata[[table]])
    }
    .mg_note_registry_source(
      table,
      "local",
      local_root,
      schema_version = attr(out, "mg_schema_version")
    )
    attr(out, "mg_schema_version") <- NULL
    return(out)
  }

  if (!.mg_use_live_registry()) {
    .mg_note_registry_source(
      table,
      "offline",
      "live registry disabled (marinegeo.utils.live_registry / MARINEGEO_UTILS_OFFLINE)"
    )
    return(marinegeo_metadata[[table]])
  }

  ref <- .mg_metadata_ref()
  reason <- NULL
  out <- tryCatch(
    .mg_registry_from_dist(table, function(rel) .mg_file_url(rel, ref)),
    error = function(e) {
      msg <- paste0(
        "Could not fetch live registry '",
        table,
        "' at ref '",
        ref,
        "': ",
        conditionMessage(e)
      )
      if (.mg_strict_registry()) {
        stop(msg, call. = FALSE)
      }
      reason <<- msg
      # A warning, not a message: a message is invisible in a non-interactive
      # script, which is where a silent fallback does the most damage.
      warning(msg, "; using offline fallback.", call. = FALSE)
      NULL
    }
  )

  if (is.null(out)) {
    # Either the fetch failed (`reason` set by the handler) -- a real fallback
    # -- or the manifest simply does not publish this table, in which case the
    # offline copy is the intended source and nothing is stale.
    .mg_note_registry_source(
      table,
      "offline",
      if (is.null(reason)) {
        paste0("not published in dist/ at ref '", ref, "'")
      } else {
        reason
      },
      fallback = !is.null(reason)
    )
    return(marinegeo_metadata[[table]])
  }
  .mg_note_registry_source(
    table,
    "live",
    ref,
    schema_version = attr(out, "mg_schema_version")
  )
  attr(out, "mg_schema_version") <- NULL
  out
}
