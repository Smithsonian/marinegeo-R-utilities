# Live access to MarineGEO registry tables from the Smithsonian/marinegeo-metadata
# repo, replacing the offline copies in `marinegeo_metadata` (R/sysdata.rda).
#
# The metadata repo publishes generated JSON bundles under dist/, built from
# its authored sources (nested YAML for rule configs, CSV for the dense
# registries) by R/generate-metadata-dist.R. This package reads those bundles.
#
# Bundles are split by how often they change, so a one-line rule edit does not
# invalidate a cached taxonomy payload:
#
#   dist/qc-metadata.json   ~6 KB gz    volatile  6 QC rule tables
#   dist/sites.json         ~8 KB gz    medium    partners, sites, data index
#   dist/taxonomy.json    ~122 KB gz    stable    machine-generated lookups
#   dist/manifest.json                  index, hashes, and column type schemas
#
# This replaces a directory-of-CSVs scheme that cost one GitHub Trees API call
# plus 46 raw file fetches per cold load (16 of them for QC metadata alone).
# A cold QC load is now one manifest fetch plus one ~6 KB bundle fetch, and
# the Trees API -- with its 60-request/hour unauthenticated limit -- is no
# longer used at all.
#
# Types are read from the manifest's declared schema, never inferred. JSON
# cannot distinguish a null from a typed NA, so an all-null column would
# otherwise come back as the wrong type; this is the same trap that made
# `uuid_identity` depend on readr guessing right, and R/qc_mg_run.R calls
# which() on that column.
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
# Note: wide-form taxonomic classifications are no longer shipped in the
# package. They are reconstructed on demand from the live `taxonomic_lookup` by
# utl_mg_get_taxonomic_classifications() (see R/utl_mg_traverse_adjacency_tables.R).

# The dist/ schema major version this package understands. A newer major means
# the bundle layout changed in a way this code cannot read, which must be a
# loud error -- the QC engine reads a malformed rule table as "no rules" and
# reports a clean pass having run no checks.
.mg_dist_schema_version <- 1L

# The oldest dist/ schema major version still readable. A dist/ built against
# an older layout than this would be read with renamed or dropped keys absent,
# filtering to zero rules and reporting a clean pass having checked nothing --
# the same silent failure, from the other direction.
.mg_dist_schema_version_min <- 1L

.mg_manifest_rel <- "dist/manifest.json"

#' The git ref to read metadata from
#'
#' Defaults to `"main"`. Set `options(marinegeo.utils.metadata_ref = "my-branch")`
#' to resolve registries against an unmerged branch or a release tag. Pinning to
#' a tag is what makes a QC run reproducible: with the default, merging a
#' metadata PR changes QC behaviour for every user immediately.
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
#' Deliberately has no offline fallback: a local override is an explicit request
#' for *these* files, so a missing or stale `dist/` must be an error. Falling
#' back would silently validate against the wrong rules.
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
#' The offline fallback is right for a field user with no network and exactly
#' wrong for a job whose purpose is to validate metadata. Set
#' `options(marinegeo.utils.strict_registry = TRUE)` in validation and CI.
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
  required <- .mg_flat_schema[[table]]
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
#' @return A tibble, or `NULL` when the manifest does not distribute `table`.
#' @noRd
.mg_registry_from_dist <- function(table, locate) {
  man_src <- locate(.mg_manifest_rel)
  manifest <- .mg_fetch_json(man_src)
  .mg_check_schema_version(manifest, man_src)

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
  .mg_coerce_registry_table(
    bundle$tables[[table]],
    entry$tables[[table]],
    table
  )
}

#' Resolve a registry table, preferring live data with offline fallback
#'
#' Reads the generated `dist/` bundles, falling back to the offline
#' `marinegeo_metadata` copy on any failure so the package keeps working
#' offline. A configured local checkout takes precedence over both and errors
#' rather than falling back; see `.mg_metadata_local()`.
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
      return(marinegeo_metadata[[table]])
    }
    return(out)
  }

  if (!.mg_use_live_registry()) {
    return(marinegeo_metadata[[table]])
  }

  ref <- .mg_metadata_ref()
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
      # A warning, not a message: a message is invisible in a non-interactive
      # script, which is where a silent fallback does the most damage.
      warning(msg, "; using offline fallback.", call. = FALSE)
      NULL
    }
  )

  if (is.null(out)) {
    return(marinegeo_metadata[[table]])
  }
  out
}
