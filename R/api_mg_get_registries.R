# Live access to MarineGEO registry tables from the Smithsonian/marinegeo-metadata
# repo, replacing the bundled copies in `marinegeo_metadata` (R/sysdata.rda) for the
# tables that are maintained there as directories of CSVs.
#
# Each registry below is a *directory* in the metadata repo holding one or more
# CSVs with matching columns. We enumerate the CSVs in a directory with a single
# GitHub git Trees API call (memoised per session), fetch each raw CSV, and
# row-bind them with `readr::read_csv()` — the same combining behaviour the
# retired `data-raw/assemble_marinegeo_metadata_sysdata.R` script performed
# locally with `list.files()`.
#
# Every directory-backed table is fetched live. When a live fetch fails (offline,
# 404, timeout, empty directory, ...) the resolver falls back to the bundled copy
# of that table, so the package keeps working without a network connection.
# Tables that are not directory-backed (`data_index`, a single root CSV, and
# `taxonomic_classifications`, computed at build time) are always returned from
# the bundled `marinegeo_metadata` object.
#
# Live fetching can be turned off entirely — forcing every table to come from the
# bundled object — by setting `options(marinegeo.utils.live_registry = FALSE)` or
# the environment variable `MARINEGEO_UTILS_OFFLINE=true`. This is useful for
# reproducible / fully offline runs (and the test suite uses it to avoid the
# network).
#
# Note: `taxonomic_classifications` is precomputed at build time from
# `taxonomic_lookup` (see .get_taxonomic_classifications() in
# R/utl_mg_traverse_adjacency_tables.R) and remains bundled. It is NOT recomputed
# from live `taxonomic_lookup`, so it can drift from the live table until the next
# sysdata rebuild. This is a known limitation, not a bug.

# GitHub sources, pinned to `main`.
.mg_raw_base <- "https://raw.githubusercontent.com/Smithsonian/marinegeo-metadata/refs/heads/main/"
.mg_tree_url <- "https://api.github.com/repos/Smithsonian/marinegeo-metadata/git/trees/main?recursive=1"

# Directory-backed registry tables: table name -> repo-relative directory.
# names() defines which tables are fetched live; everything else stays bundled.
.mg_registry_dirs <- c(
  observation_lookup = "taxonomy-and-functional-groups/observation-lookup",
  taxonomic_lookup = "taxonomy-and-functional-groups/taxonomic-lookup",
  functional_group_lookup = "taxonomy-and-functional-groups/functional-group-lookup",
  database_structure = "table-metadata/data-structure",
  categorical_values = "table-metadata/categorical-values",
  numeric_ranges = "table-metadata/numeric-ranges",
  partner_codes = "sites-and-partners/partner-codes",
  site_codes = "sites-and-partners/site-names"
)

#' List the blob (file) paths in the marinegeo-metadata repo
#'
#' The single JSON network call. Hits the GitHub git Trees API once (recursively)
#' and returns every file path in the repo, so one request covers all registry
#' directories. Kept separate from the rest of the logic so tests can swap it out
#' (along with the memoised wrapper) via `local_mocked_bindings()` and never touch
#' the network.
#'
#' @return Character vector of repo-relative file paths.
#' @noRd
.mg_list_repo_tree <- function() {
  # Fail fast when offline rather than hanging on the default timeout.
  old <- options(timeout = 30)
  on.exit(options(old), add = TRUE)

  parsed <- jsonlite::fromJSON(.mg_tree_url)
  if (isTRUE(parsed$truncated)) {
    warning(
      "GitHub tree listing for marinegeo-metadata was truncated; ",
      "some registry CSVs may be missing."
    )
  }

  tree <- parsed$tree
  tree$path[tree$type == "blob"]
}

#' Read one or more registry CSVs from raw URLs
#'
#' This is the only function that fetches CSV content. It accepts a vector of raw
#' URLs and row-binds them, mirroring how the assembly script combined the CSVs in
#' a directory. Kept separate so tests can swap it out via the memoised wrapper.
#'
#' @param urls Character vector. Raw CSV URLs.
#' @return A tibble parsed by [readr::read_csv()].
#' @noRd
.mg_read_remote_csv <- function(urls) {
  # Fail fast when offline rather than hanging on the default timeout.
  old <- options(timeout = 30)
  on.exit(options(old), add = TRUE)
  readr::read_csv(urls, show_col_types = FALSE, progress = FALSE)
}

# Memoised wrappers around .mg_list_repo_tree and .mg_read_remote_csv, assigned in
# .onLoad (see R/zzz.R). Cache per session (keyed by argument). Declared here as
# top-level bindings so they exist in the namespace and can be mocked in tests.
.mg_fetch_tree <- NULL
.mg_fetch_registry <- NULL

#' Whether to fetch registry tables live
#'
#' Defaults to `TRUE` (live). Set `options(marinegeo.utils.live_registry = FALSE)`
#' or the env var `MARINEGEO_UTILS_OFFLINE=true` to force every table to come from
#' the bundled `marinegeo_metadata` object.
#'
#' @return Logical scalar.
#' @noRd
.mg_use_live_registry <- function() {
  isTRUE(getOption(
    "marinegeo.utils.live_registry",
    !identical(tolower(Sys.getenv("MARINEGEO_UTILS_OFFLINE")), "true")
  ))
}

#' Select the direct-child CSV paths for a registry directory
#'
#' Filters a flat vector of repo paths to the CSV files that live directly in
#' `dir` (no nested subdirectories), mirroring the non-recursive `list.files()`
#' the assembly script used.
#'
#' @param paths Character vector of repo-relative file paths.
#' @param dir Character scalar. Repo-relative directory.
#' @return Character vector of matching CSV paths.
#' @noRd
.mg_filter_csv_paths <- function(paths, dir) {
  prefix <- paste0(dir, "/")
  in_dir <- startsWith(paths, prefix)
  is_csv <- grepl("\\.csv$", paths, ignore.case = TRUE)
  # Direct children only: nothing after the directory prefix may contain "/".
  rest <- substring(paths, nchar(prefix) + 1L)
  direct <- !grepl("/", rest, fixed = TRUE)
  paths[in_dir & is_csv & direct]
}

#' Resolve a registry table, preferring live data with bundled fallback
#'
#' For directory-backed tables (see `.mg_registry_dirs`) this enumerates the CSVs
#' in the table's directory (via the memoised tree listing), fetches and row-binds
#' them (memoised per session), and falls back to the bundled `marinegeo_metadata`
#' copy on any failure. All other tables are returned from `marinegeo_metadata`
#' directly.
#'
#' @param table Character scalar. Registry table name.
#' @return A data frame for the requested table.
#' @noRd
.mg_get_registry_table <- function(table) {
  if (!.mg_use_live_registry() || !table %in% names(.mg_registry_dirs)) {
    return(marinegeo_metadata[[table]])
  }

  out <- tryCatch(
    {
      paths <- .mg_fetch_tree()
      dir <- .mg_registry_dirs[[table]]
      csv_paths <- .mg_filter_csv_paths(paths, dir)
      if (length(csv_paths) == 0L) {
        stop("no CSV files found in '", dir, "'")
      }
      .mg_fetch_registry(paste0(.mg_raw_base, csv_paths))
    },
    error = function(e) NULL
  )

  if (is.null(out)) {
    message(
      "Could not fetch live registry '",
      table,
      "'; using bundled fallback."
    )
    return(marinegeo_metadata[[table]])
  }

  out
}
