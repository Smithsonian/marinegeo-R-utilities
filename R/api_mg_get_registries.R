# Live access to MarineGEO registry tables from the Smithsonian/marinegeo-metadata
# repo, replacing the bundled copies in `marinegeo_metadata` (R/sysdata.rda) for the
# tables that change most frequently.
#
# Only `observation_lookup` and `taxonomic_lookup` are fetched live. Every other
# registry table continues to come from the bundled `marinegeo_metadata` object.
# When a live fetch fails (offline, 404, timeout, ...) the resolver falls back to
# the bundled copy of that table, so the package keeps working without a network
# connection.
#
# Note: `taxonomic_classifications` is precomputed at build time from
# `taxonomic_lookup` (see .get_taxonomic_classifications() in
# R/utl_mg_traverse_adjacency_tables.R) and remains bundled. It is NOT recomputed
# from live `taxonomic_lookup`, so it can drift from the live table until the next
# sysdata rebuild. This is a known limitation, not a bug.

# Raw GitHub CSV sources for the migrated tables, pinned to `main`.
.mg_registry_urls <- c(
  observation_lookup = "https://raw.githubusercontent.com/Smithsonian/marinegeo-metadata/refs/heads/main/taxonomy-and-functional-groups/observation-lookup/marinegeo_observation_ids.csv",
  taxonomic_lookup = "https://raw.githubusercontent.com/Smithsonian/marinegeo-metadata/refs/heads/main/taxonomy-and-functional-groups/taxonomic-lookup/marinegeo_taxonomic_lookup.csv"
)

#' Read a registry CSV from a URL
#'
#' This is the only function that actually makes a network call. It is kept
#' separate from the rest of the logic so tests can swap it out (along with the
#' memoised wrapper) via `local_mocked_bindings()` and never touch the network.
#'
#' @param url Character scalar. Raw CSV URL.
#' @return A tibble parsed by [readr::read_csv()].
#' @noRd
.mg_read_remote_csv <- function(url) {
  # Fail fast when offline rather than hanging on the default timeout.
  old <- options(timeout = 30)
  on.exit(options(old), add = TRUE)
  readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
}

# Memoised wrapper around .mg_read_remote_csv, assigned in .onLoad (see R/zzz.R).
# Caches per session, keyed by URL. Declared here as a top-level binding so it
# exists in the namespace and can be mocked in tests.
.mg_fetch_registry <- NULL

#' Resolve a registry table, preferring live data with bundled fallback
#'
#' For migrated tables (`observation_lookup`, `taxonomic_lookup`) this fetches the
#' live CSV (memoised per session) and falls back to the bundled
#' `marinegeo_metadata` copy on any failure. All other tables are returned from
#' `marinegeo_metadata` directly.
#'
#' @param table Character scalar. Registry table name.
#' @return A data frame for the requested table.
#' @noRd
.mg_get_registry_table <- function(table) {
  if (!table %in% names(.mg_registry_urls)) {
    return(marinegeo_metadata[[table]])
  }
  url <- .mg_registry_urls[[table]]

  out <- tryCatch(.mg_fetch_registry(url), error = function(e) NULL)
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
