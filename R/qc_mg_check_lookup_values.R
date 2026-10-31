# `lookups` and `scopes` are the same shape, so they are validated the same
# way. The messages name the argument so a caller can tell which one it got
# wrong.
.check_named_char_list <- function(x, arg) {
  if (!is.list(x) || is.data.frame(x)) {
    stop("`", arg, "` must be a named list.", call. = FALSE)
  }
  if (length(x) > 0L && (is.null(names(x)) || any(names(x) == ""))) {
    stop(
      "`",
      arg,
      "` must be a fully named list (every element must have a name).",
      call. = FALSE
    )
  }
  if (!all(vapply(x, is.character, logical(1)))) {
    stop(
      "Every element of `",
      arg,
      "` must be a character vector.",
      call. = FALSE
    )
  }
  invisible(x)
}

#' Check column values against system-wide entity registries
#'
#' @description
#' Validates that values in designated columns belong to MarineGEO's global
#' entity registries (partner codes, site names, known species/functional
#' groups). This is a referential integrity check — semantically distinct from
#' [qc_check_categorical_values()], which checks per-table controlled
#' vocabularies. `NA` values are always ignored.
#'
#' @param data A data frame to validate.
#' @param lookups A named list where each name is a column name to check and
#'   each value is a character vector of valid entries (the registry). Only
#'   columns whose names appear in both `lookups` and `data` are validated;
#'   others are silently skipped.
#'
#'   Example:
#'   ```r
#'   list(
#'     partner_code    = c("USA-IRL", "AUS-GBR", ...),
#'     scientific_name = c("Zostera marina", ...)
#'   )
#'   ```
#' @param scopes An optional named list, in the same shape as `lookups`, giving
#'   the narrower set a column is actually allowed to draw from. A column named
#'   here must additionally hold a value from `scopes[[col]]`; a column absent
#'   from it is only required to be in its registry. Entries whose name is not
#'   also in `lookups` are ignored.
#'
#'   This is how a column bound to the whole species registry is restricted to
#'   one branch of a labelset — the seagrass leaf table's `scientific_name`
#'   accepts a registered name only if it is a seagrass.
#'
#' @return A [qc_issues] tibble with one `"fail"` row per offending cell, or
#'   zero rows if every value is recognized. `row` and `col_index` are 1-based
#'   and `value` holds the offending entry. The `issue` distinguishes the two
#'   ways a value can be wrong:
#'
#'   * `"unknown_lookup"` — not in the registry at all, e.g. a misspelling.
#'   * `"out_of_labelset"` — a registered value, but outside the narrower set
#'     `scopes` allows for that column, e.g. an oyster in a seagrass table.
#'
#'   The two are mutually exclusive, so an offending cell yields exactly one
#'   row.
#'
#' @details
#' This function is called automatically by [qc_run()] whenever `data`
#' contains one or more of the recognized lookup columns (`partner_code`,
#' `site_name`, `site_code`, `scientific_name`). Both `lookups` and `scopes`
#' are resolved from the `column_lookups` registry table, which declares each
#' binding and its optional scope per `column_id`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   partner_code = c("USA-IRL", "UNKNOWN-SITE"),
#'   site_name    = c("Carkeek Park", "Not A Real Site"),
#'   stringsAsFactors = FALSE
#' )
#'
#' lookups <- list(
#'   partner_code = c("USA-IRL", "AUS-GBR"),
#'   site_name    = c("Carkeek Park", "Bodega Bay")
#' )
#'
#' qc_check_lookup_values(df, lookups)
#'
#' # The same registry, narrowed: a real species in the wrong table
#' leaves <- data.frame(
#'   scientific_name = c("Zostera marina", "Crassostrea virginica"),
#'   stringsAsFactors = FALSE
#' )
#'
#' qc_check_lookup_values(
#'   leaves,
#'   lookups = list(
#'     scientific_name = c("Zostera marina", "Crassostrea virginica")
#'   ),
#'   scopes = list(scientific_name = "Zostera marina")
#' )
qc_check_lookup_values <- function(data, lookups, scopes = list()) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  .check_named_char_list(lookups, "lookups")
  .check_named_char_list(scopes, "scopes")

  cols_to_check <- intersect(names(lookups), colnames(data))

  chunks <- lapply(cols_to_check, function(col) {
    col_pos <- which(colnames(data) == col)
    registry <- lookups[[col]]
    scope <- scopes[[col]]
    col_vals <- data[[col]]

    # Strip rank abbreviations for scientific names to match lookup convention.
    vals_to_compare <- if (col == "scientific_name") {
      .strip_rank_abbreviations(col_vals)
    } else {
      col_vals
    }

    known <- !is.na(col_vals) & vals_to_compare %in% registry

    # The two issues partition the offending cells: out of the registry, or in
    # it but outside the narrower set this column is scoped to. Reporting a
    # scope violation as `unknown_lookup` would send the submitter looking for
    # a misspelling in a name that is spelled correctly.
    bad_idx <- which(!is.na(col_vals) & !known)
    out_idx <- if (is.null(scope)) {
      integer(0)
    } else {
      which(known & !(vals_to_compare %in% scope))
    }

    if (length(bad_idx) == 0L && length(out_idx) == 0L) {
      return(NULL)
    }

    dplyr::bind_rows(
      if (length(bad_idx) > 0L) {
        .qc_issue(
          check = "qc_check_lookup_values",
          severity = "fail",
          issue = "unknown_lookup",
          row = bad_idx,
          column = col,
          col_index = col_pos,
          value = as.character(col_vals[bad_idx])
        )
      },
      if (length(out_idx) > 0L) {
        .qc_issue(
          check = "qc_check_lookup_values",
          severity = "fail",
          issue = "out_of_labelset",
          row = out_idx,
          column = col,
          col_index = col_pos,
          value = as.character(col_vals[out_idx])
        )
      }
    )
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_lookup_values"
  )
}
