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
#'
#' @return A [qc_issues] tibble with one `"fail"` row per offending cell
#'   (`issue = "unknown_lookup"`), or zero rows if every value is recognized.
#'   `row` and `col_index` are 1-based and `value` holds the unrecognized entry.
#'
#' @details
#' This function is called automatically by [qc_run()] whenever `data`
#' contains one or more of the recognized lookup columns (`partner_code`,
#' `site_name`, `site_code`, `scientific_name`).
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
qc_check_lookup_values <- function(data, lookups) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.list(lookups) || is.data.frame(lookups)) {
    stop("`lookups` must be a named list.")
  }
  if (is.null(names(lookups)) || any(names(lookups) == "")) {
    stop(
      "`lookups` must be a fully named list (every element must have a name)."
    )
  }
  if (!all(vapply(lookups, is.character, logical(1)))) {
    stop("Every element of `lookups` must be a character vector.")
  }

  cols_to_check <- intersect(names(lookups), colnames(data))

  chunks <- lapply(cols_to_check, function(col) {
    col_pos <- which(colnames(data) == col)
    registry <- lookups[[col]]
    col_vals <- data[[col]]

    # Strip rank abbreviations for scientific names to match lookup convention.
    vals_to_compare <- if (col == "scientific_name") {
      .strip_rank_abbreviations(col_vals)
    } else {
      col_vals
    }

    bad_idx <- which(!is.na(col_vals) & !(vals_to_compare %in% registry))
    if (length(bad_idx) == 0L) {
      return(NULL)
    }

    bad_vals <- as.character(col_vals[bad_idx])

    .qc_issue(
      check = "qc_check_lookup_values",
      severity = "fail",
      issue = "unknown_lookup",
      row = bad_idx,
      column = col,
      col_index = col_pos,
      value = bad_vals
    )
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_lookup_values"
  )
}
