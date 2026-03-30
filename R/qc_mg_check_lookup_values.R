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
#' @param detail Logical. If `TRUE` (default), the `failures` element contains
#'   a data frame with one row per offending cell. If `FALSE`, `failures` is
#'   `NULL` even when violations exist, reducing memory use in automated
#'   pipelines.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_lookup_values"`.}
#'     \item{`status`}{Character. `"fail"` if any unrecognized values are
#'       found; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with one row per validated column, showing
#'       `column_name`, `lookup_source`, and `n_violations`. `lookup_source`
#'       identifies the registry used (e.g., `"partner_codes"`,
#'       `"site_names"`, `"observation_lookup"`).}
#'     \item{`failures`}{Data frame with columns `row_index`, `col_index`,
#'       `column_name`, and `value`, or `NULL` if `status == "pass"` or
#'       `detail == FALSE`. Indices are 1-based (R convention).}
#'   }
#'
#' @details
#' The `lookup_source` shown in `summary` is derived from a fixed internal
#' mapping of column names to their source tables in `marinegeo_metadata`:
#' \itemize{
#'   \item `partner_code` → `"partner_codes"`
#'   \item `site_name`    → `"site_codes"`
#'   \item `site_code`    → `"site_codes"`
#'   \item `scientific_name` → `"observation_lookup"`
#' }
#' Any column name not in this mapping will have `lookup_source = NA`.
#'
#' This function is called automatically by [qc_run()] whenever `data`
#' contains one or more of the columns listed above.
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
qc_check_lookup_values <- function(data, lookups, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.list(lookups) || is.data.frame(lookups)) {
    stop("`lookups` must be a named list.")
  }
  if (is.null(names(lookups)) || any(names(lookups) == "")) {
    stop("`lookups` must be a fully named list (every element must have a name).")
  }
  if (!all(vapply(lookups, is.character, logical(1)))) {
    stop("Every element of `lookups` must be a character vector.")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # Fixed mapping: column name -> source table label
  .source_map <- c(
    partner_code    = "partner_codes",
    site_name       = "site_codes",
    site_code       = "site_codes",
    scientific_name = "observation_lookup"
  )

  # Only validate columns present in both lookups and data
  cols_to_check <- intersect(names(lookups), colnames(data))

  if (length(cols_to_check) == 0) {
    return(list(
      test     = "qc_check_lookup_values",
      status   = "pass",
      message  = "No lookup columns found in data. No checks performed.",
      summary  = data.frame(
        column_name   = character(0),
        lookup_source = character(0),
        n_violations  = integer(0),
        stringsAsFactors = FALSE
      ),
      failures = NULL
    ))
  }

  summary_list  <- vector("list", length(cols_to_check))
  failures_list <- vector("list", length(cols_to_check))

  for (i in seq_along(cols_to_check)) {
    col       <- cols_to_check[[i]]
    col_pos   <- which(colnames(data) == col)
    registry  <- lookups[[col]]
    col_vals  <- data[[col]]

    # NA values are not violations
    bad_idx <- which(!is.na(col_vals) & !(col_vals %in% registry))

    source_label <- unname(.source_map[col])
    if (is.na(source_label)) source_label <- NA_character_

    summary_list[[i]] <- data.frame(
      column_name   = col,
      lookup_source = source_label,
      n_violations  = length(bad_idx),
      stringsAsFactors = FALSE
    )

    if (length(bad_idx) > 0 && detail) {
      failures_list[[i]] <- data.frame(
        row_index   = bad_idx,
        col_index   = col_pos,
        column_name = col,
        value       = as.character(col_vals[bad_idx]),
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df  <- do.call(rbind, summary_list)
  failures_df <- do.call(rbind, Filter(Negate(is.null), failures_list))

  total_violations <- sum(summary_df$n_violations)

  if (total_violations > 0) {
    status  <- "fail"
    bad_cols <- summary_df$column_name[summary_df$n_violations > 0]
    msg <- paste0(
      total_violations, " unrecognized value(s) found across ",
      length(bad_cols), " column(s): ",
      paste(bad_cols, collapse = ", ")
    )
  } else {
    status      <- "pass"
    msg         <- paste0(
      "All values recognized in ", length(cols_to_check),
      " validated column(s)."
    )
    failures_df <- NULL
  }

  list(
    test     = "qc_check_lookup_values",
    status   = status,
    message  = msg,
    summary  = summary_df,
    failures = if (detail) failures_df else NULL
  )
}
