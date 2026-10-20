#' Check that point counts in a quadrat sum to the points in that quadrat
#'
#' @description
#' Validates that the primary point counts recorded for a quadrat add up to the
#' quadrat's declared number of points, excluding canopy taxa (which are scored
#' over the top of the primary layer and so are not part of the total).
#'
#' @section Superseded:
#' This is now a thin wrapper around [qc_check_aggregate()], which expresses the
#' same rule from `aggregate_rules` metadata — the grouping key, the aggregate,
#' the comparison and the canopy-taxa filter are all parameters there rather
#' than literals in R. That is what lets the structurally identical fouling
#' panel total (`panel_id` / `points_in_grid`) be expressed as a CSV row instead
#' of a second copy of this function, and [qc_run()] now dispatches
#' `qc_check_aggregate()` in its place.
#'
#' This wrapper is kept so existing direct callers keep working. Prefer an
#' `aggregate_rules` row for new rules.
#'
#' @param data A data frame to validate. Must contain `site_name`, `transect`,
#'   `quadrat`, `point_count` and `points_in_quadrat`; if any are absent no
#'   issues are reported. A `cover_type` column, where present, is used to
#'   exclude `"canopy taxa"` rows from the total.
#'
#' @return A [qc_issues] tibble with one row per offending data row.
#'
#' @seealso [qc_check_aggregate()] for the general form.
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   site_name = "Site A",
#'   transect = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
#'   cover_type = c(
#'     "live_oyster", "box_oyster", "cultch", "hash", "sediment",
#'     "live_oyster", "box_oyster", "cultch", "hash", "sediment"
#'   ),
#'   quadrat = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#'   point_count = c(9, 13, 20, 34, 4, 9, 13, 20, 34, 5),
#'   points_in_quadrat = c(81, 81, 81, 81, 81, 81, 81, 81, 81, 81)
#' )
#'
#' qc_check_point_counts(data)
qc_check_point_counts <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  required_cols <- c(
    "site_name",
    "transect",
    "quadrat",
    "point_count",
    "points_in_quadrat"
  )

  empty <- function() {
    new_qc_issues(
      n_rows = nrow(data),
      checks_run = "qc_check_point_counts"
    )
  }

  # Preserved from the original: a table without these columns is simply not
  # subject to this rule, and reports nothing.
  if (!all(required_cols %in% colnames(data))) {
    return(empty())
  }

  rules <- data.frame(
    rule_id = NA_character_,
    group_columns = "site_name|transect|quadrat",
    value_column = "point_count",
    aggregate_function = "sum",
    comparison = "==",
    target_column = "points_in_quadrat",
    target_value = NA_real_,
    tolerance = NA_real_,
    severity = "fail",
    stringsAsFactors = FALSE
  )

  if ("cover_type" %in% colnames(data)) {
    # Canopy taxa are scored above the primary layer, so they are filtered out
    # before the total is taken. Routing that through the condition layer -- the
    # one row-filtering mechanism -- is also what fixes the row numbers: the
    # original indexed the post-filter frame, so a canopy row appearing before
    # the offenders shifted every reported row.
    rules$condition_id <- "primary_points"
    conditions <- data.frame(
      condition_id = "primary_points",
      table_id = NA_character_,
      column_name = "cover_type",
      operator = "not_in",
      value = "canopy taxa",
      description = "excluding canopy taxa from the primary point total",
      stringsAsFactors = FALSE
    )
    out <- .qc_apply_conditional(
      qc_check_aggregate,
      data = data,
      rules = rules,
      conditions = conditions
    )
  } else {
    out <- qc_check_aggregate(data, rules)
  }

  # Report under this function's own name, so a direct caller's result is
  # unchanged by the delegation.
  if (nrow(out) > 0) {
    out$check <- "qc_check_point_counts"
  }

  new_qc_issues(
    out,
    n_rows = nrow(data),
    checks_run = "qc_check_point_counts"
  )
}
