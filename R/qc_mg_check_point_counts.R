#' Check that the total number of point counts for each quadrat totals the number in "points in quadrat" 
#'
#' @description
#' Validates that the total number of primary point counts for a given 
#' quadrat adds up to the number of points in the quadrat
#' Designed for use in MarineGEO QA/QC pipelines and can be called
#' directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#'
#' @return A [qc_issues] tibble with one row per quadrat-transect combination that violates the point-count rules. 
#'
#' @details
#'
#' @export
#'
#' @examples
# data <- data.frame(
#   transect = c(1,1,1,1,1,2,2,2,2,2),
#   quadrat = c(5,5,5,5,5,5,5,5,5,5),
#   point_count = c(9,13,20,34,4,9,13,20,34,5),
#   points_in_quadrat= c(81,81,81,81,81,81,81,81,81,81)
# )
# 
# qc_check_point_counts(data)


  
qc_check_point_counts <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  required_cols <- c(
    "transect",
    "quadrat",
    "point_count",
    "points_in_quadrat"
  )
  
  if (all(required_cols %in% colnames(data))) {

  
  invalid_pointcount_summary_df <- data |>
    dplyr::group_by(transect, quadrat) |>
    dplyr::summarise(
      total_point_count = sum(point_count, na.rm = TRUE),
      points_in_quadrat = dplyr::first(points_in_quadrat),
      .groups = "drop"
    ) |>
    dplyr::filter(total_point_count != points_in_quadrat)
  
  
  if (nrow(data_fail) == 0) {
    chunks <- NULL
  } else {
    invalid_keys <- paste(
      invalid_pointcount_summary_df$transect,
      invalid_pointcount_summary_df$quadrat
    )
    
    row_ids <- which(
      paste(data$transect, data$quadrat) %in% invalid_keys
    )
    
    chunks <- .qc_issue(
      check = "qc_check_point_counts",
      severity = "fail",
      issue = "invalid_point_count_total",
      row = row_ids,
      column = "point_count",
      col_index = which(names(data) == "point_count"),
      value = as.character(data_fail$total_point_count)
    )
  }
  }
  
  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_point_counts"
  )
  
}
  
  
  