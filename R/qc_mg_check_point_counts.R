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
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'  transect = c(1,1,1,1,1,2,2,2,2,2),
#'   cover_type = c("live_oyster", "box_oyster", "cultch","hash","sediment","live_oyster", "box_oyster", "cultch","hash","sediment" ),
#'   quadrat = c(5,5,5,5,5,5,5,5,5,5),
#'   point_count = c(9,13,20,34,4,9,13,20,34,5),
#'   points_in_quadrat= c(81,81,81,81,81,81,81,81,81,81)
#' )
#' 
#' output_2 <- qc_check_point_counts(df_out)


  
qc_check_point_counts <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  required_cols <- c(
    "transect",
    "quadrat",
    "point_count",
    "points_in_quadrat",
    "site_name"
  )
  
  if (all(required_cols %in% colnames(data))) {
    
    if("cover_type" %in% colnames(data)){
    
    df_primary_point_count <- data |>
      dplyr::filter(cover_type != "canopy taxa")|>
      dplyr::group_by(site_name, transect, quadrat) |>
      dplyr::mutate(
        total_point_count = sum(point_count, na.rm = TRUE)
      )|>
      dplyr::ungroup()
    
    }else{
      df_primary_point_count <- data   |>
        dplyr::group_by(site_name, transect, quadrat) |>
        dplyr::mutate(
          total_point_count = sum(point_count, na.rm = TRUE)
        )|>
        dplyr::ungroup()
    }
      
      
    df_invalid_point_count <- df_primary_point_count |>
      dplyr::filter(total_point_count != points_in_quadrat)
    

  if (nrow(df_invalid_point_count) == 0) {
    chunks <- NULL
  } else {
    row_ids <- which(
      df_primary_point_count$total_point_count != df_primary_point_count$points_in_quadrat
    )
    
    chunks <- .qc_issue(
      check = "qc_check_point_counts",
      severity = "fail",
      issue = "invalid_point_count_total",
      row = row_ids,
      column = "point_count",
      col_index = which(names(data) == "point_count"),
      value = as.character(df_primary_point_count$total_point_count[row_ids])
    )
  }
  }else {
    chunks <- NULL
  }
  
  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_point_counts"
  )
  
}
  
  
  