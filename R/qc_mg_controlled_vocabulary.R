#' Check for registered partner codes
#'
#' @param df
#'
#' @returns vector of row numbers for partner codes that are not in a controlled vocabulary
#' @export
#'
#' @examples
qc_mg_partner_codes <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  if(!"partner_code" %in% colnames(df)){
    message("Partner Code is not in the input data frame")
    return(NULL)
  } else {

    tryCatch({

      partner_codes <- marinegeo_resources$partner_codes |>
        dplyr::pull(partner_code)

      row_numbers <- df |>
        tibble::rowid_to_column() |>
        dplyr::filter(!partner_code %in% partner_codes)|>
        dplyr::pull(rowid)

      if(length(row_numbers) == 0){
        return(NULL)
      }else{
        return(row_numbers)
      }

    }, error = function(e) {
      message(paste("Error checking that partner code(s) are accepted:", e$message))
    })

  }
}

#' Check for registered site names
#'
#' @param df
#'
#' @returns vector of row numbers for site names that are not in a controlled vocabulary
#' @export
#'
#' @examples
qc_mg_site_names <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  if(!"site_name" %in% colnames(df)){
    message("Site name is not in the input data frame")
    return(NULL)
  } else {

    tryCatch({

      site_names <- marinegeo_resources$site_names |>
        dplyr::pull(site_name)

      row_numbers <- df |>
        tibble::rowid_to_column() |>
        dplyr::filter(!site_name %in% site_names)|>
        dplyr::pull(rowid)

      if(length(row_numbers) == 0){
        return(NULL)
      }else{
        return(row_numbers)
      }

    }, error = function(e) {
      message(paste("Error checking that site name(s) are accepted:", e$message))
    })

  }
}
