#' Create a survey summary table
#'
#'
#' lists the unique survey metadata combinations. Check for the existance of method/block 1 and 2.
#'
#' @param df
#'
#' @returns a list of unique date, depth, vis, and sitecode combinations with other dive-metadata variables and True-False statements for method-Block combinations
#' @export
#'
#' @examples
#' filepath <- "inst/extdata/test_rls_data_EPA_two_header_rows.xlsx"
#' df <- utl_rls_load_excel(filepath)
#' summary_df <- utl_rls_summarize_table(df)


utl_rls_sample_event_summary <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  missing_columns <- dplyr::setdiff(c("Diver","Buddy","Site No.","Site Name", "Latitude", "Longitude", "Date", "Depth", "vis","Direction","Time", "P-Qs", "Method","Block"), colnames(df))
  # expect_error()
  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

        # Define a function to check each method-block combination
        check_combination <- function(df, method_val, block_val) {
          any(df$Method == method_val & df$Block == block_val)
        }

        # Filtering dives where all four conditions are met
        summary_df <- df |>
          dplyr::group_by(Diver, Buddy, `Site No.`, `Site Name`, Latitude, Longitude, Date, vis, Direction, Time, `P-Qs`, Depth) |>
          dplyr::summarize(
            M1B1 = check_combination(pick(Method, Block), 1, 1),
            M1B2 = check_combination(pick(Method, Block), 1, 2),
            M2B1 = check_combination(pick(Method, Block), 2, 1),
            M2B2 = check_combination(pick(Method, Block), 2, 2)
          ) |>
          dplyr::mutate(sample_event_id = paste(`Site No.`, "RLS", Date, Depth, sep = "_")) |>
          dplyr::select(sample_event_id, everything()) |>
          dplyr::arrange(sample_event_id)

  }, error = function(e) {
    message(paste("Error Creating Summary Table", e$message))
  })


    return(summary_df)

}
