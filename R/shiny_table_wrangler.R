#' Launch the MarineGEO Table Wrangler Shiny application
#'
#' @description
#' Opens the Table Wrangler Shiny application bundled with the `marinegeo.utils`
#' package. The app provides an interactive interface for inspecting, filtering,
#' and reshaping tabular ecological data without writing code.
#'
#' @details
#' The app is located in `inst/table-wrangler/` and is resolved at runtime via
#' [system.file()], so it works regardless of where the package is installed.
#' The function calls [shiny::runApp()], which blocks the R session until the
#' browser window is closed.
#'
#' This function is intended for interactive use only. Do not call it from
#' automated pipelines or Shiny modules, as it will block execution.
#'
#' @return Called for its side effect (launching a Shiny app). Returns the value
#'   of [shiny::runApp()] invisibly.
#'
#' @examples
#' \dontrun{
#' shiny_launch_table_wrangler()
#' }
#'
#' @export
shiny_launch_table_wrangler <- function() {
  app_dir <- system.file("table-wrangler", package = "marinegeo.utils")
  shiny::runApp(app_dir)
}
