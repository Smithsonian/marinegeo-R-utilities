#' Launch the MarineGEO Table Wrangler Shiny application
#' @export
shiny_launch_table_wrangler <- function() {
  app_dir <- system.file("table-wrangler", package = "marinegeo.utils")
  shiny::runApp(app_dir)
}
