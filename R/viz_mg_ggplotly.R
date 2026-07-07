#' Convert a ggplot to an interactive plotly widget
#'
#' @description
#' Wraps [plotly::ggplotly()] and optionally controls which traces are visible
#' by default in the legend. Designed to work with any `ggplot` object produced
#' by MarineGEO visualisation functions such as [viz_mg_timeseries_annual()].
#'
#' @param plot A `ggplot` object to convert.
#' @param plotly_visible_traces Character vector or `NULL` (default). Names of
#'   traces that should be fully visible on initial render. All other traces are
#'   set to `"legendonly"` (hidden in the plot but togglable via the legend).
#'   Trace names correspond to the levels of the colour/group aesthetic in the
#'   original ggplot. When `NULL`, all traces are visible.
#'
#' @return A `plotly` htmlwidget object.
#'
#' @details
#' Trace names in a plotly object converted from ggplot2 match the values of
#' the grouping variable (e.g., species names, site codes). Use the exact
#' strings as they appear in the data column mapped to `color` or `group`.
#'
#' Setting `"legendonly"` visibility hides a trace from the plot while keeping
#' it in the legend so users can click to show it. This is useful when a chart
#' has many groups and only a subset is relevant by default.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   year    = factor(rep(2020:2022, each = 2)),
#'   cover   = c(10, 20, 15, 25, 12, 18),
#'   species = rep(c("Halodule wrightii", "Thalassia testudinum"), 3)
#' )
#'
#' p <- viz_mg_timeseries_annual(
#'   df             = df,
#'   y_var          = "cover",
#'   y_label        = "Percent cover",
#'   y_grouping_var = "species"
#' )
#'
#' \dontrun{
#' # All traces visible
#' viz_mg_ggplotly(p)
#'
#' # Only one species visible by default
#' viz_mg_ggplotly(p, plotly_visible_traces = "Halodule wrightii")
#' }
viz_mg_ggplotly <- function(plot, plotly_visible_traces = NULL) {
  # --- Input validation -------------------------------------------------------
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.")
  }

  if (!is.null(plotly_visible_traces)) {
    if (!is.character(plotly_visible_traces) ||
        length(plotly_visible_traces) < 1L ||
        anyNA(plotly_visible_traces)) {
      stop("`plotly_visible_traces` must be a non-empty character vector with no NA values.")
    }
  }

  # --- Convert and apply trace visibility ------------------------------------
  plot <- plotly::ggplotly(plot)

  if (!is.null(plotly_visible_traces)) {
    plot$x$data <- purrr::map(plot$x$data, function(trace) {
      if (!trace$name %in% plotly_visible_traces) {
        trace$visible <- "legendonly"
      }
      trace
    })
  }

  return(plot)
}
