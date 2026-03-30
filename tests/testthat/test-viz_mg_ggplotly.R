# ---------------------------------------------------------------------------
# Shared fixture
# ---------------------------------------------------------------------------

.ggplotly_plot <- ggplot2::ggplot(
  data.frame(
    year    = factor(rep(2020:2022, each = 2)),
    cover   = c(10, 20, 15, 25, 12, 18),
    species = rep(c("Sp A", "Sp B"), 3),
    stringsAsFactors = FALSE
  ),
  ggplot2::aes(
    x     = year,
    y     = cover,
    color = species,
    group = species
  )
) +
  ggplot2::geom_line() +
  ggplot2::geom_point()

# ---------------------------------------------------------------------------
# Return value
# ---------------------------------------------------------------------------

test_that("returns a plotly object", {
  skip_if_not_installed("plotly")

  result <- viz_mg_ggplotly(.ggplotly_plot)

  expect_s3_class(result, "plotly")
})

# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("no plotly_visible_traces succeeds and all traces are visible", {
  skip_if_not_installed("plotly")

  result <- viz_mg_ggplotly(.ggplotly_plot)

  visibility <- vapply(result$x$data, function(t) {
    if (is.null(t$visible)) "visible" else as.character(t$visible)
  }, character(1L))

  expect_true(all(visibility == "visible"))
})

test_that("valid plotly_visible_traces succeeds", {
  skip_if_not_installed("plotly")

  expect_no_error(viz_mg_ggplotly(.ggplotly_plot, plotly_visible_traces = "Sp A"))
})

# ---------------------------------------------------------------------------
# Trace visibility
# ---------------------------------------------------------------------------

test_that("traces not in plotly_visible_traces are set to legendonly", {
  skip_if_not_installed("plotly")

  result <- viz_mg_ggplotly(.ggplotly_plot, plotly_visible_traces = "Sp A")

  trace_names      <- vapply(result$x$data, function(t) t$name, character(1L))
  trace_visibility <- vapply(result$x$data, function(t) {
    if (is.null(t$visible)) "visible" else as.character(t$visible)
  }, character(1L))

  expect_equal(trace_visibility[trace_names == "Sp B"], "legendonly")
})

test_that("traces in plotly_visible_traces remain visible", {
  skip_if_not_installed("plotly")

  result <- viz_mg_ggplotly(.ggplotly_plot, plotly_visible_traces = "Sp A")

  trace_names      <- vapply(result$x$data, function(t) t$name, character(1L))
  trace_visibility <- vapply(result$x$data, function(t) {
    if (is.null(t$visible)) "visible" else as.character(t$visible)
  }, character(1L))

  expect_equal(trace_visibility[trace_names == "Sp A"], "visible")
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-ggplot plot stops with informative error", {
  expect_error(
    viz_mg_ggplotly("not a plot"),
    "ggplot"
  )
})

test_that("non-ggplot list stops with informative error", {
  expect_error(
    viz_mg_ggplotly(list(x = 1)),
    "ggplot"
  )
})

test_that("non-character plotly_visible_traces stops with informative error", {
  expect_error(
    viz_mg_ggplotly(.ggplotly_plot, plotly_visible_traces = 1L),
    "character vector"
  )
})

test_that("NA in plotly_visible_traces stops with informative error", {
  expect_error(
    viz_mg_ggplotly(.ggplotly_plot, plotly_visible_traces = c("Sp A", NA_character_)),
    "NA"
  )
})

test_that("empty character vector for plotly_visible_traces stops with informative error", {
  expect_error(
    viz_mg_ggplotly(.ggplotly_plot, plotly_visible_traces = character(0)),
    "non-empty"
  )
})
