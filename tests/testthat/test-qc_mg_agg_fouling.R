# ---------------------------------------------------------------------------
# Fouling cover point totals.
#
# Grouped by panel and deployment period rather than by quadrat, with no taxon
# or method filter: both columns are required in this table, so the check always
# applies. This table never had a point-total check before.
# ---------------------------------------------------------------------------

.fouling_table <- "fouling-cover-v1"

# One panel of three taxa, summing to 100 by default.
.fouling_panel <- function(
  counts = c(40L, 30L, 30L),
  points_in_grid = 100L,
  panel_id = "P1",
  deployment_period = "3-month"
) {
  data.frame(
    sample_event_id = "USA-XYZ_SITE-001_2025",
    panel_id = panel_id,
    deployment_period = deployment_period,
    scientific_name = c("Botrylloides violaceus", "Bugula neritina", "bare"),
    point_count = counts,
    points_in_grid = points_in_grid,
    stringsAsFactors = FALSE
  )
}

test_that("a panel whose points sum correctly produces no issues", {
  result <- qc_check_aggregation(.fouling_panel(), .fouling_table)

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("a panel that does not sum to its grid fails once", {
  result <- qc_check_aggregation(
    .fouling_panel(counts = c(40L, 30L, 20L)),
    .fouling_table
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$severity, "fail")
  expect_equal(result$issue, "aggregate_mismatch")
  expect_equal(result$column, "point_count")
  expect_equal(result$value, "90")
  expect_true(is.na(result$row))
  expect_match(result$detail, "panel_id=P1")
  expect_match(result$detail, "expected points_in_grid = 100", fixed = TRUE)
})

test_that("the total is computed per panel", {
  both <- rbind(
    .fouling_panel(panel_id = "P1"),
    .fouling_panel(counts = c(40L, 30L, 20L), panel_id = "P2")
  )

  result <- qc_check_aggregation(both, .fouling_table)

  expect_equal(nrow(result), 1L)
  expect_match(result$detail, "panel_id=P2")
})

test_that("groups are distinguished by deployment period", {
  # The same panel scored after two deployments. Without the period in the key
  # these would be summed to 200 and both reported.
  both <- rbind(
    .fouling_panel(deployment_period = "3-month"),
    .fouling_panel(deployment_period = "12-month")
  )

  expect_equal(nrow(qc_check_aggregation(both, .fouling_table)), 0L)
})

test_that("backfilled zero-count rows leave a panel's total unchanged", {
  # utl_fouling_backfill_cover() adds absent taxa with point_count = 0.
  padded <- rbind(
    .fouling_panel(),
    transform(
      .fouling_panel(counts = c(0L, 0L, 0L)),
      scientific_name = c(
        "Ciona intestinalis",
        "Molgula manhattensis",
        "Watersipora"
      )
    )
  )

  expect_equal(nrow(qc_check_aggregation(padded, .fouling_table)), 0L)
})

test_that("NA point counts are summed as zero rather than passing silently", {
  with_na <- .fouling_panel(counts = c(40L, 30L, NA_integer_))

  result <- qc_check_aggregation(with_na, .fouling_table)

  expect_equal(nrow(result), 1L)
  expect_equal(result$value, "70")
})

test_that("a panel declaring no grid size is skipped", {
  no_total <- .fouling_panel(points_in_grid = NA_integer_)

  expect_equal(nrow(qc_check_aggregation(no_total, .fouling_table)), 0L)
})

test_that("a panel whose rows disagree on the grid size is reported", {
  disagrees <- .fouling_panel()
  disagrees$points_in_grid <- c(100L, 100L, 50L)

  result <- qc_check_aggregation(disagrees, .fouling_table)

  expect_equal(nrow(result), 1L)
  expect_equal(result$issue, "inconsistent_total")
})
