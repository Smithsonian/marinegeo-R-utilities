# ---------------------------------------------------------------------------
# Seagrass cover point totals.
#
# The method filter is the point of this file. `point_count` and
# `points_in_quadrat` are both optional in this table, so a partner recording
# cover by Braun-Blanquet leaves them empty -- and an all-NA group sums to 0
# against an NA total, which the comparison drops. That reads as a pass having
# checked nothing, so the restriction to `cover_method == "point count"` is
# asserted here against both a fixture and the bundled example dataset.
# ---------------------------------------------------------------------------

.seagrass_table <- "seagrass-cover-monitoring-v1"

# One point-count quadrat of three taxa, summing to 100 by default.
.seagrass_quadrat <- function(
  counts = c(50L, 30L, 20L),
  points_in_quadrat = 100L,
  cover_method = "point count",
  quadrat = 1L
) {
  data.frame(
    sample_event_id = "USA-XYZ_SITE-001_2025",
    transect = 1L,
    quadrat = quadrat,
    sample_collection_date = as.Date("2025-06-01"),
    scientific_name = c("Zostera marina", "Halodule wrightii", "bare"),
    cover_method = cover_method,
    point_count = counts,
    points_in_quadrat = points_in_quadrat,
    stringsAsFactors = FALSE
  )
}

test_that("a point-count quadrat that sums correctly produces no issues", {
  result <- qc_check_aggregation(.seagrass_quadrat(), .seagrass_table)

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("a point-count quadrat that does not sum to its total fails", {
  result <- qc_check_aggregation(
    .seagrass_quadrat(counts = c(50L, 30L, 10L)),
    .seagrass_table
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$severity, "fail")
  expect_equal(result$issue, "aggregate_mismatch")
  expect_equal(result$value, "90")
  expect_match(result$detail, "expected points_in_quadrat = 100", fixed = TRUE)
})

test_that("quadrats recorded by another method are not checked at all", {
  # These counts sum to 90 against a declared 100. Were the method ignored, the
  # quadrat would be flagged.
  braun <- .seagrass_quadrat(
    counts = c(50L, 30L, 10L),
    cover_method = "Braun-Blanquet"
  )

  expect_equal(nrow(qc_check_aggregation(braun, .seagrass_table)), 0L)
})

test_that("a mixed-method table checks only its point-count quadrats", {
  mixed <- rbind(
    .seagrass_quadrat(counts = c(50L, 30L, 10L), quadrat = 1L),
    .seagrass_quadrat(
      counts = c(NA_integer_, NA_integer_, NA_integer_),
      points_in_quadrat = NA_integer_,
      cover_method = "percent bins",
      quadrat = 2L
    )
  )

  result <- qc_check_aggregation(mixed, .seagrass_table)

  expect_equal(nrow(result), 1L)
  expect_match(result$detail, "quadrat=1")
})

test_that("NA point counts are summed as zero rather than passing silently", {
  with_na <- .seagrass_quadrat(counts = c(50L, 30L, NA_integer_))

  result <- qc_check_aggregation(with_na, .seagrass_table)

  expect_equal(nrow(result), 1L)
  expect_equal(result$value, "80")
})

test_that("a point-count quadrat declaring no total is skipped", {
  no_total <- .seagrass_quadrat(points_in_quadrat = NA_integer_)

  expect_equal(nrow(qc_check_aggregation(no_total, .seagrass_table)), 0L)
})

test_that("a quadrat whose rows disagree on the total is reported", {
  disagrees <- .seagrass_quadrat()
  disagrees$points_in_quadrat <- c(100L, 100L, 81L)

  result <- qc_check_aggregation(disagrees, .seagrass_table)

  expect_equal(nrow(result), 1L)
  expect_equal(result$issue, "inconsistent_total")
})

test_that("the bundled example dataset reports nothing", {
  # All 1,321 rows record cover by Braun-Blanquet or percent bins, with
  # point_count and points_in_quadrat NA throughout. This must pass via the
  # method filter rather than by the NA comparison being dropped -- the
  # preceding tests are what distinguish the two.
  result <- qc_check_aggregation(seagrass_cover_example, .seagrass_table)

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})
