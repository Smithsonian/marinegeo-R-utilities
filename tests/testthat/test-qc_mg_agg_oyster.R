# ---------------------------------------------------------------------------
# Oyster substrate composition point totals.
#
# Behavioural assertions ported from the retired
# test-qc_mg_check_point_counts.R (git show
# 77b0e46^:tests/testthat/test-qc_mg_check_point_counts.R), adjusted to the
# one-issue-per-group result shape. The canopy-taxa exclusion is the part worth
# the most coverage: it is the reason the retired metadata-driven version needed
# a second rule table, and a regression there silently inflates every total.
# ---------------------------------------------------------------------------

.oyster_table <- "oyster-substrate-composition-monitoring-v1"

# The five primary cover types of one quadrat. `counts` defaults to a set that
# sums to exactly 81.
.oyster_quadrat <- function(
  counts = c(9L, 13L, 20L, 34L, 5L),
  points_in_quadrat = 81L,
  quadrat = 5L,
  transect = 1L
) {
  data.frame(
    sample_event_id = "USA-XYZ_SITE-001_2025",
    transect = transect,
    quadrat = quadrat,
    sample_collection_date = as.Date("2025-06-01"),
    cover_type = c(
      "live oyster",
      "box oyster",
      "cultch",
      "shell hash",
      "sediment"
    ),
    point_count = counts,
    points_in_quadrat = points_in_quadrat,
    stringsAsFactors = FALSE
  )
}

# One canopy row, which must never count toward the primary total.
.oyster_canopy <- function(point_count = 40L, points_in_quadrat = 81L) {
  data.frame(
    sample_event_id = "USA-XYZ_SITE-001_2025",
    transect = 1L,
    quadrat = 5L,
    sample_collection_date = as.Date("2025-06-01"),
    cover_type = "canopy taxa",
    point_count = point_count,
    points_in_quadrat = points_in_quadrat,
    stringsAsFactors = FALSE
  )
}

test_that("a quadrat whose points sum correctly produces no issues", {
  result <- qc_check_aggregation(.oyster_quadrat(), .oyster_table)

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("a quadrat that does not sum to its total fails once", {
  # 9 + 13 + 20 + 34 + 4 = 80, one short of 81.
  result <- qc_check_aggregation(
    .oyster_quadrat(counts = c(9L, 13L, 20L, 34L, 4L)),
    .oyster_table
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$severity, "fail")
  expect_equal(result$issue, "aggregate_mismatch")
  expect_equal(result$column, "point_count")
  expect_equal(result$value, "80")
  # The finding spans the group, so it belongs to no single row.
  expect_true(is.na(result$row))
  # It came from code, not from a rule table.
  expect_true(is.na(result$rule_id))
  expect_match(result$detail, "quadrat=5")
  expect_match(result$detail, "expected points_in_quadrat = 81", fixed = TRUE)
})

test_that("the total is computed per quadrat", {
  both <- rbind(
    .oyster_quadrat(quadrat = 5L),
    .oyster_quadrat(counts = c(9L, 13L, 20L, 34L, 4L), quadrat = 6L)
  )

  result <- qc_check_aggregation(both, .oyster_table)

  expect_equal(nrow(result), 1L)
  expect_match(result$detail, "quadrat=6")
})

test_that("groups are distinguished by transect as well as quadrat", {
  both <- rbind(
    .oyster_quadrat(transect = 1L),
    .oyster_quadrat(counts = c(9L, 13L, 20L, 34L, 4L), transect = 2L)
  )

  result <- qc_check_aggregation(both, .oyster_table)

  expect_equal(nrow(result), 1L)
  expect_match(result$detail, "transect=2")
})

test_that("groups are distinguished by collection date", {
  # Two visits to the same quadrat in one season. `sample_event_id` is
  # season-level, so without the date in the key these would be summed into one
  # total of 161 and both visits reported.
  second_visit <- .oyster_quadrat()
  second_visit$sample_collection_date <- as.Date("2025-08-15")

  result <- qc_check_aggregation(
    rbind(.oyster_quadrat(), second_visit),
    .oyster_table
  )

  expect_equal(nrow(result), 0L)
})

test_that("canopy taxa are excluded from the primary total", {
  # The five primary rows already sum to 81. Were the canopy row's 40 counted,
  # the total would be 121 and the quadrat would be flagged.
  with_canopy <- rbind(.oyster_quadrat(), .oyster_canopy(point_count = 40L))

  expect_equal(nrow(qc_check_aggregation(with_canopy, .oyster_table)), 0L)
})

test_that("excluding canopy taxa still lets a genuine shortfall through", {
  # 80, not 81 -- and the canopy row's single point must not make it up.
  with_canopy <- rbind(
    .oyster_quadrat(counts = c(9L, 13L, 20L, 34L, 4L)),
    .oyster_canopy(point_count = 1L)
  )

  result <- qc_check_aggregation(with_canopy, .oyster_table)

  expect_equal(nrow(result), 1L)
  expect_equal(result$value, "80")
})

test_that("NA point counts are summed as zero rather than passing silently", {
  with_na <- .oyster_quadrat(counts = c(9L, 13L, 20L, 34L, NA_integer_))

  result <- qc_check_aggregation(with_na, .oyster_table)

  # The non-NA rows sum to 76, so the quadrat is short and is reported, rather
  # than the comparison collapsing to NA and dropping out.
  expect_equal(nrow(result), 1L)
  expect_equal(result$value, "76")
})

test_that("a quadrat declaring no total is skipped, not failed", {
  no_total <- .oyster_quadrat(points_in_quadrat = NA_integer_)

  expect_equal(nrow(qc_check_aggregation(no_total, .oyster_table)), 0L)
})

test_that("a quadrat whose rows disagree on the total is reported", {
  disagrees <- .oyster_quadrat()
  disagrees$points_in_quadrat <- c(81L, 81L, 81L, 81L, 100L)

  result <- qc_check_aggregation(disagrees, .oyster_table)

  expect_equal(nrow(result), 1L)
  expect_equal(result$issue, "inconsistent_total")
  expect_equal(result$severity, "fail")
  # No single expected value exists, so the detail must not claim one.
  expect_match(result$detail, "is not constant within the group", fixed = TRUE)
  expect_false(grepl("expected points_in_quadrat =", result$detail))
})
