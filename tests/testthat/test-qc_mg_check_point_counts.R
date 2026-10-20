# ---------------------------------------------------------------------------
# Safety net for qc_check_point_counts().
#
# Written before the check was generalized into qc_check_aggregate() so the
# behaviour being re-implemented is pinned down first: this check shipped
# without tests. Every assertion here describes behaviour that must survive the
# migration, so it is deliberately written against the public result rather
# than the internals.
#
# The row indices reported when `cover_type` filtering drops rows *ahead* of
# the offenders were wrong in the original implementation (they indexed the
# post-filter frame). That case is covered in
# test-qc_mg_check_aggregate.R, which asserts the corrected behaviour.
# ---------------------------------------------------------------------------

# One quadrat whose five primary cover types sum to points_in_quadrat.
clean_quadrat <- function(
  points_in_quadrat = 81,
  counts = c(9, 13, 20, 34, 5)
) {
  data.frame(
    site_name = "Site A",
    transect = 1,
    quadrat = 5,
    cover_type = c("live_oyster", "box_oyster", "cultch", "hash", "sediment"),
    point_count = counts,
    points_in_quadrat = points_in_quadrat,
    stringsAsFactors = FALSE
  )
}

test_that("a quadrat whose points sum correctly produces no issues", {
  result <- qc_check_point_counts(clean_quadrat())

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(attr(result, "status"), "pass")
})

test_that("a quadrat whose points do not sum to the total fails", {
  # 9 + 13 + 20 + 34 + 4 = 80, one short of 81.
  result <- qc_check_point_counts(clean_quadrat(counts = c(9, 13, 20, 34, 4)))

  expect_equal(nrow(result), 5L)
  expect_equal(unique(result$severity), "fail")
  expect_equal(unique(result$column), "point_count")
  expect_equal(sort(result$row), 1:5)
})

test_that("the total is computed per site/transect/quadrat group", {
  good <- clean_quadrat()
  bad <- clean_quadrat(counts = c(9, 13, 20, 34, 4))
  bad$quadrat <- 6

  result <- qc_check_point_counts(rbind(good, bad))

  # Only the second quadrat is short, so only its rows are reported.
  expect_equal(sort(result$row), 6:10)
})

test_that("groups are distinguished by transect as well as quadrat", {
  a <- clean_quadrat()
  b <- clean_quadrat(counts = c(9, 13, 20, 34, 4))
  b$transect <- 2

  result <- qc_check_point_counts(rbind(a, b))

  expect_equal(sort(result$row), 6:10)
})

test_that("canopy taxa are excluded from the primary point total", {
  # The canopy row is last, so dropping it does not shift the indices of the
  # rows that are reported -- the index handling itself is covered separately.
  with_canopy <- rbind(
    clean_quadrat(counts = c(9, 13, 20, 34, 5)),
    data.frame(
      site_name = "Site A",
      transect = 1,
      quadrat = 5,
      cover_type = "canopy taxa",
      point_count = 40,
      points_in_quadrat = 81,
      stringsAsFactors = FALSE
    )
  )

  # The five primary rows already sum to 81. Were the canopy row's 40 counted,
  # the total would be 121 and every row would be flagged.
  expect_equal(nrow(qc_check_point_counts(with_canopy)), 0L)
})

test_that("canopy taxa exclusion still allows a genuine shortfall through", {
  with_canopy <- rbind(
    clean_quadrat(counts = c(9, 13, 20, 34, 4)),
    data.frame(
      site_name = "Site A",
      transect = 1,
      quadrat = 5,
      cover_type = "canopy taxa",
      point_count = 1,
      points_in_quadrat = 81,
      stringsAsFactors = FALSE
    )
  )

  result <- qc_check_point_counts(with_canopy)

  # 80, not 81 -- and the canopy row's single point must not make up the
  # difference.
  expect_true(nrow(result) > 0L)
  expect_true(all(result$severity == "fail"))
})

test_that("a table without cover_type is summed in full", {
  no_cover_type <- clean_quadrat(counts = c(9, 13, 20, 34, 4))
  no_cover_type$cover_type <- NULL

  result <- qc_check_point_counts(no_cover_type)

  expect_equal(sort(result$row), 1:5)
})

test_that("NA point counts are treated as zero rather than poisoning the total", {
  with_na <- clean_quadrat()
  with_na$point_count[5] <- NA_real_

  result <- qc_check_point_counts(with_na)

  # The non-NA rows sum to 76, so the quadrat is short and is reported, rather
  # than the comparison collapsing to NA and silently passing.
  expect_true(nrow(result) > 0L)
})

test_that("a table missing the required columns yields no issues", {
  expect_equal(nrow(qc_check_point_counts(data.frame(a = 1))), 0L)

  no_total <- clean_quadrat()
  no_total$points_in_quadrat <- NULL
  expect_equal(nrow(qc_check_point_counts(no_total)), 0L)
})

test_that("a non-data-frame input is rejected", {
  expect_error(qc_check_point_counts(42), "must be a data frame")
})
