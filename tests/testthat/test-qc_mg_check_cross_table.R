# ---------------------------------------------------------------------------
# Cross-table relations: referential integrity between a pair of tables.
# ---------------------------------------------------------------------------

quadrats <- data.frame(
  site_name = "Site A",
  survey_date = as.Date("2024-06-01"),
  transect = 1,
  quadrat = c(1, 2, 3),
  stringsAsFactors = FALSE
)

shoots <- data.frame(
  site_name = "Site A",
  survey_date = as.Date("2024-06-01"),
  transect = 1,
  quadrat = c(1, 1, 2),
  shoot_length = c(30, 40, 50),
  stringsAsFactors = FALSE
)

xt_rule <- function(...) {
  base <- data.frame(
    rule_id = "quadrat_in_shoots",
    relation_type = "subset",
    left_table_id = "quadrat_v1",
    right_table_id = "shoots_v1",
    group_columns = "site_name|survey_date|transect",
    left_columns = "quadrat",
    right_columns = "quadrat",
    severity = "fail",
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (nm in names(overrides)) {
    base[[nm]] <- overrides[[nm]]
  }
  base
}

test_that("a subset relation flags left keys absent from the right table", {
  result <- qc_check_cross_table(quadrats, shoots, xt_rule())

  expect_qc_issues(result)
  expect_equal(result$row, 3L)
  expect_equal(result$value, "3")
  expect_equal(result$issue, "orphan_key")
  expect_equal(result$severity, "fail")
  expect_equal(result$table, "quadrat_v1")
  expect_equal(result$rule_id, "quadrat_in_shoots")
  expect_match(result$detail, "no matching row in 'shoots_v1'")
})

test_that("a satisfied relation produces no issues", {
  expect_equal(
    nrow(qc_check_cross_table(quadrats[1:2, ], shoots, xt_rule())),
    0L
  )
})

test_that("the relation is scoped by the grouping key", {
  # Quadrat 3 exists in the shoots table, but on a different transect, so it is
  # still an orphan for this sample event.
  other_transect <- shoots
  other_transect$transect <- 2
  other_transect$quadrat <- 3

  result <- qc_check_cross_table(
    quadrats,
    rbind(shoots, other_transect),
    xt_rule()
  )

  expect_equal(result$row, 3L)
})

test_that("equal_set reports unmatched rows in both directions", {
  partial <- data.frame(
    site_name = "Site A",
    survey_date = as.Date("2024-06-01"),
    transect = 1,
    quadrat = c(1, 9),
    shoot_length = c(30, 40),
    stringsAsFactors = FALSE
  )

  result <- qc_check_cross_table(
    quadrats,
    partial,
    xt_rule(relation_type = "equal_set")
  )

  orphans <- result[result$issue == "orphan_key", ]
  unmatched <- result[result$issue == "unmatched_key", ]

  expect_equal(orphans$row, c(2L, 3L))
  expect_equal(unique(orphans$table), "quadrat_v1")
  expect_equal(unmatched$row, 2L)
  expect_equal(unmatched$value, "9")
  expect_equal(unmatched$table, "shoots_v1")
})

test_that("subset ignores extra rows on the right", {
  # The mirror of the equal_set case: under `subset` a right-only key is fine.
  extra <- rbind(
    shoots,
    data.frame(
      site_name = "Site A",
      survey_date = as.Date("2024-06-01"),
      transect = 1,
      quadrat = 9,
      shoot_length = 1,
      stringsAsFactors = FALSE
    )
  )

  result <- qc_check_cross_table(quadrats[1:2, ], extra, xt_rule())
  expect_equal(nrow(result), 0L)
})

test_that("keys match across differing storage types", {
  # `quadrat` is TINYINT in some MarineGEO tables and INT in others, and a
  # CSV-read column can arrive as character. A key comparison must not care.
  as_char <- shoots
  as_char$quadrat <- as.character(as_char$quadrat)

  result <- qc_check_cross_table(quadrats, as_char, xt_rule())
  expect_equal(result$row, 3L)
})

test_that("multiple compared columns are paired positionally", {
  left <- data.frame(
    site_name = "Site A",
    transect = c(1, 1),
    quadrat = c(1, 2),
    stringsAsFactors = FALSE
  )
  right <- data.frame(
    site_name = "Site A",
    t_num = c(1),
    q_num = c(1),
    stringsAsFactors = FALSE
  )

  result <- qc_check_cross_table(
    left,
    right,
    xt_rule(
      group_columns = "site_name",
      left_columns = "transect|quadrat",
      right_columns = "t_num|q_num"
    )
  )

  expect_equal(result$row, 2L)
})

test_that("a relation with no grouping columns compares whole-table keys", {
  result <- qc_check_cross_table(
    quadrats,
    shoots,
    xt_rule(group_columns = NA_character_)
  )

  expect_equal(result$row, 3L)
})

# ---------------------------------------------------------------------------
# Conditions restrict which rows participate
# ---------------------------------------------------------------------------

test_that("a side condition restricts which rows must match", {
  conditions <- data.frame(
    condition_id = "surveyed",
    table_id = "quadrat_v1",
    column_name = "status",
    operator = "==",
    value = "surveyed",
    description = "only surveyed quadrats",
    stringsAsFactors = FALSE
  )
  flagged <- quadrats
  flagged$status <- c("surveyed", "surveyed", "not surveyed")

  # Quadrat 3 has no shoots, but it was never surveyed, so it is excluded.
  result <- qc_check_cross_table(
    flagged,
    shoots,
    xt_rule(left_condition_id = "surveyed"),
    conditions = conditions
  )

  expect_equal(nrow(result), 0L)
})

test_that("rows excluded by a condition do not shift the reported row numbers", {
  conditions <- data.frame(
    condition_id = "surveyed",
    table_id = "quadrat_v1",
    column_name = "status",
    operator = "==",
    value = "surveyed",
    stringsAsFactors = FALSE
  )
  flagged <- quadrats
  flagged$status <- c("not surveyed", "surveyed", "surveyed")
  flagged$quadrat <- c(1, 7, 8)

  result <- qc_check_cross_table(
    flagged,
    shoots,
    xt_rule(left_condition_id = "surveyed"),
    conditions = conditions
  )

  # Quadrats 7 and 8 are orphans and sit at rows 2 and 3 of the full table.
  expect_equal(result$row, c(2L, 3L))
  expect_equal(result$value, c("7", "8"))
})

test_that("a condition id is scoped to its own table", {
  # Both tables define "busy", but with different meanings. The left side must
  # see only its own definition.
  conditions <- data.frame(
    condition_id = c("busy", "busy"),
    table_id = c("quadrat_v1", "shoots_v1"),
    column_name = c("quadrat", "shoot_length"),
    operator = c("<=", ">"),
    value = c("2", "1000"),
    stringsAsFactors = FALSE
  )

  result <- qc_check_cross_table(
    quadrats,
    shoots,
    xt_rule(left_condition_id = "busy"),
    conditions = conditions
  )

  # Scoped correctly, the left side keeps quadrats 1-2, both of which match.
  # Were the shoots term applied too, `shoot_length` is absent from the left
  # table and the whole side would collapse to nothing.
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Malformed metadata
# ---------------------------------------------------------------------------

expect_unevaluable_xt <- function(result, pattern) {
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$issue, "unevaluable_rule")
  testthat::expect_equal(result$severity, "warn")
  testthat::expect_true(is.na(result$row))
  testthat::expect_match(result$detail, pattern)
}

test_that("an unknown relation_type is reported, not applied", {
  expect_warning(
    result <- qc_check_cross_table(
      quadrats,
      shoots,
      xt_rule(relation_type = "overlaps")
    ),
    "unknown relation_type"
  )
  expect_unevaluable_xt(result, "overlaps")
})

test_that("mismatched column counts are reported", {
  expect_warning(
    result <- qc_check_cross_table(
      quadrats,
      shoots,
      xt_rule(left_columns = "transect|quadrat")
    ),
    "same number of columns"
  )
  expect_unevaluable_xt(result, "same number of columns")
})

test_that("a column absent from either side is reported with its table", {
  expect_warning(
    result <- qc_check_cross_table(
      quadrats,
      shoots,
      xt_rule(right_columns = "not_a_column")
    ),
    "missing column"
  )
  expect_unevaluable_xt(result, "right table \\(shoots_v1\\): not_a_column")

  expect_warning(
    result <- qc_check_cross_table(
      quadrats,
      shoots,
      xt_rule(left_columns = "not_a_column")
    ),
    "missing column"
  )
  expect_unevaluable_xt(result, "left table \\(quadrat_v1\\): not_a_column")
})

test_that("a dangling condition id warns and leaves that side unfiltered", {
  expect_warning(
    result <- qc_check_cross_table(
      quadrats,
      shoots,
      xt_rule(left_condition_id = "nope"),
      conditions = NULL
    ),
    "no rows in `qc_conditions`"
  )
  # Unfiltered, so quadrat 3 is still an orphan -- the rule is not silently
  # widened into passing.
  expect_equal(result$row, 3L)
})

test_that("input validation rejects malformed arguments", {
  expect_error(qc_check_cross_table(42, shoots, xt_rule()), "data frames")
  expect_error(
    qc_check_cross_table(quadrats, shoots, rbind(xt_rule(), xt_rule())),
    "one-row data frame"
  )
  expect_error(
    qc_check_cross_table(quadrats, shoots, data.frame(a = 1)),
    "must have columns"
  )
})
