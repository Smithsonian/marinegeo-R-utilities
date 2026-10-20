# ---------------------------------------------------------------------------
# Grouped aggregate rules.
#
# The behaviour-preservation side of this check lives in
# test-qc_mg_check_point_counts.R, which pins the rule this generalizes. These
# tests cover the parameters that rule could not previously vary.
# ---------------------------------------------------------------------------

agg_rule <- function(...) {
  base <- data.frame(
    rule_id = "r1",
    group_columns = "site_name|transect|quadrat",
    value_column = "point_count",
    aggregate_function = "sum",
    comparison = "==",
    target_column = "points_in_quadrat",
    target_value = NA_real_,
    tolerance = NA_real_,
    severity = "fail",
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (nm in names(overrides)) {
    base[[nm]] <- overrides[[nm]]
  }
  base
}

quadrat_df <- function(point_count, points_in_quadrat = 81, ...) {
  data.frame(
    site_name = "Site A",
    transect = 1,
    quadrat = 5,
    point_count = point_count,
    points_in_quadrat = points_in_quadrat,
    stringsAsFactors = FALSE,
    ...
  )
}

# ---------------------------------------------------------------------------
# The two ways of declaring the expected value
# ---------------------------------------------------------------------------

test_that("a group total matching target_column produces no issues", {
  result <- qc_check_aggregate(quadrat_df(c(40, 41)), agg_rule())

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
})

test_that("a group total missing target_column fails every row in the group", {
  result <- qc_check_aggregate(quadrat_df(c(40, 30)), agg_rule())

  expect_equal(result$row, 1:2)
  expect_equal(unique(result$severity), "fail")
  expect_equal(unique(result$rule_id), "r1")
  expect_equal(unique(result$issue), "aggregate_mismatch")
  expect_match(result$detail[1], "sum\\(point_count\\) = 70")
  expect_match(result$detail[1], "points_in_quadrat = 81")
})

test_that("target_value compares against a literal from metadata", {
  rule <- agg_rule(target_column = NA_character_, target_value = 100)

  expect_equal(nrow(qc_check_aggregate(quadrat_df(c(60, 40)), rule)), 0L)
  expect_equal(nrow(qc_check_aggregate(quadrat_df(c(60, 50)), rule)), 2L)
})

test_that("the declared severity is honoured", {
  result <- qc_check_aggregate(
    quadrat_df(c(40, 30)),
    agg_rule(severity = "warn")
  )

  expect_equal(unique(result$severity), "warn")
  expect_equal(attr(result, "status"), "warn")
})

# ---------------------------------------------------------------------------
# Grouping is a parameter
# ---------------------------------------------------------------------------

test_that("each group is evaluated independently", {
  df <- rbind(quadrat_df(c(40, 41)), quadrat_df(c(40, 30)))
  df$quadrat <- c(5, 5, 6, 6)

  result <- qc_check_aggregate(df, agg_rule())

  expect_equal(result$row, 3:4)
})

test_that("a differently shaped protocol works from the same engine", {
  # The fouling panel total: panel_id / points_in_grid instead of
  # transect / quadrat / points_in_quadrat. This is the case the hard-coded
  # predecessor could not express at all.
  fouling <- data.frame(
    site_name = "Site A",
    panel_id = c("P1", "P1", "P2", "P2"),
    point_count = c(10, 15, 12, 13),
    points_in_grid = 25,
    stringsAsFactors = FALSE
  )

  rule <- agg_rule(
    group_columns = "site_name|panel_id",
    target_column = "points_in_grid"
  )

  # P1 sums to 25 and passes; P2 sums to 25 as well.
  expect_equal(nrow(qc_check_aggregate(fouling, rule)), 0L)

  fouling$point_count[4] <- 1
  result <- qc_check_aggregate(fouling, rule)
  expect_equal(result$row, 3:4)
  expect_match(result$detail[1], "points_in_grid = 25")
})

# ---------------------------------------------------------------------------
# Aggregates and comparisons
# ---------------------------------------------------------------------------

test_that("n and n_distinct count rows rather than summing them", {
  df <- quadrat_df(c(1, 1, 2))

  expect_equal(
    nrow(qc_check_aggregate(
      df,
      agg_rule(
        aggregate_function = "n",
        target_column = NA_character_,
        target_value = 3
      )
    )),
    0L
  )
  expect_equal(
    nrow(qc_check_aggregate(
      df,
      agg_rule(
        aggregate_function = "n_distinct",
        target_column = NA_character_,
        target_value = 2
      )
    )),
    0L
  )
})

test_that("ordered comparisons are supported", {
  df <- quadrat_df(c(10, 10))
  rule <- function(cmp) {
    agg_rule(
      comparison = cmp,
      target_column = NA_character_,
      target_value = 20
    )
  }

  # The rule states the relation that must hold, so <= 20 passes and > 20 does
  # not.
  expect_equal(nrow(qc_check_aggregate(df, rule("<="))), 0L)
  expect_equal(nrow(qc_check_aggregate(df, rule(">"))), 2L)
})

test_that("tolerance admits a near miss that exact comparison would flag", {
  df <- quadrat_df(c(40.4, 40.4))
  exact <- agg_rule(target_column = NA_character_, target_value = 81)

  expect_equal(nrow(qc_check_aggregate(df, exact)), 2L)

  slack <- agg_rule(
    target_column = NA_character_,
    target_value = 81,
    tolerance = 0.5
  )
  expect_equal(nrow(qc_check_aggregate(df, slack)), 0L)
})

test_that("NA values are skipped rather than collapsing the group total", {
  result <- qc_check_aggregate(quadrat_df(c(81, NA_real_)), agg_rule())

  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Malformed metadata degrades to a reported, non-fatal skip
# ---------------------------------------------------------------------------

expect_unevaluable <- function(result, pattern) {
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$issue, "unevaluable_rule")
  testthat::expect_equal(result$severity, "warn")
  testthat::expect_true(is.na(result$row))
  testthat::expect_match(result$detail, pattern)
}

test_that("a missing group column is reported, not silently passed", {
  # The predecessor returned zero issues here, making "this rule could not run"
  # indistinguishable from "this data is clean".
  df <- quadrat_df(c(40, 30))
  df$transect <- NULL

  expect_warning(
    result <- qc_check_aggregate(df, agg_rule()),
    "missing column"
  )
  expect_unevaluable(result, "transect")
})

test_that("both target_column and target_value set is rejected", {
  expect_warning(
    result <- qc_check_aggregate(
      quadrat_df(c(40, 30)),
      agg_rule(target_value = 81)
    ),
    "both set"
  )
  expect_unevaluable(result, "exactly one is required")
})

test_that("neither target_column nor target_value set is rejected", {
  expect_warning(
    result <- qc_check_aggregate(
      quadrat_df(c(40, 30)),
      agg_rule(target_column = NA_character_)
    ),
    "neither"
  )
  expect_unevaluable(result, "exactly one is required")
})

test_that("an unknown aggregate or comparison is rejected", {
  expect_warning(
    result <- qc_check_aggregate(
      quadrat_df(c(40, 30)),
      agg_rule(aggregate_function = "median")
    ),
    "unknown aggregate_function"
  )
  expect_unevaluable(result, "median")

  expect_warning(
    result <- qc_check_aggregate(
      quadrat_df(c(40, 30)),
      agg_rule(comparison = "~=")
    ),
    "unknown comparison"
  )
  expect_unevaluable(result, "~=")
})

test_that("a non-numeric value column is rejected", {
  df <- quadrat_df(c(40, 30))
  df$point_count <- c("a", "b")

  expect_warning(
    result <- qc_check_aggregate(df, agg_rule()),
    "not numeric"
  )
  expect_unevaluable(result, "not numeric")
})

test_that("a target that varies within a group is rejected", {
  df <- quadrat_df(c(40, 30), points_in_quadrat = c(81, 100))

  expect_warning(
    result <- qc_check_aggregate(df, agg_rule()),
    "not constant within the group"
  )
  expect_unevaluable(result, "points_in_quadrat")
})

test_that("one malformed rule does not stop the others from running", {
  rules <- rbind(
    agg_rule(aggregate_function = "median"),
    agg_rule(rule_id = "r2")
  )

  expect_warning(
    result <- qc_check_aggregate(quadrat_df(c(40, 30)), rules),
    "unknown aggregate_function"
  )

  expect_true("unevaluable_rule" %in% result$issue)
  expect_true("aggregate_mismatch" %in% result$issue)
  expect_equal(sort(unique(result$rule_id)), c("r1", "r2"))
})

test_that("input validation rejects malformed arguments", {
  expect_error(qc_check_aggregate(42, agg_rule()), "must be a data frame")
  expect_error(
    qc_check_aggregate(quadrat_df(1), data.frame(a = 1)),
    "must have columns"
  )
})

# ---------------------------------------------------------------------------
# Conditions filter rows before aggregating
# ---------------------------------------------------------------------------

test_that("a condition restricts which rows enter the aggregate", {
  df <- data.frame(
    site_name = "Site A",
    transect = 1,
    quadrat = 5,
    cover_type = c("canopy taxa", "live_oyster", "cultch", "sediment"),
    point_count = c(50, 27, 27, 27),
    points_in_quadrat = 81,
    stringsAsFactors = FALSE
  )
  conditions <- data.frame(
    condition_id = "primary",
    table_id = "t",
    column_name = "cover_type",
    operator = "not_in",
    value = "canopy taxa",
    description = "primary layer only",
    stringsAsFactors = FALSE
  )

  # The three primary rows sum to 81; counting the canopy row would give 131.
  result <- .qc_apply_conditional(
    qc_check_aggregate,
    data = df,
    rules = agg_rule(condition_id = "primary"),
    conditions = conditions
  )

  expect_equal(nrow(result), 0L)
})

test_that("rows are reported at their position in the full table", {
  # The excluded row sits ahead of the offenders, so an un-remapped index would
  # report rows 1-3 -- flagging the row that was deliberately excluded and
  # missing a real one.
  df <- data.frame(
    site_name = "Site A",
    transect = 1,
    quadrat = 5,
    cover_type = c("canopy taxa", "live_oyster", "cultch", "sediment"),
    point_count = c(50, 10, 10, 10),
    points_in_quadrat = 81,
    stringsAsFactors = FALSE
  )
  conditions <- data.frame(
    condition_id = "primary",
    table_id = "t",
    column_name = "cover_type",
    operator = "not_in",
    value = "canopy taxa",
    stringsAsFactors = FALSE
  )

  result <- .qc_apply_conditional(
    qc_check_aggregate,
    data = df,
    rules = agg_rule(condition_id = "primary"),
    conditions = conditions
  )

  expect_equal(result$row, 2:4)
  # The rule keeps its own id; the condition is appended as context.
  expect_equal(unique(result$rule_id), "r1")
  expect_match(result$detail[1], "sum\\(point_count\\) = 30")
  expect_match(result$detail[1], "primary")
})

# ---------------------------------------------------------------------------
# Dispatch from metadata via qc_run()
# ---------------------------------------------------------------------------

agg_struct <- data.frame(
  table_id = "agg_v1",
  column_name = c(
    "site_name",
    "transect",
    "quadrat",
    "point_count",
    "points_in_quadrat"
  ),
  data_type = c("STRING", "INT", "INT", "DOUBLE", "DOUBLE"),
  stringsAsFactors = FALSE
)

local_agg_metadata <- function(aggregates, env = parent.frame()) {
  local_mocked_bindings(
    marinegeo_metadata = list(
      database_structure = agg_struct,
      categorical_values = data.frame(
        table_id = character(0),
        column_name = character(0),
        value = character(0),
        stringsAsFactors = FALSE
      ),
      aggregate_rules = aggregates
    ),
    .package = "marinegeo.utils",
    .env = env
  )
}

test_that("qc_run dispatches the aggregate check from metadata", {
  local_agg_metadata(cbind(table_id = "agg_v1", agg_rule()))

  result <- qc_run(quadrat_df(c(40, 30)), table_id = "agg_v1")

  expect_true("qc_check_aggregate" %in% attr(result, "checks_run"))
  expect_equal(attr(result, "status"), "fail")
  expect_equal(
    nrow(result[result$check == "qc_check_aggregate", ]),
    2L
  )
})

test_that("a table with no aggregate metadata skips the check entirely", {
  local_agg_metadata(cbind(table_id = "other_table", agg_rule()))

  result <- qc_run(quadrat_df(c(40, 30)), table_id = "agg_v1")

  expect_false("qc_check_aggregate" %in% attr(result, "checks_run"))
})

test_that("the aggregate check is table-scoped and skipped under partial", {
  local_agg_metadata(cbind(table_id = "agg_v1", agg_rule()))

  result <- qc_run(
    quadrat_df(c(40, 30))[, "point_count", drop = FALSE],
    table_id = "agg_v1",
    partial = TRUE
  )

  expect_true("qc_check_aggregate" %in% attr(result, "checks_skipped"))
  expect_false("qc_check_aggregate" %in% attr(result, "checks_run"))
})
