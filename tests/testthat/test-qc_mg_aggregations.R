# ---------------------------------------------------------------------------
# The aggregation registry, the shared wrapper, and qc_check_aggregation()'s
# own contract. Per-table behaviour lives in test-qc_mg_agg_*.R.
#
# The registry consistency tests at the bottom are the important ones: because
# a spec names its columns as strings, a typo or a column rename in the metadata
# repo would turn a check into a silent no-op rather than an error.
# ---------------------------------------------------------------------------

.agg_panel <- data.frame(
  sample_event_id = "USA-XYZ_SITE-001_2025",
  panel_id = c("P1", "P1", "P2", "P2"),
  deployment_period = "3-month",
  point_count = c(60L, 40L, 50L, 40L),
  points_in_grid = 100L,
  stringsAsFactors = FALSE
)

# --- qc_check_aggregation() contract ---------------------------------------

test_that("the result is a well-formed qc_issues table", {
  result <- qc_check_aggregation(.agg_panel, "fouling-cover-v1")

  expect_qc_issues(result)
  expect_equal(attr(result, "checks_run"), "qc_check_aggregation")
  expect_equal(attr(result, "n_rows"), 4L)
  expect_equal(unique(result$check), "qc_check_aggregation")
})

test_that("a clean result still records that the check ran", {
  clean <- .agg_panel
  clean$point_count <- c(60L, 40L, 60L, 40L)

  result <- qc_check_aggregation(clean, "fouling-cover-v1")

  expect_equal(nrow(result), 0L)
  expect_equal(attr(result, "checks_run"), "qc_check_aggregation")
  expect_equal(attr(result, "n_rows"), 4L)
})

test_that("col_index points at the aggregated column", {
  result <- qc_check_aggregation(.agg_panel, "fouling-cover-v1")

  expect_equal(
    result$col_index,
    which(colnames(.agg_panel) == "point_count")
  )
})

test_that("the detail string names the group, the observed and the expected", {
  result <- qc_check_aggregation(.agg_panel, "fouling-cover-v1")

  expect_equal(
    result$detail,
    paste0(
      "sample_event_id=USA-XYZ_SITE-001_2025, panel_id=P2, ",
      "deployment_period=3-month: sum(point_count) = 90, ",
      "expected points_in_grid = 100"
    )
  )
})

test_that("a zero-row table yields no issues", {
  result <- qc_check_aggregation(.agg_panel[0, ], "fouling-cover-v1")

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
})

test_that("a table missing a required column yields no issues", {
  # Absent columns are qc_check_columns()'s to report; the aggregation simply
  # does not apply.
  without_total <- .agg_panel
  without_total$points_in_grid <- NULL

  expect_equal(
    nrow(qc_check_aggregation(without_total, "fouling-cover-v1")),
    0L
  )
  expect_equal(
    nrow(qc_check_aggregation(data.frame(a = 1), "fouling-cover-v1")),
    0L
  )
})

test_that("an unregistered table_id is an error", {
  # Silently passing would be indistinguishable from a genuine clean run.
  expect_error(
    qc_check_aggregation(.agg_panel, "fouling-biomass-v1"),
    "No aggregation check is defined"
  )
})

test_that("invalid arguments are rejected", {
  expect_error(qc_check_aggregation(42, "fouling-cover-v1"), "data frame")
  expect_error(
    qc_check_aggregation(.agg_panel, c("a", "b")),
    "single non-NA character string"
  )
  expect_error(
    qc_check_aggregation(.agg_panel, NA_character_),
    "single non-NA character string"
  )
})

# --- .qc_has_aggregations() -------------------------------------------------

test_that(".qc_has_aggregations() recognizes exactly the registered tables", {
  expect_true(.qc_has_aggregations("fouling-cover-v1"))
  expect_true(
    .qc_has_aggregations("oyster-substrate-composition-monitoring-v1")
  )
  expect_true(.qc_has_aggregations("seagrass-cover-monitoring-v1"))

  expect_false(.qc_has_aggregations("fouling-biomass-v1"))
  expect_false(.qc_has_aggregations(NA_character_))
  expect_false(.qc_has_aggregations(character(0)))
})

# --- .qc_agg_declared() -----------------------------------------------------

test_that(".qc_agg_declared() returns the declared value or NA, never -Inf", {
  expect_equal(.qc_agg_declared(c(81L, 81L, 81L)), 81)
  expect_equal(.qc_agg_declared(c(NA, 81L)), 81)
  # max(na.rm = TRUE) would warn and return -Inf here.
  expect_true(is.na(.qc_agg_declared(c(NA_integer_, NA_integer_))))
  expect_true(is.na(.qc_agg_declared(integer(0))))
  expect_silent(.qc_agg_declared(c(NA_integer_, NA_integer_)))
})

# --- registry consistency ---------------------------------------------------

test_that("every registry spec is well-formed", {
  specs <- .qc_aggregations()

  expect_gt(length(specs), 0L)
  expect_true(all(nzchar(names(specs))))

  for (table_id in names(specs)) {
    spec <- specs[[table_id]]
    expect_true(is.function(spec$fn), info = table_id)
    expect_type(spec$requires, "character")
    expect_gt(length(spec$requires), 0L)

    for (field in c(
      "column",
      "expected_column",
      "label",
      "issue",
      "severity"
    )) {
      expect_true(
        is.character(spec[[field]]) && length(spec[[field]]) == 1L,
        info = paste(table_id, field)
      )
    }

    expect_true(spec$severity %in% c("fail", "warn"), info = table_id)

    # A `column` or `expected_column` outside `requires` would not be checked
    # for presence, so the check would run against a column that may be absent.
    expect_true(spec$column %in% spec$requires, info = table_id)
    expect_true(spec$expected_column %in% spec$requires, info = table_id)
  }
})

test_that("every registered table and column exists in the metadata", {
  # This is the guard against a silent no-op: a renamed column in
  # marinegeo-metadata leaves `requires` unsatisfiable, and an unsatisfied
  # `requires` returns zero issues rather than erroring. Runs offline against
  # the bundled snapshot.
  struct <- .mg_get_registry_table("database_structure")
  specs <- .qc_aggregations()

  for (table_id in names(specs)) {
    cols <- struct$column_name[struct$table_id == table_id]
    expect_gt(length(cols), 0L)

    missing <- setdiff(specs[[table_id]]$requires, cols)
    expect_equal(
      missing,
      character(0),
      info = paste0(
        table_id,
        " declares column(s) absent from database_structure: ",
        paste(missing, collapse = ", ")
      )
    )
  }
})
