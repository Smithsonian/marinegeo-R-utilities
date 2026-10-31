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

# --- qc_aggregation_failures() ----------------------------------------------
#
# The tidy view the Table Wrangler app renders. It must stay distinguishable
# between "did not run" and "ran and passed": the app shows different messages
# for the two, and a NULL that read as a pass would hide a check that never ran.

test_that("failing groups come back as a tidy frame with the group key", {
  result <- qc_aggregation_failures(.agg_panel, "fouling-cover-v1")

  expect_length(result, 1L)
  expect_named(result, "sum(point_count)")

  failures <- result[["sum(point_count)"]]
  expect_s3_class(failures, "data.frame")
  expect_equal(nrow(failures), 1L)
  expect_equal(failures$panel_id, "P2")
  expect_equal(failures$observed, 90)
  expect_equal(failures$expected, 100)
  expect_equal(failures$issue, "aggregate_mismatch")
  expect_true(
    all(
      c("sample_event_id", "panel_id", "deployment_period") %in% names(failures)
    )
  )
})

test_that("the registry metadata rides along as attributes", {
  failures <- qc_aggregation_failures(.agg_panel, "fouling-cover-v1")[[1]]

  expect_equal(attr(failures, "label"), "sum(point_count)")
  expect_equal(attr(failures, "expected_column"), "points_in_grid")
  expect_equal(attr(failures, "column"), "point_count")
  expect_equal(attr(failures, "severity"), "fail")
})

test_that("a table whose groups all add up returns zero rows, not NULL", {
  clean <- .agg_panel
  clean$point_count <- c(60L, 40L, 60L, 40L)

  result <- qc_aggregation_failures(clean, "fouling-cover-v1")

  expect_length(result, 1L)
  expect_false(is.null(result[[1]]))
  expect_equal(nrow(result[[1]]), 0L)
})

test_that("an aggregation that cannot run returns NULL for that element", {
  no_total <- .agg_panel
  no_total$points_in_grid <- NULL

  result <- qc_aggregation_failures(no_total, "fouling-cover-v1")

  expect_length(result, 1L)
  expect_null(result[[1]])

  # Zero rows is also "did not run": there are no groups to aggregate.
  expect_null(qc_aggregation_failures(.agg_panel[0, ], "fouling-cover-v1")[[1]])
})

test_that("an unregistered table is a zero-length list, not an error", {
  # qc_check_aggregation() errors here on purpose; this function is called by
  # the app for whatever table happens to be loaded.
  expect_equal(
    qc_aggregation_failures(.agg_panel, "seagrass-biomass-monitoring-v1"),
    list()
  )
})

test_that("bad arguments are rejected", {
  expect_error(
    qc_aggregation_failures("not a frame", "fouling-cover-v1"),
    "must be a data frame"
  )
  expect_error(
    qc_aggregation_failures(.agg_panel, c("a", "b")),
    "single non-NA character string"
  )
})

test_that("the tidy result and the issue rows describe the same groups", {
  issues <- qc_check_aggregation(.agg_panel, "fouling-cover-v1")
  failures <- qc_aggregation_failures(.agg_panel, "fouling-cover-v1")[[1]]

  expect_equal(nrow(issues), nrow(failures))
  expect_equal(issues$value, as.character(failures$observed))
  expect_equal(issues$issue, failures$issue)
  expect_true(all(grepl(failures$panel_id, issues$detail, fixed = TRUE)))
})

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
