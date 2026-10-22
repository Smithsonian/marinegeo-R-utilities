# ---------------------------------------------------------------------------
# qc_run_dataset(): per-table checks plus the relations between them.
# ---------------------------------------------------------------------------

ds_struct <- rbind(
  data.frame(
    table_id = "quadrat_v1",
    column_name = c("site_name", "transect", "quadrat"),
    data_type = c("STRING", "DOUBLE", "DOUBLE"),
    stringsAsFactors = FALSE
  ),
  data.frame(
    table_id = "shoots_v1",
    column_name = c("site_name", "transect", "quadrat", "shoot_length"),
    data_type = c("STRING", "DOUBLE", "DOUBLE", "DOUBLE"),
    stringsAsFactors = FALSE
  )
)

ds_cross <- data.frame(
  rule_id = "quadrat_in_shoots",
  relation_type = "subset",
  left_table_id = "quadrat_v1",
  right_table_id = "shoots_v1",
  group_columns = "site_name|transect",
  left_columns = "quadrat",
  right_columns = "quadrat",
  severity = "fail",
  description = "Every surveyed quadrat must have shoot records",
  stringsAsFactors = FALSE
)

ds_quadrats <- data.frame(
  site_name = "Site A",
  transect = 1,
  quadrat = c(1, 2, 3),
  stringsAsFactors = FALSE
)

ds_shoots <- data.frame(
  site_name = "Site A",
  transect = 1,
  quadrat = c(1, 1, 2),
  shoot_length = c(30, 40, 50),
  stringsAsFactors = FALSE
)

local_ds_metadata <- function(cross = ds_cross, env = parent.frame()) {
  meta <- list(
    database_structure = ds_struct,
    categorical_values = data.frame(
      table_id = character(0),
      column_name = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    )
  )
  if (!is.null(cross)) {
    meta$cross_table_rules <- cross
  }
  local_mocked_bindings(
    marinegeo_metadata = meta,
    .package = "marinegeo.utils",
    .env = env
  )
}

both_tables <- function() {
  list(quadrat_v1 = ds_quadrats, shoots_v1 = ds_shoots)
}

# ---------------------------------------------------------------------------
# Cross-table dispatch
# ---------------------------------------------------------------------------

test_that("a relation defined in metadata is found and attributed", {
  local_ds_metadata()

  result <- qc_run_dataset(both_tables())

  expect_qc_issues(result)
  orphans <- result[result$check == "qc_check_cross_table", ]
  expect_equal(nrow(orphans), 1L)
  expect_equal(orphans$row, 3L)
  expect_equal(orphans$table, "quadrat_v1")
  expect_equal(orphans$issue, "orphan_key")
  expect_equal(attr(result, "status"), "fail")
  expect_true("qc_check_cross_table" %in% attr(result, "checks_run"))
})

test_that("a satisfied dataset is clean", {
  local_ds_metadata()

  result <- qc_run_dataset(list(
    quadrat_v1 = ds_quadrats[1:2, ],
    shoots_v1 = ds_shoots
  ))

  expect_equal(nrow(result), 0L)
  expect_equal(attr(result, "status"), "pass")
})

test_that("per-table issues are tagged with the table they came from", {
  local_ds_metadata(cross = NULL)

  # Drop a declared column from the shoots table only.
  broken <- ds_shoots[, c("site_name", "transect", "quadrat")]

  result <- qc_run_dataset(list(
    quadrat_v1 = ds_quadrats[1:2, ],
    shoots_v1 = broken
  ))

  columns_issues <- result[result$check == "qc_check_columns", ]
  expect_true(nrow(columns_issues) > 0L)
  expect_equal(unique(columns_issues$table), "shoots_v1")
})

test_that("a relation whose counterpart is absent is skipped, not failed", {
  local_ds_metadata()

  result <- qc_run_dataset(list(quadrat_v1 = ds_quadrats))

  expect_equal(nrow(result[result$check == "qc_check_cross_table", ]), 0L)
  expect_true("qc_check_cross_table" %in% attr(result, "checks_skipped"))
  expect_false("qc_check_cross_table" %in% attr(result, "checks_run"))
})

test_that("no cross-table metadata means the check is neither run nor skipped", {
  local_ds_metadata(cross = NULL)

  result <- qc_run_dataset(both_tables())

  expect_false("qc_check_cross_table" %in% attr(result, "checks_run"))
  expect_false("qc_check_cross_table" %in% attr(result, "checks_skipped"))
})

test_that("a relation between two other tables is ignored entirely", {
  elsewhere <- ds_cross
  elsewhere$left_table_id <- "other_a"
  elsewhere$right_table_id <- "other_b"
  local_ds_metadata(cross = elsewhere)

  result <- qc_run_dataset(both_tables())

  expect_false("qc_check_cross_table" %in% attr(result, "checks_skipped"))
})

# ---------------------------------------------------------------------------
# Run metadata
# ---------------------------------------------------------------------------

test_that("run attributes describe the whole dataset", {
  local_ds_metadata()

  result <- qc_run_dataset(both_tables())

  expect_equal(attr(result, "table_id"), c("quadrat_v1", "shoots_v1"))
  expect_equal(attr(result, "n_rows"), nrow(ds_quadrats) + nrow(ds_shoots))
  expect_equal(
    attr(result, "columns"),
    list(
      quadrat_v1 = colnames(ds_quadrats),
      shoots_v1 = colnames(ds_shoots)
    )
  )
  # The per-table checks still appear alongside the cross-table one.
  expect_true("qc_check_columns" %in% attr(result, "checks_run"))
})

test_that("file paths are accepted the way qc_run accepts them", {
  local_ds_metadata()

  csv <- tempfile(fileext = ".csv")
  # withr::defer(), not on.exit(): a bare on.exit() replaces the exit handlers
  # already registered on this frame, including the one local_mocked_bindings()
  # uses to restore `marinegeo_metadata` -- which would leak the mock into every
  # later test file.
  withr::defer(unlink(csv))
  readr::write_csv(ds_shoots, csv)

  from_path <- qc_run_dataset(list(quadrat_v1 = ds_quadrats, shoots_v1 = csv))
  from_df <- qc_run_dataset(both_tables())

  expect_equal(attr(from_path, "status"), attr(from_df, "status"))
  expect_equal(nrow(from_path), nrow(from_df))
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("a bare data frame is rejected with a pointer to qc_run", {
  expect_error(qc_run_dataset(ds_quadrats), "use `qc_run\\(\\)`")
})

test_that("an unnamed or partially named list is rejected", {
  expect_error(
    qc_run_dataset(list(ds_quadrats, ds_shoots)),
    "must be named with its MarineGEO table_id"
  )
  expect_error(
    qc_run_dataset(list(quadrat_v1 = ds_quadrats, ds_shoots)),
    "must be named with its MarineGEO table_id"
  )
})

test_that("duplicate table_ids are rejected", {
  expect_error(
    qc_run_dataset(list(quadrat_v1 = ds_quadrats, quadrat_v1 = ds_quadrats)),
    "duplicate table_id"
  )
})

test_that("an empty list is rejected", {
  expect_error(qc_run_dataset(list()), "at least one table")
})

# ---------------------------------------------------------------------------
# Console report
# ---------------------------------------------------------------------------

test_that("qc_run_dataset reports each table and then the dataset", {
  local_ds_metadata()

  messages <- capture_messages(qc_run_dataset(both_tables()))

  # One block per table, then one for the dataset as a whole.
  expect_equal(sum(grepl("^QC: quadrat_v1 ", messages)), 1L)
  expect_equal(sum(grepl("^QC: shoots_v1 ", messages)), 1L)
  expect_equal(sum(grepl("^QC: quadrat_v1, shoots_v1 ", messages)), 1L)
  # The cross-table check only exists at the dataset level.
  combined <- messages[grepl("^QC: quadrat_v1, shoots_v1 ", messages)]
  expect_match(combined, "FAIL  qc_check_cross_table \\(1 issue\\)")
  expect_match(combined, "Status: FAIL")
})

test_that("qc_run_dataset is silent with verbose = FALSE", {
  local_ds_metadata()

  quiet <- expect_no_message(qc_run_dataset(both_tables(), verbose = FALSE))
  loud <- suppressMessages(qc_run_dataset(both_tables()))

  expect_equal(quiet, loud)
  expect_identical(attributes(quiet), attributes(loud))
})

test_that("non-logical verbose stops with an informative error", {
  expect_error(
    qc_run_dataset(both_tables(), verbose = "yes"),
    "`verbose` must be a single `TRUE` or `FALSE`"
  )
})
