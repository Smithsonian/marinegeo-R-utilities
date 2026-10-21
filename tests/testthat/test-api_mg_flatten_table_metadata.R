# ---------------------------------------------------------------------------
# YAML -> flat tibble round trip
#
# The safety property of the CSV-to-YAML migration: flattening the authored
# YAML must reproduce the registry tibbles the QC engine already consumes, so
# all 11 qc_mg_check_*() functions and their fixtures keep working untouched.
#
# Fixtures are vendored (tests/testthat/fixtures/table-metadata/) rather than
# read from the sibling marinegeo-metadata checkout behind a skip_if_not().
# A test that silently skips is the same failure mode the migration exists to
# remove.
# ---------------------------------------------------------------------------

.flat_fixture <- function() {
  skip_if_not_installed("yaml")
  .mg_flatten_table_metadata(
    .mg_parse_table_metadata_dir(test_path("fixtures/table-metadata"))
  )
}

# readr attaches `spec` and `problems` to every table it reads, so the bundled
# tibbles are spec_tbl_df while the flattened ones are plain tibbles. Those
# attributes are not data, and comparing them is noise -- but they are also not
# covered by ignore_attr = "class", so they are stripped explicitly on both
# sides rather than tolerated.
.plain <- function(d) {
  d <- as.data.frame(d)
  attr(d, "spec") <- NULL
  attr(d, "problems") <- NULL
  rownames(d) <- NULL
  d
}

.sort_rows <- function(d, keys) {
  d <- .plain(d)
  ord <- do.call(order, lapply(keys, function(k) as.character(d[[k]])))
  d <- d[ord, , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---------------------------------------------------------------------------
# Structure: column names, row counts, per-column classes
# ---------------------------------------------------------------------------

test_that("flattened tables carry the bundled column names in order", {
  flat <- .flat_fixture()

  for (nm in names(.mg_flat_schema)) {
    expect_identical(
      colnames(flat[[nm]]),
      .mg_flat_schema[[nm]],
      info = nm
    )
    expect_identical(
      colnames(flat[[nm]]),
      colnames(marinegeo_metadata[[nm]]),
      info = nm
    )
  }
})

test_that("flattened tables carry the bundled per-column types", {
  flat <- .flat_fixture()
  cls <- function(d) vapply(d, function(x) class(x)[[1]], character(1))

  # Types agree exactly across all six tables. They did not before
  # R/sysdata.rda was rebuilt from dist/: the all-NA condition_id columns were
  # logical in the CSV-era bundle because readr inferred them that way. Types
  # are now declared in the dist/ manifest and applied on both paths, so there
  # is nothing left to reconcile.
  for (nm in names(.mg_flat_schema)) {
    expect_identical(cls(flat[[nm]]), cls(marinegeo_metadata[[nm]]), info = nm)
  }
})

test_that("uuid_identity is logical, as qc_mg_run's which() requires", {
  flat <- .flat_fixture()

  expect_true(is.logical(flat$database_structure$uuid_identity))
  # which() on a character vector errors; this is the column that would break
  # every table-scoped qc_run() if the YAML reader guessed the type.
  expect_silent(which(flat$database_structure$uuid_identity))
  expect_identical(
    sum(flat$database_structure$uuid_identity, na.rm = TRUE),
    sum(marinegeo_metadata$database_structure$uuid_identity, na.rm = TRUE)
  )
})

test_that("column_name is character, as qc_check_columns requires", {
  flat <- .flat_fixture()

  expect_true(is.character(flat$database_structure$column_name))
  # reef-life-survey has 28 numeric-looking column names ("2.5", "137.5", ...).
  # If any had stayed numeric, qc_check_columns() would abort outright.
  rls <- flat$database_structure
  rls <- rls[rls$table_id == "reef-life-survey-data-marinegeo-v1", ]
  expect_true(any(grepl("^[0-9.]+$", rls$column_name)))
  expect_true(is.character(rls$column_name))
})

# ---------------------------------------------------------------------------
# Values: exact row order where it matters, row sets where it does not
# ---------------------------------------------------------------------------

test_that("database_structure round-trips with row order intact", {
  flat <- .flat_fixture()

  # Byte-for-byte, unsorted. qc_mg_run.R feeds column_name to
  # qc_check_columns(), which emits a `wrong_order` warn -- a reordering here
  # would make every real submission report spurious order issues.
  expect_equal(
    .plain(flat$database_structure),
    .plain(marinegeo_metadata$database_structure)
  )
})

test_that("qc_conditions round-trips with ids and row order intact", {
  flat <- .flat_fixture()

  # Condition ids surface in user-facing messages via .qc_condition_detail(),
  # so `label:` in the YAML pins them rather than letting the synthesized
  # <table_id>.c<n> form leak into QC output.
  expect_equal(
    .plain(flat$qc_conditions),
    .plain(marinegeo_metadata$qc_conditions)
  )
  expect_identical(
    flat$qc_conditions$condition_id,
    c(
      "primary_points",
      "zostera_leaves",
      "flowering_zostera",
      "marine_seagrass",
      "scored_cover"
    )
  )
})

test_that("numeric_ranges round-trips as a row set", {
  flat <- .flat_fixture()
  keys <- c("table_id", "column_name", "condition_id")

  # Row order changes: ranges now nest under their column, and the
  # numeric-ranges CSVs grouped tables in a different order than
  # data-structure does. No (table, column, condition) key repeats, so
  # qc_check_numeric_ranges()'s first-match-wins never depends on the order.
  expect_equal(
    .sort_rows(flat$numeric_ranges, keys),
    .sort_rows(marinegeo_metadata$numeric_ranges, keys)
  )
})

test_that("the inert range rows survive the round trip", {
  flat <- .flat_fixture()

  # 12 rows carry no range_type and are dropped by .qc_registry_rules(). They
  # are preserved so the migration stays lossless; `range_type: ~` is what
  # keeps them expressible.
  expect_identical(
    sum(is.na(flat$numeric_ranges$range_type)),
    sum(is.na(marinegeo_metadata$numeric_ranges$range_type))
  )
})

test_that("aggregate_rules round-trips as a row set", {
  flat <- .flat_fixture()

  expect_equal(
    .sort_rows(flat$aggregate_rules, "rule_id"),
    .sort_rows(marinegeo_metadata$aggregate_rules, "rule_id")
  )
})

test_that("aggregate target_value is numeric NA, never an empty string", {
  flat <- .flat_fixture()
  agg <- flat$aggregate_rules

  # qc_mg_check_aggregate.R guards target_column with nzchar() but not
  # target_value, so an "" here would make has_val TRUE, trip the
  # both-are-set branch, and silently degrade the rule to a warning.
  expect_true(is.numeric(agg$target_value))
  expect_false(any(!is.na(agg$target_value) & agg$target_value == ""))
  expect_true(all(xor(!is.na(agg$target_column), !is.na(agg$target_value))))
})

test_that("cross_table_rules round-trips as a row set", {
  flat <- .flat_fixture()
  expect_equal(
    .sort_rows(flat$cross_table_rules, "rule_id"),
    .sort_rows(marinegeo_metadata$cross_table_rules, "rule_id")
  )
})

test_that("cross-table join columns stay pairwise aligned", {
  flat <- .flat_fixture()
  xt <- flat$cross_table_rules

  # `on: [{left:, right:}]` makes a length mismatch unauthorable, so the
  # runtime guard in qc_mg_check_cross_table.R is unreachable from metadata.
  for (i in seq_len(nrow(xt))) {
    expect_identical(
      length(.qc_split_list(xt$left_columns[[i]])),
      length(.qc_split_list(xt$right_columns[[i]])),
      info = xt$rule_id[[i]]
    )
  }
})

test_that("categorical_values round-trips as a row set", {
  flat <- .flat_fixture()
  keys <- c("table_id", "column_name", "value")

  expect_equal(
    .sort_rows(flat$categorical_values, keys),
    .sort_rows(marinegeo_metadata$categorical_values, keys)
  )
})

# ---------------------------------------------------------------------------
# Referential validation
# ---------------------------------------------------------------------------

test_that("the vendored metadata passes referential validation", {
  skip_if_not_installed("yaml")
  docs <- .mg_parse_table_metadata_dir(test_path("fixtures/table-metadata"))

  expect_true(.mg_validate_table_metadata(docs))
})
