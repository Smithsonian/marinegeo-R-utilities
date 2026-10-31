# ---------------------------------------------------------------------------
# Mock metadata for a single fake table. `cover` carries every kind of rule;
# `notes` carries none beyond its type.
#
# Rules are keyed on `column_id`, not `(table_id, column_name)`, so the mock
# needs `database_structure$column_id` to join through and a `column_ids`
# dictionary row per id. `column_lookups` is what binds `scientific_name` to
# the observation registry: without a row there, no registry QC runs.
# ---------------------------------------------------------------------------

var_db_struct <- data.frame(
  table_id = rep("test_table_v1", 4),
  column_name = c("site_name", "cover", "notes", "scientific_name"),
  column_id = c("site_name_marinegeo", "cover", "notes", "scientific_name"),
  data_type = c("STRING", "DOUBLE", "STRING", "STRING"),
  missing_values = c("fail", "warn", "allow", "allow"),
  uuid_identity = c(TRUE, NA, NA, NA),
  stringsAsFactors = FALSE
)

var_column_ids <- data.frame(
  column_id = c("site_name_marinegeo", "cover", "notes", "scientific_name"),
  label = c("Site name", "Cover", "Notes", "Scientific name"),
  unit = c(NA_character_, "percent", NA_character_, NA_character_),
  stringsAsFactors = FALSE
)

var_cat_vals <- data.frame(
  column_id = c("site_name_marinegeo", "site_name_marinegeo"),
  value = c("Site A", "Site B"),
  stringsAsFactors = FALSE
)

var_num_ranges <- data.frame(
  rule_id = "cover",
  column_id = "cover",
  scope_type = NA_character_,
  scope_value = NA_character_,
  range_type = "inclusive",
  max_fail = 100,
  min_fail = 0,
  max_warn = 80,
  min_warn = NA_real_,
  stringsAsFactors = FALSE
)

var_column_lookups <- data.frame(
  column_id = "scientific_name",
  registry_table = "observation_lookup",
  registry_column = "scientific_name",
  scope_type = NA_character_,
  scope_value = NA_character_,
  stringsAsFactors = FALSE
)

var_obs_lookup <- data.frame(
  scientific_name = c("Zostera marina", "Thalassia testudinum"),
  scientific_id = c("urn:a", "urn:b"),
  stringsAsFactors = FALSE
)

make_var_metadata <- function() {
  list(
    database_structure = var_db_struct,
    column_ids = var_column_ids,
    controlled_vocabularies = var_cat_vals,
    numeric_ranges = var_num_ranges,
    column_lookups = var_column_lookups,
    observation_lookup = var_obs_lookup
  )
}

local_var_metadata <- function(env = parent.frame()) {
  local_mocked_bindings(
    marinegeo_metadata = make_var_metadata(),
    .package = "marinegeo.utils",
    .env = env
  )
}

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("qc_check_variable returns a qc_issues table with run metadata", {
  local_var_metadata()
  result <- qc_check_variable(
    c("Site A", "Site B"),
    variable = "site_name",
    table_id = "test_table_v1"
  )

  expect_qc_issues(result)
  expect_equal(attr(result, "table_id"), "test_table_v1")
  expect_equal(attr(result, "n_rows"), 2L)
  expect_equal(attr(result, "columns"), "site_name")
  expect_equal(attr(result, "status"), "pass")
  expect_equal(nrow(result), 0L)
})

test_that("the three table-scoped checks are reported as skipped", {
  local_var_metadata()
  result <- qc_check_variable(
    "Site A",
    variable = "site_name",
    table_id = "test_table_v1"
  )

  expect_setequal(
    attr(result, "checks_skipped"),
    c(
      "qc_check_columns",
      "qc_check_row_uniqueness"
    )
  )
  expect_false(any(
    attr(result, "checks_skipped") %in% attr(result, "checks_run")
  ))
})

test_that("only checks with an applicable rule are recorded as run", {
  local_var_metadata()

  # `cover` has a type, a missing-value rule and a numeric range, but no
  # controlled vocabulary.
  cover <- qc_check_variable(
    c(20, 30),
    variable = "cover",
    table_id = "test_table_v1"
  )
  expect_true("qc_check_numeric_ranges" %in% attr(cover, "checks_run"))
  expect_false(
    "qc_check_categorical_values" %in% attr(cover, "checks_run")
  )

  # `site_name` has a controlled vocabulary but no numeric range.
  site <- qc_check_variable(
    "Site A",
    variable = "site_name",
    table_id = "test_table_v1"
  )
  expect_true("qc_check_categorical_values" %in% attr(site, "checks_run"))
  expect_false("qc_check_numeric_ranges" %in% attr(site, "checks_run"))
})

# ---------------------------------------------------------------------------
# Input shapes
# ---------------------------------------------------------------------------

test_that("a bare vector and a one-column data frame give identical results", {
  local_var_metadata()
  values <- c("Site A", "NOT_VALID")

  from_vector <- qc_check_variable(
    values,
    variable = "site_name",
    table_id = "test_table_v1"
  )
  from_frame <- qc_check_variable(
    data.frame(site_name = values, stringsAsFactors = FALSE),
    variable = "site_name",
    table_id = "test_table_v1"
  )

  expect_identical(from_vector, from_frame)
})

test_that("a multi-column data frame points the caller at qc_run(partial)", {
  local_var_metadata()
  expect_error(
    qc_check_variable(
      data.frame(site_name = "Site A", cover = 20),
      variable = "site_name",
      table_id = "test_table_v1"
    ),
    "partial = TRUE"
  )
})

test_that("a non-vector x stops with an informative error", {
  local_var_metadata()
  expect_error(
    qc_check_variable(
      list(1, 2),
      variable = "site_name",
      table_id = "test_table_v1"
    ),
    "must be an atomic vector or a one-column data frame"
  )
})

test_that("a zero-length vector passes with zero rows", {
  local_var_metadata()
  result <- qc_check_variable(
    character(0),
    variable = "site_name",
    table_id = "test_table_v1"
  )

  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(attr(result, "n_rows"), 0L)
  expect_equal(attr(result, "status"), "pass")
})

# ---------------------------------------------------------------------------
# Individual checks reached through the variable entry point
# ---------------------------------------------------------------------------

test_that("an invalid categorical value is a fail on the right cell", {
  local_var_metadata()
  result <- qc_check_variable(
    c("Site A", "NOT_VALID", "Site B"),
    variable = "site_name",
    table_id = "test_table_v1"
  )

  bad <- result[result$check == "qc_check_categorical_values", ]
  expect_equal(nrow(bad), 1L)
  expect_equal(bad$severity, "fail")
  expect_equal(bad$issue, "invalid_category")
  expect_equal(bad$row, 2L)
  expect_equal(bad$column, "site_name")
  expect_equal(bad$col_index, 1L)
  expect_equal(bad$value, "NOT_VALID")
})

test_that("NA under a fail rule fails and under a warn rule warns", {
  local_var_metadata()

  enforced <- qc_check_variable(
    c("Site A", NA),
    variable = "site_name",
    table_id = "test_table_v1"
  )
  miss <- enforced[enforced$check == "qc_check_missing_values", ]
  expect_equal(miss$severity, "fail")
  expect_equal(miss$issue, "missing_value")
  expect_equal(miss$row, 2L)

  warned <- qc_check_variable(
    c(20, NA),
    variable = "cover",
    table_id = "test_table_v1"
  )
  expect_equal(
    warned$severity[warned$check == "qc_check_missing_values"],
    "warn"
  )
})

test_that("out-of-range numbers fail or warn per threshold", {
  local_var_metadata()
  result <- qc_check_variable(
    c(20, 90, 150),
    variable = "cover",
    table_id = "test_table_v1"
  )

  ranges <- result[result$check == "qc_check_numeric_ranges", ]
  expect_equal(nrow(ranges), 2L)
  expect_equal(ranges$issue, rep("out_of_range", 2L))
  # 150 breaches max_fail; 90 only breaches max_warn.
  expect_equal(ranges$severity[ranges$row == 3L], "fail")
  expect_equal(ranges$severity[ranges$row == 2L], "warn")
  expect_equal(attr(result, "status"), "fail")
})

test_that("a type mismatch is reported as a column-level fail", {
  local_var_metadata()
  result <- qc_check_variable(
    c("20", "30"),
    variable = "cover",
    table_id = "test_table_v1"
  )

  types <- result[result$check == "qc_check_data_types", ]
  expect_equal(nrow(types), 1L)
  expect_equal(types$severity, "fail")
  expect_equal(types$issue, "type_mismatch")
  expect_equal(types$value, "character")
  expect_true(is.na(types$row))
})

test_that("scientific_name is routed to the lookup registry check", {
  local_var_metadata()
  result <- qc_check_variable(
    c("Zostera marina", "Not A Species"),
    variable = "scientific_name",
    table_id = "test_table_v1"
  )

  expect_true("qc_check_lookup_values" %in% attr(result, "checks_run"))
  bad <- result[result$check == "qc_check_lookup_values", ]
  expect_equal(nrow(bad), 1L)
  expect_equal(bad$issue, "unknown_lookup")
  expect_equal(bad$row, 2L)
  expect_equal(bad$value, "Not A Species")
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("an unknown variable names the helper that lists valid ones", {
  local_var_metadata()
  expect_error(
    qc_check_variable(1, variable = "nope", table_id = "test_table_v1"),
    "is not a known variable for table_id"
  )
})

test_that("an unknown table_id stops with an informative error", {
  local_var_metadata()
  expect_error(
    qc_check_variable(1, variable = "cover", table_id = "no_such_table"),
    "No metadata found for table_id"
  )
})

test_that("non-scalar variable or table_id stops with an informative error", {
  expect_error(
    qc_check_variable(1, variable = 5, table_id = "test_table_v1"),
    "`variable` must be a single non-NA character string"
  )
  expect_error(
    qc_check_variable(1, variable = c("a", "b"), table_id = "test_table_v1"),
    "`variable` must be a single non-NA character string"
  )
  expect_error(
    qc_check_variable(1, variable = "cover", table_id = NA_character_),
    "`table_id` must be a single non-NA character string"
  )
})

# ---------------------------------------------------------------------------
# Integration against the bundled registry (no mocks)
#
# Guarded on the table still existing, so a future sysdata rebuild degrades to
# a skip rather than a hard failure.
# ---------------------------------------------------------------------------

test_that("cover_code validates against the bundled seagrass metadata", {
  struct <- .mg_get_registry_table("database_structure")
  skip_if(
    is.null(struct) ||
      !"seagrass-cover-monitoring-v1" %in% struct$table_id,
    "seagrass-cover-monitoring-v1 is absent from the bundled metadata"
  )

  result <- qc_check_variable(
    c(0, 0.5, 9),
    variable = "cover_code",
    table_id = "seagrass-cover-monitoring-v1"
  )

  expect_qc_issues(result)
  expect_equal(attr(result, "columns"), "cover_code")
  expect_true(any(
    result$check == "qc_check_categorical_values" &
      result$issue == "invalid_category" &
      result$value == "9"
  ))
  # No numeric_ranges row exists for cover_code, so that check must not run.
  expect_false("qc_check_numeric_ranges" %in% attr(result, "checks_run"))
})

# ---------------------------------------------------------------------------
# Taxon-scoped range rules in column scope
#
# A scoped rule is evaluated against `scientific_name`, which a single column
# never supplies. The rule must be dropped and reported, never applied as
# though it were unconditional -- that would validate every row against one
# clade's bounds.
# ---------------------------------------------------------------------------

test_that("a scoped range rule is dropped, reported, and fires on no rows", {
  scoped <- data.frame(
    rule_id = "cover__taxon_within__genus_zostera",
    column_id = "cover",
    scope_type = "taxon_within",
    scope_value = "Genus:Zostera",
    range_type = "inclusive",
    max_fail = 1,
    min_fail = 0,
    max_warn = NA_real_,
    min_warn = NA_real_,
    stringsAsFactors = FALSE
  )
  meta <- make_var_metadata()
  meta$numeric_ranges <- scoped
  local_mocked_bindings(
    marinegeo_metadata = meta,
    .package = "marinegeo.utils"
  )

  result <- qc_check_variable(
    c(500, 900),
    variable = "cover",
    table_id = "test_table_v1"
  )

  expect_false("qc_check_numeric_ranges" %in% attr(result, "checks_run"))
  expect_equal(
    attr(result, "rules_skipped"),
    "cover__taxon_within__genus_zostera"
  )
  expect_equal(nrow(result[result$check == "qc_check_numeric_ranges", ]), 0L)
})
