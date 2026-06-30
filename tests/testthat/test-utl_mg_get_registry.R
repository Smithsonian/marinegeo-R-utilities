# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.partner_codes <- data.frame(
  partner_code = c("USA-MDA", "BLZ-CBC", "AUS-SYD"),
  name = c("Maryland", "Calabash Caye", "Sydney"),
  institution = c("SERC", "BCB", "UNSW"),
  country = c("USA", "Belize", "Australia"),
  type = c("observatory", "project", "observatory"),
  stringsAsFactors = FALSE
)

.site_codes <- data.frame(
  partner_code = c("USA-MDA", "USA-MDA", "BLZ-CBC"),
  site_code = c("RHO-001", "CHB-001", "CCN-001"),
  site_name = c("Rhode River", "Chesapeake Bay", "Calabash Caye North"),
  habitat = c("seagrass", "estuary", "coral reef"),
  latitude = c(38.88, 38.72, 17.21),
  longitude = c(-76.55, -76.40, -87.90),
  stringsAsFactors = FALSE
)

.observation_lookup <- data.frame(
  scientific_name = c("Zostera marina", "Halodule wrightii"),
  scientific_id = c("APHIA:374p", "APHIA:374q"),
  stringsAsFactors = FALSE
)

.taxonomic_lookup <- data.frame(
  id = c(1L, 2L, 3L),
  scientific_id = c("APHIA:51", "APHIA:374534", "APHIA:145792"),
  parent_id = c(NA_character_, "APHIA:51", "APHIA:51"),
  rank = c("Kingdom", "Species", "Species"),
  name = c("Animalia", "Zostera marina", "Posidonia oceanica"),
  stringsAsFactors = FALSE
)

.functional_group_lookup <- data.frame(
  scientific_id = c("FUNCTIONAL:1", "FUNCTIONAL:2"),
  parent_id = c(NA_character_, "FUNCTIONAL:1"),
  functional_group_name = c("Seagrasses", "Tropical seagrasses"),
  enroll_all_lower_ranks = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

.data_index <- data.frame(
  table_id = c("sav_cover_v1", "sav_density_v1"),
  protocol = c("seagrass", "seagrass"),
  table_name = c("SAV Percent Cover", "SAV Shoot Density"),
  stringsAsFactors = FALSE
)

.database_structure <- data.frame(
  protocol = c("seagrass", "seagrass", "seagrass", "seagrass"),
  table_id = c(
    "sav_cover_v1",
    "sav_cover_v1",
    "sav_cover_v1",
    "sav_density_v1"
  ),
  level = c("raw", "raw", "raw", "raw"),
  column_name = c("site_code", "site_name", "percent_cover", "shoot_density"),
  data_type = c("STRING", "STRING", "DOUBLE", "DOUBLE"),
  uuid_identity = c(FALSE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

.categorical_values <- data.frame(
  table_id = c("sav_cover_v1", "sav_cover_v1", "sav_density_v1"),
  column_name = c("habitat", "habitat", "habitat"),
  value = c("seagrass", "mixed", "seagrass"),
  stringsAsFactors = FALSE
)

.numeric_ranges <- data.frame(
  table_id = c("sav_cover_v1", "sav_cover_v1"),
  column_name = c("percent_cover", "shoot_density"),
  min_fail = c(0, NA_real_),
  max_fail = c(100, 500),
  min_warn = c(NA_real_, NA_real_),
  max_warn = c(80, 300),
  range_type = c("inclusive", "inclusive"),
  stringsAsFactors = FALSE
)

.mock_metadata <- list(
  partner_codes = .partner_codes,
  site_codes = .site_codes,
  observation_lookup = .observation_lookup,
  taxonomic_lookup = .taxonomic_lookup,
  functional_group_lookup = .functional_group_lookup,
  data_index = .data_index,
  database_structure = .database_structure,
  categorical_values = .categorical_values,
  numeric_ranges = .numeric_ranges
)

# ---------------------------------------------------------------------------
# Happy path — full table retrieval (original tables)
# ---------------------------------------------------------------------------

test_that("returns full partner_codes table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("partner_codes")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("partner_code", "name", "institution", "country", "type") %in%
      colnames(result)
  ))
  expect_equal(nrow(result), nrow(.partner_codes))
})

test_that("returns full site_codes table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("site_codes")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c(
      "partner_code",
      "site_code",
      "site_name",
      "habitat",
      "latitude",
      "longitude"
    ) %in%
      colnames(result)
  ))
  expect_equal(nrow(result), nrow(.site_codes))
})

test_that("returns full observation_lookup table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .mg_fetch_registry = function(url) .observation_lookup,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("observation_lookup")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("scientific_name", "scientific_id") %in% colnames(result)))
  expect_equal(nrow(result), nrow(.observation_lookup))
})

# ---------------------------------------------------------------------------
# Happy path — full table retrieval (new tables)
# ---------------------------------------------------------------------------

test_that("returns full taxonomic_lookup table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .mg_fetch_registry = function(url) .taxonomic_lookup,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("taxonomic_lookup")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("id", "scientific_id", "parent_id", "rank", "name") %in% colnames(result)
  ))
  expect_equal(nrow(result), nrow(.taxonomic_lookup))
})

test_that("returns full functional_group_lookup table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("functional_group_lookup")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c(
      "scientific_id",
      "parent_id",
      "functional_group_name",
      "enroll_all_lower_ranks"
    ) %in%
      colnames(result)
  ))
  expect_equal(nrow(result), nrow(.functional_group_lookup))
})

test_that("returns full data_index table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("data_index")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("table_id", "protocol", "table_name") %in% colnames(result)
  ))
  expect_equal(nrow(result), nrow(.data_index))
})

test_that("returns full database_structure table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("database_structure")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c(
      "protocol",
      "table_id",
      "level",
      "column_name",
      "data_type",
      "uuid_identity"
    ) %in%
      colnames(result)
  ))
  expect_equal(nrow(result), nrow(.database_structure))
})

test_that("returns full categorical_values table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("categorical_values")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("table_id", "column_name", "value") %in% colnames(result)))
  expect_equal(nrow(result), nrow(.categorical_values))
})

test_that("returns full numeric_ranges table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("numeric_ranges")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c(
      "table_id",
      "column_name",
      "min_fail",
      "max_fail",
      "min_warn",
      "max_warn",
      "range_type"
    ) %in%
      colnames(result)
  ))
  expect_equal(nrow(result), nrow(.numeric_ranges))
})

# ---------------------------------------------------------------------------
# Filtering — single value
# ---------------------------------------------------------------------------

test_that("single filter returns matching subset", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("site_codes", partner_code = "USA-MDA")

  expect_equal(nrow(result), 2L)
  expect_true(all(result$partner_code == "USA-MDA"))
})

test_that("filtering categorical_values by table_id returns matching rows", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry(
    "categorical_values",
    table_id = "sav_density_v1"
  )

  expect_equal(nrow(result), 1L)
  expect_true(all(result$table_id == "sav_density_v1"))
})

test_that("filtering numeric_ranges by table_id returns matching rows", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("numeric_ranges", table_id = "sav_cover_v1")

  expect_equal(nrow(result), 2L)
  expect_true(all(result$table_id == "sav_cover_v1"))
})

# ---------------------------------------------------------------------------
# Filtering — multi-value vector
# ---------------------------------------------------------------------------

test_that("multi-value filter returns union of matching rows", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry(
    "site_codes",
    partner_code = c("USA-MDA", "BLZ-CBC")
  )

  expect_equal(nrow(result), 3L)
  expect_true(all(result$partner_code %in% c("USA-MDA", "BLZ-CBC")))
})

# ---------------------------------------------------------------------------
# Filtering — multi-column AND logic
# ---------------------------------------------------------------------------

test_that("two filter args are combined with AND logic", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry(
    "site_codes",
    partner_code = "USA-MDA",
    habitat = "seagrass"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$site_name, "Rhode River")
})

# ---------------------------------------------------------------------------
# Error: invalid table name
# ---------------------------------------------------------------------------

test_that("stops with informative error for unrecognized table", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_get_registry("bad_table"),
    "not a recognized registry table"
  )
})

test_that("stops when table is not a single character string", {
  expect_error(
    utl_mg_get_registry(123),
    "`table` must be a single non-NA character string"
  )
})

# ---------------------------------------------------------------------------
# Error: unknown filter column
# ---------------------------------------------------------------------------

test_that("stops with informative error for unknown filter column", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_get_registry("site_codes", not_a_col = "USA-MDA"),
    "Unknown filter column"
  )
})

test_that("error for unknown filter column lists valid columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_get_registry("site_codes", not_a_col = "x"),
    "partner_code"
  )
})

# ---------------------------------------------------------------------------
# Empty result
# ---------------------------------------------------------------------------

test_that("returns 0-row data frame and emits message when filter matches nothing", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_get_registry("site_codes", partner_code = "ZZZ-XXX"),
    "No rows matched"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})
