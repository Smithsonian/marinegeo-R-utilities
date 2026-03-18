# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.partner_codes <- data.frame(
  partner_code = c("USA-MDA", "BLZ-CBC", "AUS-SYD"),
  name         = c("Maryland", "Calabash Caye", "Sydney"),
  institution  = c("SERC", "BCB", "UNSW"),
  country      = c("USA", "Belize", "Australia"),
  type         = c("observatory", "project", "observatory"),
  stringsAsFactors = FALSE
)

.site_names <- data.frame(
  partner_code = c("USA-MDA", "USA-MDA", "BLZ-CBC"),
  site_name    = c("Rhode River", "Chesapeake Bay", "Calabash Caye North"),
  habitat      = c("seagrass", "estuary", "coral reef"),
  latitude     = c(38.88, 38.72, 17.21),
  longitude    = c(-76.55, -76.40, -87.90),
  stringsAsFactors = FALSE
)

.observation_lookup <- data.frame(
  scientific_name = c("Zostera marina", "Halodule wrightii"),
  scientific_id   = c("APHIA:374p", "APHIA:374q"),
  stringsAsFactors = FALSE
)

.mock_metadata <- list(
  partner_codes      = .partner_codes,
  site_names         = .site_names,
  observation_lookup = .observation_lookup
)

# ---------------------------------------------------------------------------
# Happy path — full table retrieval
# ---------------------------------------------------------------------------

test_that("returns full partner_codes table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("partner_codes")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("partner_code", "name", "institution", "country", "type") %in% colnames(result)))
  expect_equal(nrow(result), nrow(.partner_codes))
})

test_that("returns full site_names table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("site_names")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("partner_code", "site_name", "habitat", "latitude", "longitude") %in% colnames(result)))
  expect_equal(nrow(result), nrow(.site_names))
})

test_that("returns full observation_lookup table with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("observation_lookup")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("scientific_name", "scientific_id") %in% colnames(result)))
  expect_equal(nrow(result), nrow(.observation_lookup))
})

# ---------------------------------------------------------------------------
# Filtering — single value
# ---------------------------------------------------------------------------

test_that("single filter returns matching subset", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("site_names", partner_code = "USA-MDA")

  expect_equal(nrow(result), 2L)
  expect_true(all(result$partner_code == "USA-MDA"))
})

# ---------------------------------------------------------------------------
# Filtering — multi-value vector
# ---------------------------------------------------------------------------

test_that("multi-value filter returns union of matching rows", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_registry("site_names", partner_code = c("USA-MDA", "BLZ-CBC"))

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

  result <- utl_mg_get_registry("site_names", partner_code = "USA-MDA", habitat = "seagrass")

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
    utl_mg_get_registry("site_names", not_a_col = "USA-MDA"),
    "Unknown filter column"
  )
})

test_that("error for unknown filter column lists valid columns", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_get_registry("site_names", not_a_col = "x"),
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
    result <- utl_mg_get_registry("site_names", partner_code = "ZZZ-XXX"),
    "No rows matched"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})
