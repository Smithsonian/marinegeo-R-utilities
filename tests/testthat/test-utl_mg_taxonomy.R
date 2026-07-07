# ---------------------------------------------------------------------------
# utl_mg_get_scientific_id
# ---------------------------------------------------------------------------

.obs_lookup_get <- data.frame(
  scientific_name = c("Zostera marina", "Halodule wrightii"),
  scientific_id = c("APHIA:374p", "APHIA:374q"),
  stringsAsFactors = FALSE
)

test_that("happy path: returns matched scientific_ids as a character vector", {
  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_scientific_id(c("Zostera marina", "Halodule wrightii"))

  expect_type(result, "character")
  expect_equal(length(result), 2)
  expect_equal(result, c("APHIA:374p", "APHIA:374q"))
})

test_that("unmatched names return NA and trigger a warning", {
  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  expect_warning(
    result <- utl_mg_get_scientific_id(c("Zostera marina", "Fake species")),
    "1 scientific name\\(s\\) could not be matched"
  )
  expect_equal(result[1], "APHIA:374p")
  expect_true(is.na(result[2]))
})

test_that("NA input elements pass through as NA without a warning", {
  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  expect_no_warning(
    result <- utl_mg_get_scientific_id(c("Zostera marina", NA))
  )
  expect_equal(result[1], "APHIA:374p")
  expect_true(is.na(result[2]))
})

test_that("drop_abbreviations strips trailing sp. / spp. before matching", {
  obs_lookup_genus <- data.frame(
    scientific_name = "Halodule",
    scientific_id = "APHIA:374q",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = obs_lookup_genus),
    .mg_fetch_registry = function(url) obs_lookup_genus,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_scientific_id("Halodule sp.", drop_abbreviations = TRUE)
  expect_equal(result, "APHIA:374q")
})

test_that("drop_abbreviations = FALSE does not strip abbreviations", {
  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  expect_warning(
    result <- utl_mg_get_scientific_id(
      "Zostera sp.",
      drop_abbreviations = FALSE
    ),
    "could not be matched"
  )
  expect_true(is.na(result))
})

test_that("output length always matches input length", {
  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  input <- c("Zostera marina", "Fake sp.", NA, "Halodule wrightii")
  suppressWarnings(result <- utl_mg_get_scientific_id(input))
  expect_equal(length(result), length(input))
})

test_that("works inside dplyr::mutate()", {
  df <- data.frame(
    scientific_name = c("Zostera marina", "Halodule wrightii"),
    value = c(1, 2)
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  result <- dplyr::mutate(
    df,
    scientific_id = utl_mg_get_scientific_id(scientific_name)
  )

  expect_equal(result$scientific_id, c("APHIA:374p", "APHIA:374q"))
})

test_that("stops on non-character input", {
  expect_error(
    utl_mg_get_scientific_id(123),
    "`scientific_name` must be a character vector"
  )
})

test_that("stops on invalid drop_abbreviations value", {
  expect_error(
    utl_mg_get_scientific_id("Zostera marina", drop_abbreviations = "yes"),
    "`drop_abbreviations` must be a single logical value"
  )
})

test_that("empty character vector returns empty character vector", {
  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = .obs_lookup_get),
    .mg_fetch_registry = function(url) .obs_lookup_get,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_scientific_id(character(0))
  expect_equal(result, character(0))
})
