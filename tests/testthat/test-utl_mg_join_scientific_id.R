test_that("happy path: adds scientific_id column to df", {
  df <- data.frame(
    scientific_name = c("Zostera marina", "Halodule wrightii"),
    value = c(10, 20)
  )

  # Mock marinegeo_metadata in the package environment
  obs_lookup <- data.frame(
    scientific_name = c("Zostera marina", "Halodule wrightii"),
    scientific_id = c("APHIA:374p", "APHIA:374q")
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = obs_lookup),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_join_scientific_id(df)

  expect_true("scientific_id" %in% colnames(result))
  expect_equal(nrow(result), nrow(df))
  expect_equal(result$scientific_id, c("APHIA:374p", "APHIA:374q"))
})

test_that("unmatched scientific names produce NA and a warning", {
  df <- data.frame(scientific_name = c("Zostera marina", "Unknown sp."))

  obs_lookup <- data.frame(
    scientific_name = "Zostera marina",
    scientific_id = "APHIA:374p"
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = obs_lookup),
    .package = "marinegeo.utils"
  )

  expect_warning(
    result <- utl_mg_join_scientific_id(df),
    "1 scientific name\\(s\\) could not be matched"
  )
  expect_true(is.na(result$scientific_id[
    result$scientific_name == "Unknown sp."
  ]))
})

test_that("custom scientific_name_col is handled and restored", {
  df <- data.frame(
    Species = c("Zostera marina"),
    value = 5
  )

  obs_lookup <- data.frame(
    scientific_name = "Zostera marina",
    scientific_id = "APHIA:374p"
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = obs_lookup),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_join_scientific_id(df, scientific_name_col = "Species")

  expect_true("Species" %in% colnames(result))
  expect_false("scientific_name" %in% colnames(result))
  expect_equal(result$scientific_id, "APHIA:374p")
})

test_that("existing scientific_id column is overwritten with a warning", {
  df <- data.frame(
    scientific_name = "Zostera marina",
    scientific_id = "OLD"
  )

  obs_lookup <- data.frame(
    scientific_name = "Zostera marina",
    scientific_id = "APHIA:374p"
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = obs_lookup),
    .package = "marinegeo.utils"
  )

  expect_warning(
    result <- utl_mg_join_scientific_id(df),
    "already contains a `scientific_id` column"
  )
  expect_equal(result$scientific_id, "APHIA:374p")
})

test_that("stops on non-data-frame input", {
  expect_error(
    utl_mg_join_scientific_id("not a df"),
    "`df` must be a data frame"
  )
})

test_that("stops when scientific_name_col is missing from df", {
  df <- data.frame(x = 1)
  expect_error(
    utl_mg_join_scientific_id(df, scientific_name_col = "scientific_name"),
    "Column 'scientific_name' not found"
  )
})

test_that("stops on invalid scientific_name_col type", {
  df <- data.frame(scientific_name = "Zostera marina")
  expect_error(
    utl_mg_join_scientific_id(df, scientific_name_col = 123),
    "`scientific_name_col` must be a single character string"
  )
})

test_that("returns empty df unchanged when input has zero rows", {
  df <- data.frame(scientific_name = character(0), value = numeric(0))

  obs_lookup <- data.frame(
    scientific_name = "Zostera marina",
    scientific_id = "APHIA:374p"
  )

  local_mocked_bindings(
    marinegeo_metadata = list(observation_lookup = obs_lookup),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_join_scientific_id(df)
  expect_equal(nrow(result), 0)
  expect_true("scientific_id" %in% colnames(result))
})
