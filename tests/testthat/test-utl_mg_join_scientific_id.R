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

# ---------------------------------------------------------------------------
# include_classifications = TRUE
# ---------------------------------------------------------------------------

# Shared fixtures for classification tests
.obs_lookup_cls <- data.frame(
  scientific_name = c("Zostera marina", "Halodule wrightii"),
  scientific_id   = c("APHIA:374p", "APHIA:374q"),
  stringsAsFactors = FALSE
)

.cls_table <- data.frame(
  scientific_id = c("APHIA:374p", "APHIA:374q"),
  rank          = c("Species",    "Species"),
  Kingdom       = c("Plantae",    "Plantae"),
  Phylum        = c("Tracheophyta", "Tracheophyta"),
  Class         = c("Liliopsida", "Liliopsida"),
  Order         = c("Alismatales", "Alismatales"),
  Family        = c("Zosteraceae", "Cymodoceaceae"),
  Genus         = c("Zostera",    "Halodule"),
  Species       = c("Zostera marina", "Halodule wrightii"),
  stringsAsFactors = FALSE
)

test_that("include_classifications = TRUE appends rank columns", {
  df <- data.frame(
    scientific_name = c("Zostera marina", "Halodule wrightii"),
    value = c(10, 20)
  )

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_join_scientific_id(df, include_classifications = TRUE)

  expect_true(all(
    c("scientific_id", "rank", "Kingdom", "Phylum", "Class",
      "Order", "Family", "Genus", "Species") %in% colnames(result)
  ))
})

test_that("include_classifications = TRUE populates classification values correctly", {
  df <- data.frame(scientific_name = "Zostera marina", value = 5)

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_join_scientific_id(df, include_classifications = TRUE)

  expect_equal(result$Kingdom, "Plantae")
  expect_equal(result$Family,  "Zosteraceae")
  expect_equal(result$Species, "Zostera marina")
})

test_that("include_classifications = FALSE (default) does not append rank columns", {
  df <- data.frame(scientific_name = "Zostera marina", value = 5)

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_join_scientific_id(df)

  expect_false(any(c("Kingdom", "rank") %in% colnames(result)))
})

test_that("conflicting classification columns are skipped with a warning", {
  # df already has a 'Family' column
  df <- data.frame(
    scientific_name = "Zostera marina",
    Family = "my_family",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  expect_warning(
    result <- utl_mg_join_scientific_id(df, include_classifications = TRUE),
    "1 classification column\\(s\\) already present"
  )

  # Original value preserved, not overwritten
  expect_equal(result$Family, "my_family")
})

test_that("warning for conflicting columns names each conflict", {
  df <- data.frame(
    scientific_name = "Zostera marina",
    Kingdom = "my_kingdom",
    Phylum  = "my_phylum",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  expect_warning(
    utl_mg_join_scientific_id(df, include_classifications = TRUE),
    "Kingdom"
  )
})

test_that("non-conflicting classification columns are added even when some conflict", {
  df <- data.frame(
    scientific_name = "Zostera marina",
    Kingdom = "my_kingdom",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  suppressWarnings(
    result <- utl_mg_join_scientific_id(df, include_classifications = TRUE)
  )

  # Kingdom not overwritten, but Family (non-conflicting) IS added
  expect_equal(result$Kingdom, "my_kingdom")
  expect_equal(result$Family,  "Zosteraceae")
})

test_that("include_classifications = TRUE with all-unmatched names adds no rank columns", {
  df <- data.frame(scientific_name = "Unknown sp.", value = 1)

  local_mocked_bindings(
    marinegeo_metadata = list(
      observation_lookup      = .obs_lookup_cls,
      taxonomic_classifications = .cls_table
    ),
    .package = "marinegeo.utils"
  )

  suppressWarnings(
    result <- utl_mg_join_scientific_id(df, include_classifications = TRUE)
  )

  expect_false("Kingdom" %in% colnames(result))
})

test_that("invalid include_classifications value stops with an error", {
  df <- data.frame(scientific_name = "Zostera marina")

  expect_error(
    utl_mg_join_scientific_id(df, include_classifications = "yes"),
    "`include_classifications` must be a single logical value"
  )
})
