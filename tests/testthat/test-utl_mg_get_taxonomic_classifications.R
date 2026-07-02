# utl_mg_get_taxonomic_classifications() reconstructs wide-form classifications
# at call time from the `taxonomic_lookup` adjacency table (resolved through the
# registry). The test suite runs offline (see setup.R), so the resolver returns
# the mocked `marinegeo_metadata$taxonomic_lookup` below.
#
# Adjacency fixture: two lineages.
#   Animal: Animalia > Chordata > Actinopterygii > Perciformes > Labridae >
#           Thalassoma > Thalassoma bifasciatum / T. lunare
#   Plant:  Plantae > Tracheophyta (Phylum (Division)) > Liliopsida >
#           Alismatales > Zosteraceae > Zostera > Zostera marina
mock_taxonomic_lookup <- tibble::tribble(
  ~id     , ~scientific_id , ~parent_id , ~rank               , ~name                    ,
       1L , "APHIA:1"      , NA         , "Kingdom"           , "Animalia"               ,
       2L , "APHIA:2"      , "APHIA:1"  , "Phylum"            , "Chordata"               ,
       3L , "APHIA:3"      , "APHIA:2"  , "Class"             , "Actinopterygii"         ,
       4L , "APHIA:4"      , "APHIA:3"  , "Order"             , "Perciformes"            ,
       5L , "APHIA:5"      , "APHIA:4"  , "Family"            , "Labridae"               ,
       6L , "APHIA:6"      , "APHIA:5"  , "Genus"             , "Thalassoma"             ,
  125476L , "APHIA:125476" , "APHIA:6"  , "Species"           , "Thalassoma bifasciatum" ,
  125477L , "APHIA:125477" , "APHIA:6"  , "Species"           , "Thalassoma lunare"      ,
      10L , "APHIA:10"     , NA         , "Kingdom"           , "Plantae"                ,
      11L , "APHIA:11"     , "APHIA:10" , "Phylum (Division)" , "Tracheophyta"           ,
      12L , "APHIA:12"     , "APHIA:11" , "Class"             , "Liliopsida"             ,
      13L , "APHIA:13"     , "APHIA:12" , "Order"             , "Alismatales"            ,
      14L , "APHIA:14"     , "APHIA:13" , "Family"            , "Zosteraceae"            ,
      15L , "APHIA:15"     , "APHIA:14" , "Genus"             , "Zostera"                ,
  374534L , "APHIA:374534" , "APHIA:15" , "Species"           , "Zostera marina"
)

mock_metadata <- list(taxonomic_lookup = mock_taxonomic_lookup)

# ---------------------------------------------------------------------------
# Return type and column structure
# ---------------------------------------------------------------------------

test_that("result is a data frame", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications("APHIA:374534")
  expect_s3_class(result, "data.frame")
})

test_that("result has scientific_id and rank as first two columns", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications("APHIA:374534")
  expect_equal(colnames(result)[1], "scientific_id")
  expect_equal(colnames(result)[2], "rank")
})

# ---------------------------------------------------------------------------
# Happy-path tests
# ---------------------------------------------------------------------------

test_that("single ID returns one row with correct values", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications("APHIA:374534")

  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id, "APHIA:374534")
  expect_equal(result$rank, "Species")
  expect_equal(result$Kingdom, "Plantae")
  expect_equal(result$Phylum, "Tracheophyta")
  expect_equal(result$Family, "Zosteraceae")
  expect_equal(result$Species, "Zostera marina")
})

test_that("multiple IDs return one row each", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications(
    c("APHIA:374534", "APHIA:125476", "APHIA:125477")
  )

  expect_equal(nrow(result), 3)
  expect_setequal(
    result$scientific_id,
    c("APHIA:374534", "APHIA:125476", "APHIA:125477")
  )
})

test_that("different kingdoms return correct values in the same columns", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications(
    c("APHIA:374534", "APHIA:125476")
  )

  plant_row <- result[result$scientific_id == "APHIA:374534", ]
  fish_row <- result[result$scientific_id == "APHIA:125476", ]

  expect_equal(plant_row$Kingdom, "Plantae")
  expect_equal(fish_row$Kingdom, "Animalia")
})

# ---------------------------------------------------------------------------
# ranks argument (custom column subset)
# ---------------------------------------------------------------------------

test_that("ranks argument restricts and orders the rank columns", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications(
    "APHIA:374534",
    ranks = c("Family", "Genus", "Species")
  )

  expect_equal(
    colnames(result),
    c("scientific_id", "rank", "Family", "Genus", "Species")
  )
  expect_equal(result$Family, "Zosteraceae")
  expect_equal(result$Species, "Zostera marina")
})

test_that("ranks argument can select a single level", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications(
    "APHIA:374534",
    ranks = "Kingdom"
  )

  expect_equal(colnames(result), c("scientific_id", "rank", "Kingdom"))
  expect_equal(result$Kingdom, "Plantae")
})

test_that("unknown rank in `ranks` stops with an informative error", {
  expect_error(
    utl_mg_get_taxonomic_classifications("APHIA:374534", ranks = "Domain"),
    "Unknown taxonomic rank"
  )
})

test_that("non-character `ranks` stops with an informative error", {
  expect_error(
    utl_mg_get_taxonomic_classifications("APHIA:374534", ranks = 1:3),
    "`ranks` must be a character vector"
  )
})

# ---------------------------------------------------------------------------
# Unknown / unmatched IDs
# ---------------------------------------------------------------------------

test_that("unknown ID returns zero-row data frame without error", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications("APHIA:99999999")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("FUNCTIONAL: ID returns zero rows without error", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications("FUNCTIONAL:1")
  expect_equal(nrow(result), 0)
})

test_that("mix of known and unknown IDs: only known IDs appear in result", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_taxonomic_classifications(
    c("APHIA:374534", "APHIA:99999999")
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id, "APHIA:374534")
})

# ---------------------------------------------------------------------------
# Edge cases: empty input, NA input
# ---------------------------------------------------------------------------

test_that("empty character vector returns zero-row data frame with scientific_id and rank columns", {
  result <- utl_mg_get_taxonomic_classifications(character(0))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("scientific_id", "rank") %in% colnames(result)))
})

test_that("all-NA input removes NAs with message and returns zero-row data frame", {
  expect_message(
    result <- utl_mg_get_taxonomic_classifications(c(
      NA_character_,
      NA_character_
    )),
    "2 NA value\\(s\\) removed"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("NAs mixed with valid IDs: NAs removed with message, valid IDs processed", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_get_taxonomic_classifications(
      c(NA_character_, "APHIA:374534", "APHIA:125476")
    ),
    "1 NA value\\(s\\) removed"
  )

  expect_equal(nrow(result), 2)
  expect_setequal(result$scientific_id, c("APHIA:374534", "APHIA:125476"))
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-character input stops with an informative error", {
  expect_error(
    utl_mg_get_taxonomic_classifications(12345),
    "`scientific_ids` must be a character vector"
  )
})

test_that("logical input stops with an informative error", {
  expect_error(
    utl_mg_get_taxonomic_classifications(TRUE),
    "`scientific_ids` must be a character vector"
  )
})
