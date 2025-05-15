# Test RLS EPA species IDs
test_that("db_get_taxonomy_by_scientific_name works with EPA RLS M1 & M2 species names", {

  # Load all species names in the EPA RLS M1 and M2 sheets
  rls_species <- dplyr::bind_rows(
    readxl::read_excel(test_path("testdata/test_rls_data_EPA_one_header_row.xlsx"), sheet = "M1"),
    readxl::read_excel(test_path("testdata/test_rls_data_EPA_one_header_row.xlsx"), sheet = "M2")
  ) |>
    dplyr::select(`Species Name`) |>
    dplyr::distinct() |>
    dplyr::mutate(scientific_name = gsub(" spp.", "", `Species Name`))

  scientific_names <- unique(rls_species$scientific_name)

  # Will need to update text
  expect_warning(
    result1 <- db_get_taxonomy_by_scientific_name(scientific_names),
    "53 scientific names could not be matched in taxonomic database"
  )

  expect_type(result1, "list")
  expect_equal(nrow(result1) > 1, TRUE)
  expect_equal(length(colnames(result1)), 34)

  # Test specifying classification levels
  expect_warning(
    result2 <- db_get_taxonomy_by_scientific_name(scientific_names, "phylum"),
    "53 scientific names could not be matched in taxonomic database"
  )

  expect_type(result2, "list")
  expect_equal(nrow(result2) > 1, TRUE)
  expect_equal(length(colnames(result2)), 3)

  # This needs to be converted into a snapshot
  # Test with wrong classification level
  # expect_warning(
  #   result3 <- db_get_taxonomy_by_scientific_name(scientific_names, "pylum"),
  #   "53 scientific names could not be matched in taxonomic database",
  #   "The following taxonomic level(s) is not defined in the database table: pylum"
  # )

  # expect_type(result3, "list")
  # expect_equal(nrow(result3) > 1, TRUE)
  # expect_equal(length(colnames(result3)), 2)

})

# Test RLS EPA species IDs
test_that("db_get_taxonomy_by_scientific_name warns with invalid input", {

  # Setup test files
  scientific_names <- c(1, 4, 6)

  expect_error(
    result1 <- db_get_taxonomy_by_scientific_name(scientific_names),
    "`scientific_names` must be a character vector",
    fixed = TRUE
  )

  scientific_names <- c(NULL)

  expect_error(
    result2 <- db_get_taxonomy_by_scientific_name(scientific_names),
    "`scientific_names` must be a character vector",
    fixed = TRUE
  )

})
