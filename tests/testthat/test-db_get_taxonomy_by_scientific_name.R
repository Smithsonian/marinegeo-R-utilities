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
