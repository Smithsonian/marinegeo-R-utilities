# Test RLS EPA species IDs
test_that("utl_join_taxonomy_by_scientific_name works with EPA RLS data", {

  # Load list of RLS data loaded with `utl_rls_load_excel.R`
  input_list <- readRDS(test_path("testdata/data_list-utl_rls_load_excel.RDS"))

  # Test input with partial matches
  result1 <- lapply(input_list, function(i){
    expect_warning(
      utl_join_taxonomy_by_scientific_name(i,
                                           identification_column_name = "species"),
      "\\d{1,3} scientific names could not be matched in taxonomic database"
    )
  })

  expect_length(result1, 17)

  # Don't define species column, generate error
  result3 <- lapply(input_list, function(i){
    expect_error(
      utl_join_taxonomy_by_scientific_name(i),
      "Missing required column(s): scientific_name",
      fixed = TRUE
    )
  })
})

test_that("utl_join_taxonomy_by_scientific_name creates proper errors with dummy data", {

  expect_error(
    utl_join_taxonomy_by_scientific_name("not a data frame"),
    "`df` must be a dataframe",
    fixed = TRUE
  )

  expect_error(
    utl_join_taxonomy_by_scientific_name(data.frame(), 123),
    "`identification_column_name` must be a character",
    fixed = TRUE
  )

})
