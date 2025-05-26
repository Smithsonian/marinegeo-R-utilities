#Test with RLS data
test_that("qc_mg_missing_taxonomic_id works with dummy data", {

  df <- utl_rls_load_excel(test_path("testdata/test_rls_data_EPA_one_header_row.xlsx"))

  expect_warning(
    df <- utl_join_taxonomy_by_scientific_name(df, "species")
  )

  result1 <- qc_mg_missing_taxonomic_id(df)
  expect_type(result1, "integer")
  expect_equal(length(result1), 40)

  # Test non-existent data frame
  expect_error(
    result2 <- qc_mg_missing_taxonomic_id(c(1,2,3))
  )

})
