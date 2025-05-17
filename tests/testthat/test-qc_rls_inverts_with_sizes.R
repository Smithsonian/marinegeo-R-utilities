#Test with RLS data
test_that("qc_rls_inverts_with_sizes works with rls data", {

  df <- utl_rls_load_excel(test_path("testdata/test_rls_data_EPA_one_header_row.xlsx"))

  expect_warning(
    df <- utl_join_taxonomy_by_scientific_name(df, "Species")
  )

  result1 <- qc_rls_inverts_with_sizes(df)
  expect_type(result1, "NULL")

  # Test non-existent data frame
  expect_error(
    result2 <- qc_rls_inverts_with_sizes(c(1,2,3))
  )

})
