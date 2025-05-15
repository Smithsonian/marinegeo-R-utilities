#Test Dummy Data
test_that("qc_rls_total_count_0 works with dummy data", {

  # Data frame with all required columns and no fish count over 100
  df_complete_over_0 <- data.frame(
    Total = c(4,5,2,10,14),
    phylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  expect_message(
    result1 <- qc_rls_total_count_0(df_complete_over_0),
    "There are no Total counts of 0"
  )

  expect_type(result1, "NULL")
  expect_equal(length(result1), 0)

  # Data frame with all required columns and includes total count of 0
  df_complete_with_0 <- data.frame(
    Total = c(4,0,22,0,105),
    phylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  result2 <- qc_rls_total_count_0(df_complete_with_0)
  expect_type(result2, "integer")
  expect_equal(length(result2), 2)
  expect_equal(result2, c(2,4))

  # Data Frame Missing Total column
  df_missing_total_col <- data.frame(
    totals = c(12, 10, 20, 30, 0),
    phylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  expect_error(
    result3 <- qc_rls_total_count_0(df_missing_total_col)
  )

  # Test non-existent data frame
  expect_error(
    result4 <- qc_rls_total_count_0(c(1,2,3))
  )

})
