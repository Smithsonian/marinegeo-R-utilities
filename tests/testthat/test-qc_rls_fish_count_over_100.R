#Test Dummy Data
test_that("qc_rls_fish_count_over_100 works with dummy data", {

  # Data frame with all required columns and no fish count over 100
  df_complete_under100 <- data.frame(
    total = c(4,5,2,10,14),
    phylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  expect_message(
    result1 <- qc_rls_fish_count_over_100(df_complete_under100),
    "There are no fish with counts over 100"
  )

  expect_type(result1, "NULL")
  expect_equal(length(result1), 0)


  # Data frame with all required columns and includes fish count over 100
  df_complete_over100 <- data.frame(
    total = c(4,25,22,22,105),
    phylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  result2 <- qc_rls_fish_count_over_100(df_complete_over100)
  expect_type(result2, "integer")
  expect_equal(length(result2), 1)
  expect_equal(result2, 5)

  # Data frame with no fish species
  df_only_inverts <- data.frame(
    total = c(4,5,2,10,14),
    phylum = c("Invert","Invert","Invert","Invert","Invert")
  )

  expect_message(
    result3 <- qc_rls_fish_count_over_100(df_only_inverts),
    "Data Frame Contains only Inverts or no observations"
  )
  expect_type(result3, "NULL")
  expect_equal(length(result3), 0)

  # Data Frame Missing total column
  df_missing_total_col <- data.frame(
    totals = c(12, 10, 20, 30, 0),
    phylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  expect_error(
    result4 <- qc_rls_fish_count_over_100(df_missing_total_col)
  )

  # Data Frame Missing phylum column
  df_missing_phylum_col <- data.frame(
    total = c(12, 10, 20, 30, 0),
    pylum = c("Invert","Invert","Chordata","Invert","Chordata")
  )

  expect_error(
    result5 <- qc_rls_fish_count_over_100(df_missing_phylum_col)
  )

  # Test non-existent data frame
  expect_error(
    result6 <- qc_rls_fish_count_over_100(c(1,2,3))
  )

})
