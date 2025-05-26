#Setup: Create test RDS object in a temporary directory
setup_test_files <- function() {

  # Data frame with all required columns and no invert count over 20
  df_complete_under20 <- data.frame(
    inverts = c(4,5,0,10,0),
    total = c(4,5,2,10,14),
    phylum = c("invert","invert","Chordata","invert","Chordata")
  )

  # Data frame with all required columns and includes invert count over 20
  df_complete_over20 <- data.frame(
    inverts = c(4,25,0,22,0),
    total = c(4,25,22,22,24),
    phylum = c("invert","invert","Chordata","invert","Chordata")
  )

  # Data frame with empty invert values. only chordata
  df_empty_invert_values <- data.frame(
    inverts = c(0,0,0,0,0),
    total = c(4,5,2,10,14),
    phylum = c("Chordata","Chordata","Chordata","Chordata","Chordata")
  )

  # Data Frame Missing invert column
  df_missing_invert_col <- data.frame(
    Method = c("0, 1, 2", 1, 2, 1, 1),
    total = c(12, 10, 20, 30, 0),
    inverts = c(12,10,20,30,0),
    phylum = c("invert","invert","Chordata","invert","Chordata")
  )

  # Data Frame Missing phylum column
  df_missing_phylum_col <- data.frame(
    Method = c("0, 1, 2", 1, 2, 1, 1),
    total = c(12, 10, 20, 30, 0),
    inverts = c(12,10,0,30,0),
    pylum = c("invert","invert","Chordata","invert","Chordata")
  )

  return(list(
    complete_under20 = df_complete_under20,
    complete_over20 = df_complete_over20,
    no_invert_vals = df_empty_invert_values,
    missing_invert_col = df_missing_invert_col,
    missing_phylum_col = df_missing_phylum_col
  ))
}

#Test Fake Data
test_that("qc_rls_inverts_over_20 works with dummy data", {

  # Setup test files
  files <- setup_test_files()

  # DF 1
  expect_message(
    result1 <- qc_rls_inverts_over_20(files$complete_under20),
    "There are no inverts with counts over 20."
  )
  expect_type(result1, "NULL")
  expect_equal(length(result1), 0)

  # DF 2
  result2 <- qc_rls_inverts_over_20(files$complete_over20)
  expect_type(result2, "integer")
  expect_equal(length(result2), 2)
  expect_equal(result2, c(2,4))

  # DF 3
  expect_message(
    result3 <- qc_rls_inverts_over_20(files$no_invert_vals),
    "Data Frame Contains only Chordata Species"
  )
  expect_type(result3, "NULL")
  expect_equal(length(result3), 0)

  # DF 4
  expect_error(
    result4 <- qc_rls_inverts_over_20(files$missing_invert_col)
  )

  # DF 5
  expect_error(
    result5 <- qc_rls_inverts_over_20(files$missing_phylum_col)
  )


  #Test 6: non-existent data frame
  expect_error(
    result6 <- qc_rls_inverts_over_20(files$non_existent_df)
  )
})


#Test RLS R data sets in /inst/extdata
test_that("qc_rls_inverts_over_20 works with test RLS data", {

  # Load test data
  test_without_inverts_over_20 <- readRDS(
    test_path("testdata/test_rls_data_EPA.RDS")
  )

  expect_message(
    result7 <- qc_rls_inverts_over_20(test_without_inverts_over_20),
    "There are no inverts with counts over 20."
  )
  expect_type(result7, "NULL")
  expect_equal(length(result7), 0)

  # Load test data
  test_with_inverts_over_20 <- readRDS(
    test_path("testdata/test_rls_data_EPA_inverts_over_20.RDS")
  )

  # File 1 header
  result8 <- qc_rls_inverts_over_20(test_with_inverts_over_20)
  expect_type(result8, "integer")
  expect_equal(length(result8), 3)
  expect_equal(result8, c(70,76,124))

})




