#Setup: Create test RDS object in a temporary directory
setup_test_files <- function() {

  # Data frame with all required columns and no invert count over 20
  df_complete_no_issues <- data.frame(
    inverts = c(4,5,0,10,0),
    total = c(4,5,2,10,14),
    phylum = c("invert","invert","Chordata","invert","Chordata")
  )


  # Data frame with all required columns and includes verts with invert count
  df_complete_verts_with_invert_count <- data.frame(
    inverts = c(4,25,22,22,24),
    total = c(4,25,22,22,24),
    phylum = c("invert","invert","Chordata","invert","Chordata")
  )

  # Data Frame Missing invert column
  df_missing_invert_col <- data.frame(
    method = c("0, 1, 2", 1, 2, 1, 1),
    total = c(12, 10, 20, 30, 0),
    inverts = c(12,10,20,30,0),
    phylum = c("invert","invert","Chordata","invert","Chordata")
  )


  # Data Frame Missing phylum column
  df_missing_phylum_col <- data.frame(
    method = c("0, 1, 2", 1, 2, 1, 1),
    total = c(12, 10, 20, 30, 0),
    inverts = c(12,10,0,30,0),
    pylum = c("invert","invert","Chordata","invert","Chordata")
  )

  return(list(
    complete_no_issues = df_complete_no_issues,
    complete_verts_with_invert_count = df_complete_verts_with_invert_count,
    missing_invert_col = df_missing_invert_col,
    missing_phylum_col = df_missing_phylum_col
  ))
}

#Test Fake Data
test_that("qc_rls_verts_with_invert_count works with dummy data", {

  # Setup test files
  files <- setup_test_files()

  # DF 1
  expect_message(
    result1 <- qc_rls_verts_with_invert_count(files$complete_no_issues),
    "There are no Vertebrates with an invert Count."
  )

  expect_type(result1, "NULL")
  expect_equal(length(result1), 0)

  # DF 2
  result2 <- qc_rls_verts_with_invert_count(files$complete_verts_with_invert_count)
  expect_type(result2, "integer")
  expect_equal(length(result2), 2)
  expect_equal(result2, c(3,5))

  # DF 3
  expect_error(
    result3 <- qc_rls_verts_with_invert_count(files$missing_invert_col)
  )

  # DF 4
  expect_error(
    result4 <- qc_rls_verts_with_invert_count(files$missing_phylum_col)
  )

  #Test 6: non-existent data frame
  expect_error(
    result6 <- qc_rls_verts_with_invert_count(files$non_existent_df)
  )

})

#Test RLS R data sets in /inst/extdata
test_that("qc_rls_verts_with_invert_count works with test RLS data", {

  # Load test data
  test_no_verts_with_invert_count <- readRDS(
    test_path("testdata/test_rls_data_EPA.RDS")
  )

  # File 1
  expect_message(
    result1 <- qc_rls_verts_with_invert_count(test_no_verts_with_invert_count),
    "There are no Vertebrates with an invert Count."
  )
  expect_type(result1, "NULL")

  # Load test data
  test_verts_with_invert_count <- readRDS(
    test_path("testdata/test_rls_data_EPA_verts_with_invert_count.RDS")
  )

  # File 2
  result1 <- qc_rls_verts_with_invert_count(test_verts_with_invert_count)
  expect_type(result1, "integer")
  expect_equal(length(result1), 6)
  expect_equal(result1, c(20, 38, 67, 77, 100, 113))

})
