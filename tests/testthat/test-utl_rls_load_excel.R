# Setup: Create test Excel files in temporary directory
setup_test_files <- function() {
  # Create a temporary directory for test files
  test_dir <- file.path(tempdir(), "test_excel")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  # File with both Method and Total columns
  # a single header row
  # and trailing empty row
  df_complete_1_header <- data.frame(
    Method = c("0, 1, 2", 1, 2, 1, 1),
    Total = c(NA, 10, 20, 30, 0)
  )
  complete_1_file <- file.path(test_dir, "complete_1_header.xlsx")
  writexl::write_xlsx(list("DATA" = df_complete_1_header), complete_1_file)

  # File with both Method and Total columns
  # two header rows
  # and trailing empty row
  df_complete_2_header <- data.frame(
    Method = c("0, 1, 2", "0, 1, 2", 1, 2, 1, 1),
    Total = c(NA, NA, 10, 20, 30, 0)
  )
  complete_2_file <- file.path(test_dir, "complete_2_header.xlsx")
  writexl::write_xlsx(list("DATA" = df_complete_2_header), complete_2_file)

  # File with no total values
  df_empty_totals <- data.frame(
    Method = c("0, 1, 2", "0, 1, 2", 1, 2, 1, 1),
    Total = c(NA, NA, 0, 0, 0, 0)
  )
  empty_total_file <- file.path(test_dir, "empty_total.xlsx")
  writexl::write_xlsx(list("DATA" = df_empty_totals), empty_total_file)

  # File missing Total
  df_missing_total_col <- data.frame(
    Method = c("0, 1, 2", 1, 2, 1, 1),
    total = c(NA, 10, 20, 30, 0)
  )
  missing_total_file <- file.path(test_dir, "missing_total_column.xlsx")
  writexl::write_xlsx(list("DATA" = df_missing_total_col), missing_total_file)

  # File missing Method
  df_missing_method_col <- data.frame(
    method = c("0, 1, 2", 1, 2, 1, 1),
    Total = c(NA, 10, 20, 30, 0)
  )
  missing_method_file <- file.path(test_dir, "missing_method_column.xlsx")
  writexl::write_xlsx(list("DATA" = df_missing_method_col), missing_method_file)

  # File missing Method
  df_missing_cols <- data.frame(
    method = c("0, 1, 2", 1, 2, 1, 1),
    total = c(NA, 10, 20, 30, 0)
  )
  missing_columns_file <- file.path(test_dir, "missing_columns.xlsx")
  writexl::write_xlsx(list("DATA" = df_missing_cols), missing_columns_file)

  return(list(
    complete_1_header = complete_1_file,
    complete_2_header = complete_2_file,
    no_total_vals = empty_total_file,
    no_total_col = missing_total_file,
    no_method_col = missing_method_file,
    no_req_col = missing_columns_file
  ))
}

# Test fake data
test_that("utl_rls_load_excel works with dummy data", {

  # Setup test files
  files <- setup_test_files()

  # File 1
  result1 <- utl_rls_load_excel(files$complete_1_header)
  expect_type(result1, "list")
  expect_equal(nrow(result1), 3)
  expect_equal(result1$Method, c("1","2","1"))

  # File 2
  result2 <- utl_rls_load_excel(files$complete_2_header)
  expect_type(result2, "list")
  expect_equal(nrow(result2), 3)
  expect_equal(result2$Method, c("1","2","1"))

  # File 3
  result3 <- utl_rls_load_excel(files$no_total_col)
  expect_null(result3, "list")

  # File 4
  result4 <- utl_rls_load_excel(files$no_method_col)
  expect_null(result4, "list")

  # File 5
  result5 <- utl_rls_load_excel(files$no_req_col)
  expect_null(result5, "list")

  # File 6
  expect_warning(
    result6 <- utl_rls_load_excel(files$no_total_vals),
    "Dataframe only has rows with a Total equal to 0"
  )
  expect_type(result6, "list")
  expect_equal(nrow(result6), 4)

  # Test 7: Non-existent file
  expect_message(
    result7 <- utl_rls_load_excel("/data/nonexistent.xlsx"),
    "Error reading Excel file"
  )
  expect_null(result7)
})

# Test RLS Excel data in /inst/extdata
test_that("utl_rls_load_excel works with test RLS data", {

  # Load test data
  test_1_header_row <- test_path("testdata/test_rls_data_EPA_one_header_row.xlsx")
  test_2_header_row <- test_path("testdata/test_rls_data_EPA_two_header_rows.xlsx")

  # File 1 header
  result1 <- utl_rls_load_excel(test_1_header_row)
  expect_type(result1, "list")
  expect_equal(nrow(result1), 282)

  # File 2 header
  result2 <- utl_rls_load_excel(test_2_header_row)
  expect_type(result2, "list")
  expect_equal(nrow(result2), 606)

})

# Test RLS data list in /inst/extdata
test_that("utl_rls_load_excel works with test RLS data", {

  # Load test data list
  comparison_list <- readRDS(test_path("testdata/data_list-utl_rls_load_excel.RDS"))

  # Filepaths of Excel files
  filepaths <- list.files(
    paste0(
      Sys.getenv("onedrive_resources"),
      "R-misc/R-test-resources/reef-life-survey-marinegeo/load-rls-all-excel/data"
    ), full.names = T
  )

  # Only run test if data to test exists on testing machine
  if(dir.exists(Sys.getenv("onedrive_resources"))){

    # Create result to compare with comparison_list
    result1 <- lapply(filepaths, utl_rls_load_excel)

    expect_equal(
      comparison_list,
      result1
    )

    expect_type(result1, "list")
    expect_length(result1, 17)

  }

})
