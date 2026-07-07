# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.mock_schema <- data.frame(
  table_id    = "test_table_v1",
  column_name = c("site", "count", "cover", "flag", "survey_date", "present", "ts_col"),
  data_type   = c("STRING", "INT", "DOUBLE", "TINYINT", "DATE", "BOOL", "TIMESTAMP"),
  stringsAsFactors = FALSE
)

.mock_meta <- list(database_structure = .mock_schema)

.good_df <- data.frame(
  site        = "A",
  count       = 5L,
  cover       = 0.75,
  flag        = 1L,
  survey_date = as.Date("2024-01-01"),
  present     = TRUE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("all correct types -> returns df identically", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  result <- utl_mg_test_data_types(.good_df, "test_table_v1")
  expect_identical(result, .good_df)
})

test_that("return value is invisible (pipe-safe)", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  vis <- withVisible(utl_mg_test_data_types(.good_df, "test_table_v1"))
  expect_false(vis$visible)
})

test_that("result can be piped to a subsequent function", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  result <- .good_df |>
    utl_mg_test_data_types("test_table_v1") |>
    dplyr::mutate(extra = 1L)
  expect_true("extra" %in% names(result))
})

# ---------------------------------------------------------------------------
# STRING: any type is accepted
# ---------------------------------------------------------------------------

test_that("numeric column where STRING expected -> pass", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$site <- 99
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

test_that("logical column where STRING expected -> pass", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$site <- TRUE
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

# ---------------------------------------------------------------------------
# Type failures
# ---------------------------------------------------------------------------

test_that("character where DOUBLE expected -> stop with column name in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$cover <- "0.75"
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'cover'")
})

test_that("character where INT expected -> stop with column name in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$count <- "5"
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'count'")
})

test_that("character where DATE expected -> stop with column name in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$survey_date <- "2024-01-01"
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'survey_date'")
})

test_that("integer where BOOL expected -> stop with column name in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$present <- 1L
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'present'")
})

test_that("character where BOOL expected -> stop with column name in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$present <- "TRUE"
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'present'")
})

# ---------------------------------------------------------------------------
# INT / TINYINT decimal checks
# ---------------------------------------------------------------------------

test_that("INT column with integer -> pass", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$count <- 5L
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

test_that("INT column with logical -> pass", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$count <- TRUE
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

test_that("INT column with numeric whole numbers -> pass", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$count <- 5.0
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

test_that("INT column with numeric non-integers -> stop", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$count <- 5.5
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'count'")
})

test_that("TINYINT column with numeric non-integers -> stop", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$flag <- 1.7
  expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'flag'")
})

test_that("INT column all-NA numeric -> pass (empty decimal check)", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$count <- as.numeric(NA)
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

test_that("INT column numeric vector with NAs and whole numbers -> pass", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- data.frame(
    site = "A", count = c(1.0, NA, 3.0), cover = 0.75, flag = 1L,
    survey_date = as.Date("2024-01-01"), present = TRUE,
    stringsAsFactors = FALSE
  )
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

# ---------------------------------------------------------------------------
# Multiple failures -> single stop listing all columns
# ---------------------------------------------------------------------------

test_that("multiple type failures -> single stop with all column names in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$cover   <- "bad"   # DOUBLE expects numeric
  df$present <- 1L      # BOOL expects logical
  err <- tryCatch(
    utl_mg_test_data_types(df, "test_table_v1"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "'cover'")
  expect_match(err, "'present'")
})

# ---------------------------------------------------------------------------
# All-NA logical columns
# ---------------------------------------------------------------------------

test_that("all-NA logical column with non-BOOL expected -> message not stop", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$cover <- NA   # logical NA — read_csv artifact
  expect_message(
    expect_no_error(utl_mg_test_data_types(df, "test_table_v1")),
    "'cover'"
  )
})

test_that("all-NA logical column with BOOL expected -> pass silently", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$present <- NA   # logical NA satisfies BOOL
  expect_no_message(
    expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
  )
})

test_that("all-NA logical (non-BOOL) and a real failure -> message + stop", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$cover   <- NA      # all-NA logical -> message
  df$present <- 1L      # real BOOL failure -> stop
  expect_message(
    expect_error(utl_mg_test_data_types(df, "test_table_v1"), "'present'"),
    "'cover'"
  )
})

# ---------------------------------------------------------------------------
# Skipped columns
# ---------------------------------------------------------------------------

test_that("column in df not in metadata -> silently skipped", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$extra_col <- list(1)   # would fail any type check if evaluated
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

test_that("unknown SQL type (TIMESTAMP) in metadata -> silently skipped", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  df <- .good_df
  df$ts_col <- "2024-01-01T00:00:00Z"   # character; would fail DATE check
  expect_no_error(utl_mg_test_data_types(df, "test_table_v1"))
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-data-frame df -> stop with informative error", {
  expect_error(
    utl_mg_test_data_types(list(a = 1), "test_table_v1"),
    "`df` must be a data frame"
  )
})

test_that("non-character table_id -> stop", {
  expect_error(
    utl_mg_test_data_types(.good_df, 123),
    "`table_id` must be a single non-NA character string"
  )
})

test_that("NA table_id -> stop", {
  expect_error(
    utl_mg_test_data_types(.good_df, NA_character_),
    "`table_id` must be a single non-NA character string"
  )
})

test_that("length > 1 table_id -> stop", {
  expect_error(
    utl_mg_test_data_types(.good_df, c("a", "b")),
    "`table_id` must be a single non-NA character string"
  )
})

test_that("table_id not in metadata -> stop with table_id in message", {
  local_mocked_bindings(marinegeo_metadata = .mock_meta, .package = "marinegeo.utils")
  expect_error(
    utl_mg_test_data_types(.good_df, "nonexistent_table_v99"),
    "nonexistent_table_v99"
  )
})
