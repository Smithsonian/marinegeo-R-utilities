# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.db_struct_uuid <- data.frame(
  table_id      = c("sav_cover_v1", "sav_cover_v1", "sav_cover_v1"),
  column_name   = c("site_code", "transect_id", "percent_cover"),
  uuid_identity = c(TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

.db_struct_no_uuid <- data.frame(
  table_id      = c("sav_cover_v1", "sav_cover_v1"),
  column_name   = c("site_code", "percent_cover"),
  uuid_identity = c(FALSE, FALSE),
  stringsAsFactors = FALSE
)

.db_struct_no_col <- data.frame(
  table_id      = "sav_cover_v1",
  column_name   = "site_code",
  stringsAsFactors = FALSE
)

.mock_meta_uuid <- list(
  database_structure = .db_struct_uuid
)

.mock_meta_no_uuid <- list(
  database_structure = .db_struct_no_uuid
)

.mock_meta_no_col <- list(
  database_structure = .db_struct_no_col
)

.sample_df <- data.frame(
  site_code     = c("BIS-001", "BIS-001", "CCN-001"),
  transect_id   = c(1L, 2L, 1L),
  percent_cover = c(45.2, 30.1, 60.0),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("row_uuid column is added and is the first column", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result)[1], "row_uuid")
  expect_equal(nrow(result), nrow(.sample_df))
})

test_that("all other columns are preserved", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")

  expect_true(all(c("site_code", "transect_id", "percent_cover") %in% colnames(result)))
})

test_that("row_uuid values are character strings", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")

  expect_type(result$row_uuid, "character")
  expect_equal(length(result$row_uuid), nrow(.sample_df))
})

# ---------------------------------------------------------------------------
# UUID format
# ---------------------------------------------------------------------------

test_that("row_uuid values match UUID format (8-4-4-4-12 hex)", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")

  uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  expect_true(all(grepl(uuid_pattern, result$row_uuid)))
})

# ---------------------------------------------------------------------------
# Stability (determinism)
# ---------------------------------------------------------------------------

test_that("same input always produces the same UUIDs", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  result1 <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")
  result2 <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")

  expect_equal(result1$row_uuid, result2$row_uuid)
})

# ---------------------------------------------------------------------------
# Uniqueness
# ---------------------------------------------------------------------------

test_that("different identity-column values produce different UUIDs", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1")

  expect_equal(length(unique(result$row_uuid)), nrow(.sample_df))
})

test_that("non-identity columns do not affect UUID", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  df_a <- .sample_df
  df_b <- .sample_df
  df_b$percent_cover <- 99.9  # non-identity column changed

  result_a <- utl_mg_generate_row_uuid(df_a, "sav_cover_v1")
  result_b <- utl_mg_generate_row_uuid(df_b, "sav_cover_v1")

  expect_equal(result_a$row_uuid, result_b$row_uuid)
})

# ---------------------------------------------------------------------------
# Overwrite guard
# ---------------------------------------------------------------------------

test_that("existing row_uuid column triggers a warning and is regenerated", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  df_with_uuid <- dplyr::mutate(.sample_df, row_uuid = "old-value")

  expect_warning(
    result <- utl_mg_generate_row_uuid(df_with_uuid, "sav_cover_v1"),
    "already contains a `row_uuid` column"
  )

  uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  expect_true(all(grepl(uuid_pattern, result$row_uuid)))
})

# ---------------------------------------------------------------------------
# NA handling
# ---------------------------------------------------------------------------

test_that("NA in identity column does not error and produces a valid UUID", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  df_na <- .sample_df
  df_na$site_code[1] <- NA_character_

  expect_no_error(result <- utl_mg_generate_row_uuid(df_na, "sav_cover_v1"))

  uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  expect_true(all(grepl(uuid_pattern, result$row_uuid)))
})

# ---------------------------------------------------------------------------
# Error: invalid table_id
# ---------------------------------------------------------------------------

test_that("unknown table_id -> stop with informative message", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_generate_row_uuid(.sample_df, "nonexistent_table"),
    "No rows found in `database_structure`"
  )
})

# ---------------------------------------------------------------------------
# Error: no uuid_identity = TRUE columns
# ---------------------------------------------------------------------------

test_that("no uuid_identity = TRUE columns -> stop", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_no_uuid,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1"),
    "No columns with `uuid_identity = TRUE`"
  )
})

# ---------------------------------------------------------------------------
# Error: missing uuid_identity column in database_structure
# ---------------------------------------------------------------------------

test_that("database_structure without uuid_identity column -> stop", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_no_col,
    .package = "marinegeo.utils"
  )

  expect_error(
    utl_mg_generate_row_uuid(.sample_df, "sav_cover_v1"),
    "`database_structure` does not have a `uuid_identity` column"
  )
})

# ---------------------------------------------------------------------------
# Error: identity columns missing from data
# ---------------------------------------------------------------------------

test_that("identity column absent from data -> stop with column name", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_meta_uuid,
    .package = "marinegeo.utils"
  )

  df_missing <- .sample_df[, c("transect_id", "percent_cover"), drop = FALSE]

  expect_error(
    utl_mg_generate_row_uuid(df_missing, "sav_cover_v1"),
    '"site_code"'
  )
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-data-frame data -> stop", {
  expect_error(
    utl_mg_generate_row_uuid("not a df", "sav_cover_v1"),
    "`data` must be a data frame"
  )
})

test_that("non-character table_id -> stop", {
  expect_error(
    utl_mg_generate_row_uuid(.sample_df, 123),
    "`table_id` must be a single non-NA character string"
  )
})

test_that("NA table_id -> stop", {
  expect_error(
    utl_mg_generate_row_uuid(.sample_df, NA_character_),
    "`table_id` must be a single non-NA character string"
  )
})

test_that("length-2 table_id -> stop", {
  expect_error(
    utl_mg_generate_row_uuid(.sample_df, c("sav_cover_v1", "sav_density_v1")),
    "`table_id` must be a single non-NA character string"
  )
})
