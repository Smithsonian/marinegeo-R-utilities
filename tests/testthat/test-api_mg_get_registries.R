# ---------------------------------------------------------------------------
# Directory-based live registry fetch with bundled fallback
#
# All tests mock the two network seams (.mg_fetch_tree, .mg_fetch_registry) and
# the bundled marinegeo_metadata, so nothing here touches the network.
# ---------------------------------------------------------------------------

.struct_fixture <- data.frame(
  table_id = c("sav_cover_v1", "sav_cover_v1"),
  column_name = c("cover", "depth"),
  data_type = c("DOUBLE", "DOUBLE"),
  stringsAsFactors = FALSE
)

.bundled_struct <- data.frame(
  table_id = "bundled_v1",
  column_name = "bundled_col",
  data_type = "STRING",
  stringsAsFactors = FALSE
)

.bundled_index <- data.frame(
  table_id = "bundled_v1",
  protocol = "seagrass",
  stringsAsFactors = FALSE
)

.mock_md <- list(
  database_structure = .bundled_struct,
  data_index = .bundled_index
)

# Two CSVs in the database_structure directory plus noise that must be ignored.
.struct_tree <- c(
  "table-metadata/data-structure/part-a.csv",
  "table-metadata/data-structure/part-b.csv",
  "table-metadata/data-structure/README.md",
  "table-metadata/data-structure/nested/skip.csv",
  "sites-and-partners/partner-codes/partners.csv"
)

# ---------------------------------------------------------------------------
# .mg_get_registry_table
# ---------------------------------------------------------------------------

test_that("directory-backed table returns live row-bound data on success", {
  withr::local_options(marinegeo.utils.live_registry = TRUE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() .struct_tree,
    .mg_fetch_registry = function(urls) .struct_fixture,
    .package = "marinegeo.utils"
  )

  result <- .mg_get_registry_table("database_structure")

  expect_equal(result, .struct_fixture)
})

test_that("resolver builds raw URLs for every direct-child CSV in the directory", {
  withr::local_options(marinegeo.utils.live_registry = TRUE)
  captured <- NULL
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() .struct_tree,
    .mg_fetch_registry = function(urls) {
      captured <<- urls
      .struct_fixture
    },
    .package = "marinegeo.utils"
  )

  .mg_get_registry_table("database_structure")

  expect_equal(
    captured,
    paste0(
      .mg_raw_base,
      c(
        "table-metadata/data-structure/part-a.csv",
        "table-metadata/data-structure/part-b.csv"
      )
    )
  )
})

test_that("CSV read failure falls back to bundled data and messages", {
  withr::local_options(marinegeo.utils.live_registry = TRUE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() .struct_tree,
    .mg_fetch_registry = function(urls) stop("offline"),
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- .mg_get_registry_table("database_structure"),
    "using bundled fallback"
  )
  expect_equal(result, .bundled_struct)
})

test_that("tree listing failure falls back to bundled data and messages", {
  withr::local_options(marinegeo.utils.live_registry = TRUE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() stop("offline"),
    .mg_fetch_registry = function(urls) stop("fetcher should not be called"),
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- .mg_get_registry_table("database_structure"),
    "using bundled fallback"
  )
  expect_equal(result, .bundled_struct)
})

test_that("an empty directory falls back without reading any CSV", {
  withr::local_options(marinegeo.utils.live_registry = TRUE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() {
      c(
        "table-metadata/data-structure/README.md",
        "other/x.csv"
      )
    },
    .mg_fetch_registry = function(urls) stop("fetcher should not be called"),
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- .mg_get_registry_table("database_structure"),
    "using bundled fallback"
  )
  expect_equal(result, .bundled_struct)
})

test_that("non-directory table returns bundled data without calling fetchers", {
  withr::local_options(marinegeo.utils.live_registry = TRUE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() stop("tree should not be called"),
    .mg_fetch_registry = function(urls) stop("fetcher should not be called"),
    .package = "marinegeo.utils"
  )

  result <- .mg_get_registry_table("data_index")

  expect_equal(result, .bundled_index)
})

test_that("offline mode returns bundled data for a directory-backed table", {
  withr::local_options(marinegeo.utils.live_registry = FALSE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_tree = function() stop("tree should not be called"),
    .mg_fetch_registry = function(urls) stop("fetcher should not be called"),
    .package = "marinegeo.utils"
  )

  result <- .mg_get_registry_table("database_structure")

  expect_equal(result, .bundled_struct)
})

# ---------------------------------------------------------------------------
# .mg_filter_csv_paths
# ---------------------------------------------------------------------------

test_that(".mg_filter_csv_paths keeps only direct-child CSVs of the directory", {
  paths <- c(
    "table-metadata/data-structure/a.csv",
    "table-metadata/data-structure/b.CSV",
    "table-metadata/data-structure/sub/c.csv",
    "table-metadata/data-structure/readme.md",
    "table-metadata/data-structure-extra/d.csv",
    "other/e.csv"
  )

  result <- .mg_filter_csv_paths(paths, "table-metadata/data-structure")

  expect_equal(
    result,
    c(
      "table-metadata/data-structure/a.csv",
      "table-metadata/data-structure/b.CSV"
    )
  )
})

# ---------------------------------------------------------------------------
# .mg_list_repo_tree (JSON parsing, no network)
# ---------------------------------------------------------------------------

test_that(".mg_list_repo_tree returns only blob paths", {
  fake <- list(
    truncated = FALSE,
    tree = data.frame(
      path = c("dir/a.csv", "dir", "dir/b.csv"),
      type = c("blob", "tree", "blob"),
      stringsAsFactors = FALSE
    )
  )
  local_mocked_bindings(fromJSON = function(...) fake, .package = "jsonlite")

  expect_equal(.mg_list_repo_tree(), c("dir/a.csv", "dir/b.csv"))
})

test_that(".mg_list_repo_tree warns when the tree listing is truncated", {
  fake <- list(
    truncated = TRUE,
    tree = data.frame(
      path = "dir/a.csv",
      type = "blob",
      stringsAsFactors = FALSE
    )
  )
  local_mocked_bindings(fromJSON = function(...) fake, .package = "jsonlite")

  expect_warning(.mg_list_repo_tree(), "truncated")
})
