# Mock functional_group_lookup edge-list (new format).
#
# Each row: from=child display name, to=parent display name, scientific_id=child's sci_id.
# Root ("Life") has no scientific_id and never appears in `from`.
#
# Linear tree (no siblings):
#
#   Life (root, no scientific_id)
#     Biota (FUNCTIONAL:1)
#       Macrophytes (FUNCTIONAL:2)
#         Zosteraceae (APHIA:143770) <- anchor node
#           Zostera marina (APHIA:495077) <- enrolled species
#
# Expected results:
#   "APHIA:495077"  -> group_id: FUNCTIONAL:1 (Biota), FUNCTIONAL:2 (Macrophytes)
#   "APHIA:143770"  -> group_id: FUNCTIONAL:1 (Biota), FUNCTIONAL:2 (Macrophytes)
#   "FUNCTIONAL:2"  -> group_id: FUNCTIONAL:1 (Biota), FUNCTIONAL:2 (self, Macrophytes)
#   "FUNCTIONAL:1"  -> group_id: FUNCTIONAL:1 (self, Biota) — 1 row

mock_fg_lookup <- data.frame(
  from         = c("Biota",       "Macrophytes",  "Zosteraceae",  "Zostera marina"),
  to           = c("Life",        "Biota",        "Macrophytes",  "Zosteraceae"),
  scientific_id = c("FUNCTIONAL:1", "FUNCTIONAL:2", "APHIA:143770", "APHIA:495077"),
  tree_name    = rep("test_tree", 4),
  stringsAsFactors = FALSE
)

mock_metadata <- list(functional_group_lookup = mock_fg_lookup)

# ---------------------------------------------------------------------------
# Return type and column structure
# ---------------------------------------------------------------------------

test_that("result is a data frame with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077", "test_tree")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("scientific_id", "group_id", "group_name") %in% colnames(result)
  ))
})

test_that("group_id and group_name columns are character", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077", "test_tree")
  expect_type(result$group_id, "character")
  expect_type(result$group_name, "character")
})

# ---------------------------------------------------------------------------
# APHIA: species input
# ---------------------------------------------------------------------------

test_that("APHIA: species returns all FUNCTIONAL: ancestor nodes", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077", "test_tree")

  expect_setequal(result$group_id, c("FUNCTIONAL:1", "FUNCTIONAL:2"))
  expect_setequal(result$group_name, c("Biota", "Macrophytes"))
  expect_true(all(result$scientific_id == "APHIA:495077"))
})

test_that("queried APHIA: species does not appear in group_id column", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077", "test_tree")
  expect_false("APHIA:495077" %in% result$group_id)
})

test_that("APHIA: anchor node returns only its FUNCTIONAL: ancestors", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:143770", "test_tree")

  expect_setequal(result$group_id, c("FUNCTIONAL:1", "FUNCTIONAL:2"))
  expect_false("APHIA:143770" %in% result$group_id)
})

test_that("APHIA: anchor node result contains only FUNCTIONAL: group_id values", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:143770", "test_tree")
  expect_true(all(startsWith(result$group_id, "FUNCTIONAL:")))
})

# ---------------------------------------------------------------------------
# FUNCTIONAL: group input
# ---------------------------------------------------------------------------

test_that("FUNCTIONAL: input returns self and all ancestor FUNCTIONAL: nodes", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:2", "test_tree")

  expect_setequal(result$group_id, c("FUNCTIONAL:1", "FUNCTIONAL:2"))
  expect_setequal(result$group_name, c("Biota", "Macrophytes"))
})

test_that("queried FUNCTIONAL: node appears in its own group_id column (self-row)", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:2", "test_tree")
  expect_true("FUNCTIONAL:2" %in% result$group_id)
})

test_that("self-row for queried FUNCTIONAL: node has correct group_name", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:2", "test_tree")
  self_row <- result[result$group_id == "FUNCTIONAL:2", ]

  expect_equal(nrow(self_row), 1L)
  expect_equal(self_row$group_name, "Macrophytes")
})

test_that("root FUNCTIONAL: node returns one row for itself", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:1", "test_tree")

  expect_equal(nrow(result), 1L)
  expect_equal(result$group_id, "FUNCTIONAL:1")
  expect_equal(result$group_name, "Biota")
})

# ---------------------------------------------------------------------------
# Multiple IDs
# ---------------------------------------------------------------------------

test_that("multiple IDs return rows for each scientific_id", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(
    c("APHIA:495077", "FUNCTIONAL:2"),
    "test_tree"
  )

  expect_s3_class(result, "data.frame")
  expect_true("APHIA:495077" %in% result$scientific_id)
  expect_true("FUNCTIONAL:2" %in% result$scientific_id)
})

test_that("each scientific_id receives the correct group memberships", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(
    c("APHIA:495077", "FUNCTIONAL:1"),
    "test_tree"
  )

  aphia_groups <- result$group_id[result$scientific_id == "APHIA:495077"]
  fg1_groups   <- result$group_id[result$scientific_id == "FUNCTIONAL:1"]

  expect_setequal(aphia_groups, c("FUNCTIONAL:1", "FUNCTIONAL:2"))
  expect_equal(fg1_groups, "FUNCTIONAL:1")
})

# ---------------------------------------------------------------------------
# Unknown / unmatched IDs
# ---------------------------------------------------------------------------

test_that("unknown single ID returns zero-row data frame without error", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:99999999", "test_tree")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("scientific_id", "group_id", "group_name") %in% colnames(result)
  ))
})

test_that("mix of known and unknown IDs: only known ID produces rows", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(
    c("APHIA:495077", "APHIA:99999999"),
    "test_tree"
  )
  expect_true(all(result$scientific_id == "APHIA:495077"))
})

# ---------------------------------------------------------------------------
# Edge cases: empty input, NA input
# ---------------------------------------------------------------------------

test_that("empty character vector returns zero-row data frame with expected columns", {
  result <- utl_mg_get_functional_groups(character(0), "test_tree")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("scientific_id", "group_id", "group_name") %in% colnames(result)
  ))
})

test_that("all-NA input removes NAs with message and returns zero-row data frame", {
  expect_message(
    result <- utl_mg_get_functional_groups(
      c(NA_character_, NA_character_),
      "test_tree"
    ),
    "2 NA value\\(s\\) removed"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("NAs mixed with valid ID: NAs removed with message, valid ID processed", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_get_functional_groups(
      c(NA_character_, "APHIA:495077"),
      "test_tree"
    ),
    "1 NA value\\(s\\) removed"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$scientific_id == "APHIA:495077"))
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-character input stops with an informative error", {
  expect_error(
    utl_mg_get_functional_groups(12345, "test_tree"),
    "`scientific_ids` must be a character vector"
  )
})

test_that("logical input stops with an informative error", {
  expect_error(
    utl_mg_get_functional_groups(TRUE, "test_tree"),
    "`scientific_ids` must be a character vector"
  )
})
