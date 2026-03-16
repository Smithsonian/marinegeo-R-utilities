# Mock functional group enrollment nested tree.
# Members are stored only at the deepest enrollment node — not propagated up.
#
#   FUNCTIONAL:1 (Biota)           depth=1 — members: (empty)
#     FUNCTIONAL:2 (Macrophytes)   depth=2 — members: (empty)
#       APHIA:143770 (Zosteraceae) depth=3 — members: APHIA:143770, APHIA:495077
#     FUNCTIONAL:3 (Fish)          depth=2 — members: (empty)
#       APHIA:111111 (Labridae)    depth=3 — members: APHIA:111111
#
# Expected results (ancestor nodes, queried ID excluded):
#   "APHIA:495077"  -> FUNCTIONAL:1(d=1), FUNCTIONAL:2(d=2), APHIA:143770(d=3)
#   "APHIA:143770"  -> FUNCTIONAL:1(d=1), FUNCTIONAL:2(d=2)
#   "APHIA:111111"  -> FUNCTIONAL:1(d=1), FUNCTIONAL:3(d=2)
#   "FUNCTIONAL:2"  -> FUNCTIONAL:1(d=1)
#   "FUNCTIONAL:1"  -> (empty — root has no ancestors)

mock_tree <- list(
  "FUNCTIONAL:1" = list(
    name = "Biota",
    members = character(0),
    children = list(
      "FUNCTIONAL:2" = list(
        name = "Macrophytes",
        members = character(0),
        children = list(
          "APHIA:143770" = list(
            name = "Zosteraceae",
            members = c("APHIA:143770", "APHIA:495077"),
            children = list()
          )
        )
      ),
      "FUNCTIONAL:3" = list(
        name = "Fish",
        members = character(0),
        children = list(
          "APHIA:111111" = list(
            name = "Labridae",
            members = c("APHIA:111111"),
            children = list()
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Return type and column structure
# ---------------------------------------------------------------------------

test_that("result is a data frame with expected columns", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("scientific_id", "parent_scientific_id", "parent_name", "depth") %in%
      colnames(result)
  ))
})

test_that("depth column is integer", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")
  expect_type(result$depth, "integer")
})

# ---------------------------------------------------------------------------
# APHIA: species input
# ---------------------------------------------------------------------------

test_that("APHIA: species returns all ancestor nodes including APHIA: anchor", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")

  expect_setequal(
    result$parent_scientific_id,
    c("FUNCTIONAL:1", "FUNCTIONAL:2", "APHIA:143770")
  )
  expect_true(all(result$scientific_id == "APHIA:495077"))
})

test_that("queried APHIA: species does not appear in its own parent_scientific_id column", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")
  expect_false("APHIA:495077" %in% result$parent_scientific_id)
})

test_that("depth reflects position in fg hierarchy counting all fg nodes (root = 1)", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")
  result <- result[order(result$depth), ]

  expect_equal(result$parent_scientific_id[1], "FUNCTIONAL:1")
  expect_equal(result$depth[1], 1L)
  expect_equal(result$parent_scientific_id[2], "FUNCTIONAL:2")
  expect_equal(result$depth[2], 2L)
  expect_equal(result$parent_scientific_id[3], "APHIA:143770")
  expect_equal(result$depth[3], 3L)
})

test_that("APHIA: anchor node returns its ancestor FUNCTIONAL: nodes only", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:143770")

  expect_setequal(
    result$parent_scientific_id,
    c("FUNCTIONAL:1", "FUNCTIONAL:2")
  )
  expect_false("APHIA:143770" %in% result$parent_scientific_id)
})

test_that("sibling functional groups at the same level get the same depth", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  macro <- utl_mg_get_functional_groups("APHIA:495077")
  fish <- utl_mg_get_functional_groups("APHIA:111111")

  macro_depth <- macro$depth[macro$parent_scientific_id == "FUNCTIONAL:2"]
  fish_depth <- fish$depth[fish$parent_scientific_id == "FUNCTIONAL:3"]
  expect_equal(macro_depth, fish_depth)
})

# ---------------------------------------------------------------------------
# FUNCTIONAL: group input
# ---------------------------------------------------------------------------

test_that("FUNCTIONAL: input returns ancestor FUNCTIONAL: nodes", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:2")

  expect_setequal(result$parent_scientific_id, "FUNCTIONAL:1")
  expect_equal(result$parent_name, "Biota")
})

test_that("queried FUNCTIONAL: node does not appear in its own parent_scientific_id column", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:2")
  expect_false("FUNCTIONAL:2" %in% result$parent_scientific_id)
})

test_that("root FUNCTIONAL: node returns zero rows (no ancestors)", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:1")
  expect_equal(nrow(result), 0)
})

# ---------------------------------------------------------------------------
# Multiple IDs
# ---------------------------------------------------------------------------

test_that("multiple IDs return one row per (scientific_id, ancestor) pair", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(c("APHIA:495077", "APHIA:111111"))

  expect_s3_class(result, "data.frame")
  # APHIA:495077 -> 3 rows, APHIA:111111 -> 2 rows (FUNCTIONAL:1 + FUNCTIONAL:3 only,
  # APHIA:111111 is not in its own results)
  expect_equal(nrow(result[result$scientific_id == "APHIA:495077", ]), 3)
  expect_equal(nrow(result[result$scientific_id == "APHIA:111111", ]), 2)
})

# ---------------------------------------------------------------------------
# Unknown / unmatched IDs
# ---------------------------------------------------------------------------

test_that("unknown single ID returns zero-row data frame without error", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:99999999")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("scientific_id", "parent_scientific_id", "parent_name", "depth") %in%
      colnames(result)
  ))
})

test_that("mix of known and unknown IDs: only known ID produces rows", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(c("APHIA:495077", "APHIA:99999999"))
  expect_true(all(result$scientific_id == "APHIA:495077"))
})

# ---------------------------------------------------------------------------
# Edge cases: empty input, NA input
# ---------------------------------------------------------------------------

test_that("empty character vector returns zero-row data frame with expected columns", {
  result <- utl_mg_get_functional_groups(character(0))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("scientific_id", "parent_scientific_id", "parent_name", "depth") %in%
      colnames(result)
  ))
})

test_that("all-NA input removes NAs with message and returns zero-row data frame", {
  expect_message(
    result <- utl_mg_get_functional_groups(c(NA_character_, NA_character_)),
    "2 NA value\\(s\\) removed"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("NAs mixed with valid ID: NAs removed with message, valid ID processed", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_tree),
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_get_functional_groups(c(NA_character_, "APHIA:495077")),
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
    utl_mg_get_functional_groups(12345),
    "`scientific_ids` must be a character vector"
  )
})

test_that("logical input stops with an informative error", {
  expect_error(
    utl_mg_get_functional_groups(TRUE),
    "`scientific_ids` must be a character vector"
  )
})
