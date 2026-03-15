# Mock enrollment table covering several scenarios:
#
#   Functional hierarchy (functional_group_lookup):
#     FUNCTIONAL:1 (Biota)
#       FUNCTIONAL:2 (Macrophytes)
#         APHIA:143770 (Zosteraceae, enroll_all_lower_ranks = TRUE)
#     FUNCTIONAL:3 (Fish)
#       APHIA:111111  (Labridae, direct — enroll_all_lower_ranks = FALSE)
#
#   Taxonomic descendants (from taxonomic_lookup BFS at build time):
#     APHIA:143770 (Zosteraceae) -> APHIA:495077 (Zostera marina)
#
# Resulting enrollment table rows:
#   FUNCTIONAL:1   direct               FUNCTIONAL:1  "Biota"
#   FUNCTIONAL:2   direct               FUNCTIONAL:2  "Biota > Macrophytes"
#   APHIA:143770   direct               FUNCTIONAL:2  "Biota > Macrophytes > Zosteraceae"
#   APHIA:495077   enroll_all_lower_ranks FUNCTIONAL:2 "Biota > Macrophytes > Zosteraceae"
#   FUNCTIONAL:3   direct               FUNCTIONAL:3  "Biota > Fish"  <- NOTE: fish is under biota in mock
#   APHIA:111111   direct               FUNCTIONAL:3  "Biota > Fish > Labridae"

mock_enrollment <- tibble::tribble(
  ~scientific_id,   ~functional_group_id, ~functional_group_name, ~lineage,                                    ~enrolled_via,              ~anchor_id,
  "FUNCTIONAL:1",   "FUNCTIONAL:1",       "Biota",                "Biota",                                     "direct",                   "FUNCTIONAL:1",
  "FUNCTIONAL:2",   "FUNCTIONAL:2",       "Macrophytes",          "Biota > Macrophytes",                       "direct",                   "FUNCTIONAL:2",
  "APHIA:143770",   "FUNCTIONAL:2",       "Macrophytes",          "Biota > Macrophytes > Zosteraceae",         "direct",                   "APHIA:143770",
  "APHIA:495077",   "FUNCTIONAL:2",       "Macrophytes",          "Biota > Macrophytes > Zosteraceae",         "enroll_all_lower_ranks",   "APHIA:143770",
  "FUNCTIONAL:3",   "FUNCTIONAL:3",       "Fish",                 "Biota > Fish",                              "direct",                   "FUNCTIONAL:3",
  "APHIA:111111",   "FUNCTIONAL:3",       "Fish",                 "Biota > Fish > Labridae",                   "direct",                   "APHIA:111111"
)

# ---------------------------------------------------------------------------
# Happy-path tests
# ---------------------------------------------------------------------------

test_that("single APHIA ID enrolled via enroll_all returns correct row", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id,         "APHIA:495077")
  expect_equal(result$functional_group_id,   "FUNCTIONAL:2")
  expect_equal(result$functional_group_name, "Macrophytes")
  expect_equal(result$enrolled_via,          "enroll_all_lower_ranks")
  expect_equal(result$anchor_id,             "APHIA:143770")
  expect_true(grepl("Zosteraceae", result$lineage))
})

test_that("FUNCTIONAL ID returns its own direct enrollment row", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("FUNCTIONAL:2")

  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id,       "FUNCTIONAL:2")
  expect_equal(result$enrolled_via,        "direct")
  expect_equal(result$anchor_id,           "FUNCTIONAL:2")
  expect_equal(result$lineage,             "Biota > Macrophytes")
})

test_that("anchor APHIA node returns direct enrollment row", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:143770")

  expect_equal(nrow(result), 1)
  expect_equal(result$enrolled_via, "direct")
  expect_equal(result$anchor_id,    "APHIA:143770")
})

test_that("batch lookup returns one row per matched ID", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(
    c("APHIA:495077", "APHIA:111111", "FUNCTIONAL:1")
  )

  expect_equal(nrow(result), 3)
  expect_setequal(
    result$scientific_id,
    c("APHIA:495077", "APHIA:111111", "FUNCTIONAL:1")
  )
})

test_that("result has all expected columns in any order", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")
  expected_cols <- c(
    "scientific_id", "functional_group_id", "functional_group_name",
    "lineage", "enrolled_via", "anchor_id"
  )

  expect_true(all(expected_cols %in% colnames(result)))
})

# ---------------------------------------------------------------------------
# Multi-group membership
# ---------------------------------------------------------------------------

test_that("species with multiple functional group memberships returns multiple rows", {
  # Add a second enrollment for APHIA:495077 under FUNCTIONAL:3 as well
  multi_enrollment <- dplyr::bind_rows(
    mock_enrollment,
    tibble::tibble(
      scientific_id         = "APHIA:495077",
      functional_group_id   = "FUNCTIONAL:3",
      functional_group_name = "Fish",
      lineage               = "Biota > Fish > Labridae",
      enrolled_via          = "enroll_all_lower_ranks",
      anchor_id             = "APHIA:111111"
    )
  )

  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = multi_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:495077")

  expect_equal(nrow(result), 2)
  expect_setequal(result$functional_group_id, c("FUNCTIONAL:2", "FUNCTIONAL:3"))
})

# ---------------------------------------------------------------------------
# Unknown / unmatched IDs
# ---------------------------------------------------------------------------

test_that("unknown ID returns zero rows without error", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups("APHIA:99999999")

  expect_equal(nrow(result), 0)
  expect_true("scientific_id" %in% colnames(result))
})

test_that("mix of known and unknown IDs: only known IDs appear in result", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  result <- utl_mg_get_functional_groups(
    c("APHIA:495077", "APHIA:99999999")
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id, "APHIA:495077")
})

# ---------------------------------------------------------------------------
# Edge cases: empty input, NA input
# ---------------------------------------------------------------------------

test_that("empty character vector returns zero-row data frame with expected columns", {
  result <- utl_mg_get_functional_groups(character(0))

  expect_equal(nrow(result), 0)
  expect_true(all(
    c("scientific_id", "functional_group_id", "functional_group_name",
      "lineage", "enrolled_via", "anchor_id") %in% colnames(result)
  ))
})

test_that("all-NA input removes NAs with message and returns zero-row data frame", {
  expect_message(
    result <- utl_mg_get_functional_groups(c(NA_character_, NA_character_)),
    "2 NA value\\(s\\) removed"
  )
  expect_equal(nrow(result), 0)
})

test_that("NAs mixed with valid IDs: NAs removed with message, valid IDs processed", {
  local_mocked_bindings(
    marinegeo_metadata = list(functional_group_enrollment = mock_enrollment),
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_get_functional_groups(c(NA_character_, "APHIA:495077")),
    "1 NA value\\(s\\) removed"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id, "APHIA:495077")
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
