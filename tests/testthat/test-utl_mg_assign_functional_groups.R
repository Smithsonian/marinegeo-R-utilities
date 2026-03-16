# Mock functional group enrollment tree (reuses structure from
# test-utl_mg_get_functional_groups.R):
#
#   FUNCTIONAL:1 (Biota)             depth=1
#     FUNCTIONAL:2 (Macrophytes)     depth=2
#       APHIA:143770 (Zosteraceae)   depth=3  members: APHIA:143770, APHIA:495077
#     FUNCTIONAL:3 (Fish)            depth=2
#       APHIA:111111 (Labridae)      depth=3  members: APHIA:111111
#
# Observation lookup:
#   "Zostera marina"  -> APHIA:495077  (Macrophytes, Biota)
#   "Labridae spp."   -> APHIA:111111  (Fish, Biota)
#   "Unknown sp."     -> APHIA:999999  (not in fg tree)

mock_tree <- list(
  "FUNCTIONAL:1" = list(
    name    = "Biota",
    members = character(0),
    children = list(
      "FUNCTIONAL:2" = list(
        name    = "Macrophytes",
        members = character(0),
        children = list(
          "APHIA:143770" = list(
            name    = "Zosteraceae",
            members = c("APHIA:143770", "APHIA:495077"),
            children = list()
          )
        )
      ),
      "FUNCTIONAL:3" = list(
        name    = "Fish",
        members = character(0),
        children = list(
          "APHIA:111111" = list(
            name    = "Labridae",
            members = c("APHIA:111111"),
            children = list()
          )
        )
      )
    )
  )
)

mock_obs_lookup <- data.frame(
  scientific_name = c("Zostera marina", "Labridae spp.", "Unknown sp."),
  scientific_id   = c("APHIA:495077",   "APHIA:111111", "APHIA:999999"),
  stringsAsFactors = FALSE
)

mock_metadata <- list(
  observation_lookup          = mock_obs_lookup,
  functional_group_enrollment = mock_tree
)

# ---------------------------------------------------------------------------
# Return type and length
# ---------------------------------------------------------------------------

test_that("returns a character vector of the same length as scientific_names", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- suppressMessages(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Fish"),
      scientific_names = c("Zostera marina", "Labridae spp.", NA)
    )
  )

  expect_type(result, "character")
  expect_length(result, 3)
})

# ---------------------------------------------------------------------------
# Happy path — single match
# ---------------------------------------------------------------------------

test_that("name belonging to exactly one candidate group returns that group", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- suppressMessages(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Fish"),
      scientific_names = c("Zostera marina", "Labridae spp.")
    )
  )

  expect_equal(result[1], "Macrophytes")
  expect_equal(result[2], "Fish")
})

test_that("repeated names in scientific_names are all assigned correctly", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- suppressMessages(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Fish"),
      scientific_names = c("Zostera marina", "Zostera marina", "Labridae spp.")
    )
  )

  expect_equal(result, c("Macrophytes", "Macrophytes", "Fish"))
})

# ---------------------------------------------------------------------------
# NA input
# ---------------------------------------------------------------------------

test_that("NA scientific names are returned as NA without a message", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_no_message(
    result <- utl_mg_assign_functional_groups(
      fg               = c("Macrophytes"),
      scientific_names = c(NA_character_, NA_character_)
    )
  )

  expect_equal(result, c(NA_character_, NA_character_))
})

test_that("NA values in a mixed vector are NA in output, valid names are assigned", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- suppressMessages(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Fish"),
      scientific_names = c(NA_character_, "Zostera marina")
    )
  )

  expect_true(is.na(result[1]))
  expect_equal(result[2], "Macrophytes")
})

# ---------------------------------------------------------------------------
# No match — name in lookup but not enrolled in any candidate group
# ---------------------------------------------------------------------------

test_that("name with no fg match returns NA with a message", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_assign_functional_groups(
      fg               = c("Macrophytes"),
      scientific_names = "Unknown sp."
    ),
    "did not match any of the provided functional groups"
  )

  expect_true(is.na(result))
})

test_that("no-match message lists the affected scientific name", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes"),
      scientific_names = "Unknown sp."
    ),
    "Unknown sp\\."
  )
})

# ---------------------------------------------------------------------------
# Multiple matches — name belongs to more than one candidate group
# ---------------------------------------------------------------------------

test_that("name matching multiple candidate groups returns NA with a message", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  # Zostera marina (APHIA:495077) belongs to both Macrophytes and Biota
  expect_message(
    result <- utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Biota"),
      scientific_names = "Zostera marina"
    ),
    "matched multiple functional groups"
  )

  expect_true(is.na(result))
})

test_that("multi-match message lists the affected name", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Biota"),
      scientific_names = "Zostera marina"
    ),
    "Zostera marina"
  )
})

# ---------------------------------------------------------------------------
# Name not in observation_lookup
# ---------------------------------------------------------------------------

test_that("name absent from observation_lookup returns NA with a message", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- utl_mg_assign_functional_groups(
      fg               = c("Macrophytes"),
      scientific_names = "Nonexistent species"
    ),
    "not found in observation_lookup"
  )

  expect_true(is.na(result))
})

test_that("unresolved-name message lists the affected name", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  expect_message(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes"),
      scientific_names = "Nonexistent species"
    ),
    "Nonexistent species"
  )
})

# ---------------------------------------------------------------------------
# Mixed valid / invalid in one call
# ---------------------------------------------------------------------------

test_that("mix of matched, unmatched, and unresolved names all handled in one call", {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .package = "marinegeo.utils"
  )

  result <- suppressMessages(
    utl_mg_assign_functional_groups(
      fg               = c("Macrophytes", "Fish"),
      scientific_names = c("Zostera marina", "Unknown sp.", "Not in lookup")
    )
  )

  expect_equal(result[1], "Macrophytes")
  expect_true(is.na(result[2]))   # in lookup, but not enrolled in fg tree
  expect_true(is.na(result[3]))   # not in lookup at all
})

# ---------------------------------------------------------------------------
# All-NA input
# ---------------------------------------------------------------------------

test_that("all-NA scientific_names returns all-NA character vector with no message", {
  expect_no_message(
    result <- utl_mg_assign_functional_groups(
      fg               = c("Macrophytes"),
      scientific_names = c(NA_character_, NA_character_, NA_character_)
    )
  )

  expect_equal(result, rep(NA_character_, 3))
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-character fg stops with an informative error", {
  expect_error(
    utl_mg_assign_functional_groups(fg = 1:3, scientific_names = "Zostera marina"),
    "`fg` must be a non-empty character vector"
  )
})

test_that("empty fg vector stops with an informative error", {
  expect_error(
    utl_mg_assign_functional_groups(fg = character(0), scientific_names = "Zostera marina"),
    "`fg` must be a non-empty character vector"
  )
})

test_that("non-character scientific_names stops with an informative error", {
  expect_error(
    utl_mg_assign_functional_groups(fg = "Macrophytes", scientific_names = 123),
    "`scientific_names` must be a character vector"
  )
})
