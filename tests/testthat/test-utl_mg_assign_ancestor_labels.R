# Mock functional_group_lookup edge-list.
#
# Each row: from=child display name, to=parent display name, scientific_id=child's sci_id.
# Root ("Life") has no scientific_id and never appears in `from`, matching the
# real tables — so the root is never matchable.
#
#   Life (root, no row of its own)
#     Biota (FUNCTIONAL:1)                type=primary
#       Macrophytes (FUNCTIONAL:2)        type=primary          <- nested primary
#         Zosteraceae (APHIA:143770)      rank=Family
#           Zostera (APHIA:143771)        rank=Genus, code=zos
#             Zostera marina (APHIA:495077) rank=Species
#       Algae (FUNCTIONAL:3)              rank=Phylum (Division)
#         Ulva (APHIA:222222)             rank=Genus, code=ulv
#       Fish (FUNCTIONAL:4)
#         Labridae (APHIA:111111)         rank=Family
#     Detritus (FUNCTIONAL:5)                                   <- no primary ancestor
#
# Observation lookup:
#   "Zostera marina"          -> APHIA:495077
#   "Zostera"                 -> APHIA:143771
#   "Labridae"                -> APHIA:111111  ["Labridae spp." strips to this]
#   "Ulva"                    -> APHIA:222222
#   "detritus"                -> FUNCTIONAL:5  (in tree, but no primary ancestor)
#   "Unknown"                 -> APHIA:999999  (not in the fg tree at all)
#   "unidentified macroalgae" -> FUNCTIONAL:2  (is Macrophytes)

mock_fg_lookup <- data.frame(
  from = c(
    "Biota",
    "Macrophytes",
    "Zosteraceae",
    "Zostera",
    "Zostera marina",
    "Algae",
    "Ulva",
    "Fish",
    "Labridae",
    "Detritus"
  ),
  to = c(
    "Life",
    "Biota",
    "Macrophytes",
    "Zosteraceae",
    "Zostera",
    "Biota",
    "Algae",
    "Biota",
    "Fish",
    "Life"
  ),
  scientific_id = c(
    "FUNCTIONAL:1",
    "FUNCTIONAL:2",
    "APHIA:143770",
    "APHIA:143771",
    "APHIA:495077",
    "FUNCTIONAL:3",
    "APHIA:222222",
    "FUNCTIONAL:4",
    "APHIA:111111",
    "FUNCTIONAL:5"
  ),
  type = c(
    "primary",
    "primary",
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  ),
  code = c(NA, NA, NA, "zos", NA, NA, "ulv", NA, NA, NA),
  rank = c(
    NA,
    NA,
    "Family",
    "Genus",
    "Species",
    "Phylum (Division)",
    "Genus",
    NA,
    "Family",
    NA
  ),
  tree_name = rep("test_tree", 10),
  stringsAsFactors = FALSE
)

mock_obs_lookup <- data.frame(
  scientific_name = c(
    "Zostera marina",
    "Zostera",
    "Labridae",
    "Ulva",
    "detritus",
    "Unknown",
    "unidentified macroalgae"
  ),
  scientific_id = c(
    "APHIA:495077",
    "APHIA:143771",
    "APHIA:111111",
    "APHIA:222222",
    "FUNCTIONAL:5",
    "APHIA:999999",
    "FUNCTIONAL:2"
  ),
  stringsAsFactors = FALSE
)

mock_metadata <- list(
  observation_lookup = mock_obs_lookup,
  functional_group_lookup = mock_fg_lookup
)

use_mocks <- function(env = parent.frame()) {
  local_mocked_bindings(
    marinegeo_metadata = mock_metadata,
    .mg_fetch_registry = function(url) mock_obs_lookup,
    .package = "marinegeo.utils",
    .env = env
  )
}

# ---------------------------------------------------------------------------
# Nearest-ancestor semantics
# ---------------------------------------------------------------------------

test_that("the nearest matching ancestor wins when several ancestors match", {
  use_mocks()

  # Both Macrophytes and Biota are type=primary; Macrophytes is nearer.
  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Zostera marina",
      type = "primary"
    )
  )

  expect_equal(result, "Macrophytes")
})

test_that("a rank criterion returns the ancestor at that rank", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Zostera marina",
      rank = "Family"
    )
  )

  expect_equal(result, "Zosteraceae")
})

test_that("the queried node itself is eligible to match", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Zostera marina",
      rank = "Species"
    )
  )

  expect_equal(result, "Zostera marina")
})

test_that("the root node is never matched", {
  use_mocks()

  # "Life" is the root and has no row, so no criterion can select it.
  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "detritus",
      type = "primary"
    )
  )

  expect_true(is.na(result))
})

# ---------------------------------------------------------------------------
# Multiple criteria are ANDed
# ---------------------------------------------------------------------------

test_that("multiple criteria must be satisfied by the same ancestor", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Zostera marina",
      rank = "Genus",
      code = "zos"
    )
  )

  expect_equal(result, "Zostera")
})

test_that("criteria split across two different nodes match neither", {
  use_mocks()

  # Zosteraceae has rank=Family but no code; Zostera has code=zos but rank=Genus.
  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Zostera marina",
      rank = "Family",
      code = "zos"
    )
  )

  expect_true(is.na(result))
})

# ---------------------------------------------------------------------------
# Rank normalization
# ---------------------------------------------------------------------------

test_that("rank = \"Phylum\" matches a \"Phylum (Division)\" node", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Ulva",
      rank = "Phylum"
    )
  )

  expect_equal(result, "Algae")
})

# ---------------------------------------------------------------------------
# Return type, length, and NA handling
# ---------------------------------------------------------------------------

test_that("returns a character vector of the same length as scientific_names", {
  use_mocks()

  result <- suppressWarnings(suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = c("Zostera marina", "Labridae spp.", NA),
      type = "primary"
    )
  ))

  expect_type(result, "character")
  expect_length(result, 3)
})

test_that("NA scientific names are returned as NA without a message", {
  use_mocks()

  expect_no_message(
    result <- utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = c(NA_character_, NA_character_),
      type = "primary"
    )
  )

  expect_equal(result, c(NA_character_, NA_character_))
})

test_that("NA values in a mixed vector are NA, valid names are assigned", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = c("Zostera marina", NA, "Labridae"),
      type = "primary"
    )
  )

  expect_equal(result, c("Macrophytes", NA, "Biota"))
})

test_that("repeated names are all assigned correctly", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = c("Zostera marina", "Ulva", "Zostera marina"),
      type = "primary"
    )
  )

  expect_equal(result, c("Macrophytes", "Biota", "Macrophytes"))
})

test_that("trailing rank abbreviations are stripped before lookup", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Labridae spp.",
      rank = "Family"
    )
  )

  expect_equal(result, "Labridae")
})

# ---------------------------------------------------------------------------
# Unmatched names
# ---------------------------------------------------------------------------

test_that("name absent from observation_lookup returns NA with a warning", {
  use_mocks()

  expect_warning(
    result <- suppressMessages(
      utl_mg_assign_ancestor_labels(
        fg_tree = "test_tree",
        scientific_names = "Nonesuch",
        type = "primary"
      )
    ),
    "could not be matched in `observation_lookup`"
  )

  expect_true(is.na(result))
})

test_that("id that resolves but is absent from the tree returns NA with a message", {
  use_mocks()

  expect_message(
    result <- utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "Unknown",
      type = "primary"
    ),
    "Unknown"
  )

  expect_true(is.na(result))
})

test_that("no-match message names the affected scientific name and criteria", {
  use_mocks()

  expect_message(
    result <- utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "detritus",
      type = "primary"
    ),
    'type = "primary"'
  )

  expect_true(is.na(result))
})

test_that("mix of matched, unmatched, and unresolved names handled in one call", {
  use_mocks()

  result <- suppressWarnings(suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = c("Zostera marina", "detritus", "Nonesuch", NA),
      type = "primary"
    )
  ))

  expect_equal(result, c("Macrophytes", NA, NA, NA))
})

test_that("a FUNCTIONAL: name that is itself a primary group is assigned itself", {
  use_mocks()

  result <- suppressMessages(
    utl_mg_assign_ancestor_labels(
      fg_tree = "test_tree",
      scientific_names = "unidentified macroalgae",
      type = "primary"
    )
  )

  expect_equal(result, "Macrophytes")
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("no criteria stops with an informative error", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels("test_tree", "Zostera marina"),
    "At least one `column = value` criterion"
  )
})

test_that("unnamed criteria stop with an informative error", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels("test_tree", "Zostera marina", "primary"),
    "must be named"
  )
})

test_that("an unknown criteria column stops and lists the matchable columns", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels(
      "test_tree",
      "Zostera marina",
      typ = "primary"
    ),
    "Unknown criteria column"
  )
})

test_that("reserved structural columns are rejected as criteria", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels(
      "test_tree",
      "Zostera marina",
      from = "Biota"
    ),
    "Unknown criteria column"
  )
  expect_error(
    utl_mg_assign_ancestor_labels("test_tree", "Zostera marina", to = "Biota"),
    "Unknown criteria column"
  )
  expect_error(
    utl_mg_assign_ancestor_labels(
      "test_tree",
      "Zostera marina",
      tree_name = "test_tree"
    ),
    "Unknown criteria column"
  )
})

test_that("a non-scalar or NA criterion value stops with an informative error", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels(
      "test_tree",
      "Zostera marina",
      type = c("primary", "secondary")
    ),
    "length-1, non-NA atomic value"
  )
  expect_error(
    utl_mg_assign_ancestor_labels("test_tree", "Zostera marina", type = NA),
    "length-1, non-NA atomic value"
  )
})

test_that("non-character scientific_names stops with an informative error", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels("test_tree", 12345, type = "primary"),
    "`scientific_names` must be a character vector"
  )
})

test_that("a non-scalar fg_tree stops with an informative error", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels(
      c("test_tree", "other"),
      "Zostera marina",
      type = "primary"
    ),
    "`fg_tree` must be a single non-NA character value"
  )
})

test_that("an unknown tree stops and lists the available trees", {
  use_mocks()

  expect_error(
    utl_mg_assign_ancestor_labels(
      "no_such_tree",
      "Zostera marina",
      type = "primary"
    ),
    "Available trees: test_tree"
  )
})
