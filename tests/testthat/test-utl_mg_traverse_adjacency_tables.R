# Minimal adjacency table covering two lineages, matching the real sysdata
# structure:
#   - id:            numeric (raw AphiaID)
#   - scientific_id: "APHIA:X" for EVERY row (not just species)
#   - parent_id:     "APHIA:X" of the parent row, or NA for roots
#
# Lineages:
#   Animal: Animalia > Chordata > Actinopterygii > Perciformes > Labridae >
#           Thalassoma > Thalassoma bifasciatum / T. lunare
#   Plant:  Plantae > Tracheophyta (Phylum (Division)) > Liliopsida >
#           Alismatales > Zosteraceae > Zostera > Zostera marina
#
# Having both lineages ensures the `Phylum (Division)` → `Phylum` case_when
# produces both columns after unnesting.

mock_taxonomic_lookup <- tibble::tribble(
  ~id,      ~scientific_id,     ~parent_id,          ~rank,               ~name,
  # Animal lineage
  1L,       "APHIA:1",          NA,                  "Kingdom",           "Animalia",
  2L,       "APHIA:2",          "APHIA:1",           "Phylum",            "Chordata",
  3L,       "APHIA:3",          "APHIA:2",           "Class",             "Actinopterygii",
  4L,       "APHIA:4",          "APHIA:3",           "Order",             "Perciformes",
  5L,       "APHIA:5",          "APHIA:4",           "Family",            "Labridae",
  6L,       "APHIA:6",          "APHIA:5",           "Genus",             "Thalassoma",
  125476L,  "APHIA:125476",     "APHIA:6",           "Species",           "Thalassoma bifasciatum",
  125477L,  "APHIA:125477",     "APHIA:6",           "Species",           "Thalassoma lunare",
  # Plant lineage
  10L,      "APHIA:10",         NA,                  "Kingdom",           "Plantae",
  11L,      "APHIA:11",         "APHIA:10",          "Phylum (Division)", "Tracheophyta",
  12L,      "APHIA:12",         "APHIA:11",          "Class",             "Liliopsida",
  13L,      "APHIA:13",         "APHIA:12",          "Order",             "Alismatales",
  14L,      "APHIA:14",         "APHIA:13",          "Family",            "Zosteraceae",
  15L,      "APHIA:15",         "APHIA:14",          "Genus",             "Zostera",
  374534L,  "APHIA:374534",     "APHIA:15",          "Species",           "Zostera marina"
)

# ---------------------------------------------------------------------------
# .get_parent_rank() unit tests
# ---------------------------------------------------------------------------

test_that(".get_parent_rank returns all ancestor ranks up to root", {
  # node_id is the numeric id of Thalassoma bifasciatum
  result <- .get_parent_rank(125476L, mock_taxonomic_lookup)

  expect_type(result, "list")
  expect_equal(result[["Kingdom"]], "Animalia")
  expect_equal(result[["Phylum"]],  "Chordata")
  expect_equal(result[["Class"]],   "Actinopterygii")
  expect_equal(result[["Order"]],   "Perciformes")
  expect_equal(result[["Family"]],  "Labridae")
  expect_equal(result[["Genus"]],   "Thalassoma")
  expect_equal(result[["Species"]], "Thalassoma bifasciatum")
})

test_that(".get_parent_rank returns single-element list for a root node", {
  result <- .get_parent_rank(1L, mock_taxonomic_lookup)

  expect_length(result, 1)
  expect_equal(result[["Kingdom"]], "Animalia")
})

test_that(".get_parent_rank returns empty list when node_id is not found", {
  result <- .get_parent_rank(99999L, mock_taxonomic_lookup)

  expect_type(result, "list")
  expect_length(result, 0)
})

# ---------------------------------------------------------------------------
# .get_taxonomic_classifications() tests
# ---------------------------------------------------------------------------

test_that("happy path: returns correct columns and values for a single ID", {
  result <- .get_taxonomic_classifications("APHIA:125476", mock_taxonomic_lookup)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("scientific_id", "rank") %in% colnames(result)))
  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id, "APHIA:125476")
  expect_equal(result$rank, "Species")
  expect_equal(result$Kingdom, "Animalia")
  expect_equal(result$Phylum,  "Chordata")
  expect_equal(result$Species, "Thalassoma bifasciatum")
})

test_that("scientific_id and rank are the first two columns", {
  result <- .get_taxonomic_classifications("APHIA:125476", mock_taxonomic_lookup)

  expect_equal(colnames(result)[1], "scientific_id")
  expect_equal(colnames(result)[2], "rank")
})

test_that("multiple IDs return one row each", {
  result <- .get_taxonomic_classifications(
    c("APHIA:125476", "APHIA:125477", "APHIA:374534"),
    mock_taxonomic_lookup
  )

  expect_equal(nrow(result), 3)
  expect_setequal(result$scientific_id, c("APHIA:125476", "APHIA:125477", "APHIA:374534"))
})

test_that("Phylum (Division) is mapped to Phylum", {
  result <- .get_taxonomic_classifications(
    c("APHIA:125476", "APHIA:374534"),
    mock_taxonomic_lookup
  )

  seagrass_row <- result[result$scientific_id == "APHIA:374534", ]
  expect_equal(seagrass_row$Kingdom, "Plantae")
  expect_equal(seagrass_row$Phylum,  "Tracheophyta")
  expect_false("Phylum (Division)" %in% colnames(result))
})

test_that("ID not present in taxonomic_lookup is absent from output", {
  result <- .get_taxonomic_classifications(
    c("APHIA:125476", "APHIA:UNKNOWN"),
    mock_taxonomic_lookup
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$scientific_id, "APHIA:125476")
})

test_that("empty character vector returns zero-row data frame with expected columns", {
  result <- .get_taxonomic_classifications(character(0), mock_taxonomic_lookup)

  expect_equal(nrow(result), 0)
  expect_true(all(
    c("scientific_id", "rank", "Kingdom", "Phylum", "Class",
      "Order", "Family", "Genus", "Species") %in% colnames(result)
  ))
})

test_that("all-NA input returns zero-row data frame with a message", {
  expect_message(
    result <- .get_taxonomic_classifications(
      c(NA_character_, NA_character_),
      mock_taxonomic_lookup
    ),
    "2 NA value\\(s\\) removed"
  )
  expect_equal(nrow(result), 0)
})

test_that("NAs mixed with valid IDs are removed with a message and rest are processed", {
  expect_message(
    result <- .get_taxonomic_classifications(
      c(NA_character_, "APHIA:125476", "APHIA:125477"),
      mock_taxonomic_lookup
    ),
    "1 NA value\\(s\\) removed"
  )

  expect_equal(nrow(result), 2)
  expect_setequal(result$scientific_id, c("APHIA:125476", "APHIA:125477"))
})

test_that("non-character input stops with an error", {
  expect_error(
    .get_taxonomic_classifications(123, mock_taxonomic_lookup),
    "`scientific_ids` must be a character vector"
  )
})


# ---------------------------------------------------------------------------
# .build_functional_group_enrollment() tests
# ---------------------------------------------------------------------------

# Functional group hierarchy:
#   FUNCTIONAL:1 (Biota, root)
#     FUNCTIONAL:2 (Macrophytes)
#       APHIA:143770 (Zosteraceae, enroll_all_lower_ranks = TRUE)
#     FUNCTIONAL:3 (Fish)
#       APHIA:111111 (Labridae, enroll_all_lower_ranks = FALSE)
#
# Taxonomic descendants of APHIA:143770:
#   APHIA:495077 (Zostera marina)  — present in observation_lookup (allowed)
#   APHIA:888888 (Unknown species) — NOT in observation_lookup (excluded)
#
# allowed_aphia = {APHIA:143770, APHIA:111111} (from fg)
#               union {APHIA:495077, APHIA:111111} (from ol)
#             = {APHIA:143770, APHIA:111111, APHIA:495077}
#
# Expected members after propagation:
#   APHIA:143770  -> {APHIA:143770, APHIA:495077}   (BFS filtered)
#   APHIA:111111  -> {APHIA:111111}                  (direct, in allowed)
#   FUNCTIONAL:2  -> {APHIA:143770, APHIA:495077}
#   FUNCTIONAL:3  -> {APHIA:111111}
#   FUNCTIONAL:1  -> {APHIA:143770, APHIA:495077, APHIA:111111}

mock_fg <- tibble::tribble(
  ~scientific_id,   ~parent_id,      ~functional_group_name, ~enroll_all_lower_ranks,
  "FUNCTIONAL:1",   NA,              "Biota",                FALSE,
  "FUNCTIONAL:2",   "FUNCTIONAL:1",  "Macrophytes",          FALSE,
  "APHIA:143770",   "FUNCTIONAL:2",  "Zosteraceae",          TRUE,
  "FUNCTIONAL:3",   "FUNCTIONAL:1",  "Fish",                 FALSE,
  "APHIA:111111",   "FUNCTIONAL:3",  "Labridae",             FALSE
)

mock_tl_fg <- tibble::tribble(
  ~id,   ~scientific_id,   ~parent_id,      ~rank,     ~name,
  "f2",  "APHIA:143770",   NA,              "Family",  "Zosteraceae",
  "s1",  "APHIA:495077",   "APHIA:143770",  "Species", "Zostera marina",
  "s2",  "APHIA:888888",   "APHIA:143770",  "Species", "Unknown species",
  "f3",  "APHIA:111111",   NA,              "Family",  "Labridae"
)

mock_ol <- tibble::tibble(scientific_id = c("APHIA:495077", "APHIA:111111"))

test_that("happy path: result is a named list with expected top-level keys", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)

  expect_type(result, "list")
  expect_true("FUNCTIONAL:1" %in% names(result))
})

test_that("each node has name, members, and children fields", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)
  node <- result[["FUNCTIONAL:1"]]

  expect_true(all(c("name", "members", "children") %in% names(node)))
})

test_that("FUNCTIONAL: nodes have empty members (IDs stored only at deepest node)", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)

  expect_length(result[["FUNCTIONAL:1"]]$members, 0)
  expect_length(result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:2"]]$members, 0)
  expect_length(result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:3"]]$members, 0)
})

test_that("each APHIA: ID appears only at its deepest enrollment node", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)

  # APHIA:495077 is only in APHIA:143770$members, not in any ancestor
  zostera_node <- result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:2"]]$children[["APHIA:143770"]]
  expect_true("APHIA:495077" %in% zostera_node$members)

  # Not duplicated in parent nodes
  expect_false("APHIA:495077" %in% result[["FUNCTIONAL:1"]]$members)
  expect_false("APHIA:495077" %in% result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:2"]]$members)
})

test_that("BFS descendants not in obs_lookup or fg_lookup are excluded from members", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)

  # APHIA:888888 is a taxonomic child of APHIA:143770 but not in ol or fg
  zostera_node <- result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:2"]]$children[["APHIA:143770"]]
  expect_false("APHIA:888888" %in% zostera_node$members)
})

test_that("enroll_all_lower_ranks = FALSE: only the node itself is enrolled if in allowed", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)

  labridae_node <- result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:3"]]$children[["APHIA:111111"]]
  expect_setequal(labridae_node$members, "APHIA:111111")
})

test_that("APHIA:143770 node members contain Zosteraceae and Zostera marina (BFS filtered)", {
  result <- .build_functional_group_enrollment(mock_tl_fg, mock_fg, mock_ol)

  zostera_node <- result[["FUNCTIONAL:1"]]$children[["FUNCTIONAL:2"]]$children[["APHIA:143770"]]
  expect_setequal(zostera_node$members, c("APHIA:143770", "APHIA:495077"))
})

test_that("empty fg returns empty list", {
  empty_fg <- mock_fg[0, ]
  result <- .build_functional_group_enrollment(mock_tl_fg, empty_fg, mock_ol)

  expect_type(result, "list")
  expect_length(result, 0)
})
