# Minimal adjacency table covering two lineages:
#   - Animal lineage (uses "Phylum"): Animalia > Chordata > Actinopterygii >
#     Perciformes > Labridae > Thalassoma > Thalassoma bifasciatum / T. lunare
#   - Plant lineage (uses "Phylum (Division)"): Plantae > Tracheophyta >
#     Liliopsida > Alismatales > Zosteraceae > Zostera > Zostera marina
#
# Having both lineages ensures the `Phylum (Division)` → `Phylum` case_when
# produces both columns after unnesting.

mock_taxonomic_lookup <- tibble::tribble(
  ~id,   ~scientific_id,    ~parent_id, ~rank,               ~name,
  "k1",  NA,                NA,         "Kingdom",           "Animalia",
  "p1",  NA,                "k1",       "Phylum",            "Chordata",
  "c1",  NA,                "p1",       "Class",             "Actinopterygii",
  "o1",  NA,                "c1",       "Order",             "Perciformes",
  "f1",  NA,                "o1",       "Family",            "Labridae",
  "g1",  NA,                "f1",       "Genus",             "Thalassoma",
  "s1",  "APHIA:125476",    "g1",       "Species",           "Thalassoma bifasciatum",
  "s2",  "APHIA:125477",    "g1",       "Species",           "Thalassoma lunare",
  "k2",  NA,                NA,         "Kingdom",           "Plantae",
  "p2",  NA,                "k2",       "Phylum (Division)", "Tracheophyta",
  "c2",  NA,                "p2",       "Class",             "Liliopsida",
  "o2",  NA,                "c2",       "Order",             "Alismatales",
  "f2",  NA,                "o2",       "Family",            "Zosteraceae",
  "g2",  NA,                "f2",       "Genus",             "Zostera",
  "s3",  "APHIA:374534",    "g2",       "Species",           "Zostera marina"
)

# ---------------------------------------------------------------------------
# .get_parent_rank() unit tests
# ---------------------------------------------------------------------------

test_that(".get_parent_rank returns all ancestor ranks up to root", {
  result <- .get_parent_rank("s1", mock_taxonomic_lookup)

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
  result <- .get_parent_rank("k1", mock_taxonomic_lookup)

  expect_length(result, 1)
  expect_equal(result[["Kingdom"]], "Animalia")
})

test_that(".get_parent_rank returns empty list when node_id is not found", {
  result <- .get_parent_rank("NONEXISTENT", mock_taxonomic_lookup)

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
