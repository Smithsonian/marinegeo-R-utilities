# ---------------------------------------------------------------------------
# Taxonomy-aware condition targets.
#
# The taxon facts (cases keyed to real clades) are asserted against the bundled
# registry, because the specific contents ARE the behaviour being bought. The
# grammar, error and cycle cases use small mocked hierarchies.
#
# The suite is forced offline by setup.R, so these read bundled data and touch
# no network.
# ---------------------------------------------------------------------------

# Captured before any mocking, so a mocked `marinegeo_metadata` can still carry
# the real taxonomy tables.
real_meta <- marinegeo.utils:::marinegeo_metadata

zostera_genus_lsid <- "urn:lsid:marinespecies.org:taxname:144233"

# ---------------------------------------------------------------------------
# .qc_norm_rank
# ---------------------------------------------------------------------------

test_that(".qc_norm_rank folds the parenthetical WoRMS spellings, NA-safely", {
  expect_equal(.qc_norm_rank("Phylum (Division)"), "Phylum")
  expect_equal(.qc_norm_rank("Subphylum (Subdivision)"), "Subphylum")
  expect_equal(.qc_norm_rank("Genus"), "Genus")
  # NA must survive rather than erroring on an NA subscript assignment.
  expect_equal(
    .qc_norm_rank(c("Phylum (Division)", NA, "Class")),
    c("Phylum", NA, "Class")
  )
  expect_equal(.qc_norm_rank(character(0)), character(0))
})

# ---------------------------------------------------------------------------
# Expansion against the real registry
# ---------------------------------------------------------------------------

test_that("Genus:Zostera expands to every registry Zostera, not just the obvious ones", {
  out <- utl_mg_expand_taxon_set("Genus:Zostera")

  expect_named(out, c("scientific_name", "scientific_id"))
  # The regression this feature exists for: a hand-written "Zostera *" list
  # misses this name, because it does not begin with "Zostera".
  expect_true("Heterozostera nigricaulis" %in% out$scientific_name)
  expect_true("Zostera marina" %in% out$scientific_name)
  expect_gt(nrow(out), 5L)
})

test_that("a clade reference and its LSID give the same set", {
  expect_equal(
    utl_mg_expand_taxon_set(zostera_genus_lsid),
    utl_mg_expand_taxon_set("Genus:Zostera")
  )
})

test_that("a higher rank widens the set and a species narrows it to itself", {
  genus <- utl_mg_expand_taxon_set("Genus:Zostera")
  family <- utl_mg_expand_taxon_set("Family:Zosteraceae")
  species <- utl_mg_expand_taxon_set("Species:Zostera marina")

  expect_true(all(genus$scientific_name %in% family$scientific_name))
  # A node is its own ancestor.
  expect_equal(species$scientific_name, "Zostera marina")
})

test_that("rank normalization lets a plant lineage be named `Phylum`", {
  # Tracheophyta is stored as "Phylum (Division)".
  expect_gt(nrow(utl_mg_expand_taxon_set("Phylum:Tracheophyta")), 0L)
})

test_that("several references give the union of their sets", {
  both <- utl_mg_expand_taxon_set(c("Genus:Zostera", "Genus:Halodule"))
  expect_true("Zostera marina" %in% both$scientific_name)
  expect_true("Halodule wrightii" %in% both$scientific_name)
})

# ---------------------------------------------------------------------------
# Operator behaviour
# ---------------------------------------------------------------------------

taxa_df <- function(names) {
  data.frame(scientific_name = names, stringsAsFactors = FALSE)
}

test_that("taxon_within matches rows at or below the named clade", {
  d <- taxa_df(c(
    "Zostera marina",
    "Halodule wrightii",
    "Heterozostera nigricaulis"
  ))

  expect_equal(
    .qc_eval_term(d, "scientific_name", "taxon_within", "Genus:Zostera"),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("`|` in a taxon value is a union, which is how OR is expressed", {
  d <- taxa_df(c("Zostera marina", "Halodule wrightii", "Gastropoda"))

  expect_equal(
    .qc_eval_term(
      d,
      "scientific_name",
      "taxon_within",
      "Genus:Zostera|Genus:Halodule"
    ),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("rank abbreviations resolve to the parent taxon", {
  # utl_mg_get_scientific_id() strips " sp."/" spp.", so an abbreviated entry
  # still lands inside its clade -- but only where the stripped name is itself a
  # registry entry, which is true of Gastropoda and not of most genera.
  d <- taxa_df(c("Gastropoda sp.", "Gastropoda spp.", "Zostera marina"))

  expect_equal(
    .qc_eval_term(d, "scientific_name", "taxon_within", "Class:Gastropoda"),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("an unregistered name does not leak out as a condition warning", {
  # qc_check_lookup_values() reports unregistered names properly and once;
  # repeating it per condition would be noise on every QC run.
  d <- taxa_df(c("Zostera marina", "Not A Real Name"))

  expect_no_warning(
    mask <- .qc_eval_term(d, "scientific_name", "taxon_within", "Genus:Zostera")
  )
  expect_equal(mask, c(TRUE, FALSE))
})

test_that("a row the hierarchy cannot place matches neither form", {
  functional_name <- real_meta$observation_lookup$scientific_name[
    startsWith(real_meta$observation_lookup$scientific_id, "FUNCTIONAL:")
  ][1]
  skip_if(is.na(functional_name), "no FUNCTIONAL: entry in the registry")

  d <- taxa_df(c("Zostera marina", functional_name, NA, "Not A Real Name"))

  within <- .qc_eval_term(
    d,
    "scientific_name",
    "taxon_within",
    "Genus:Zostera"
  )
  not_within <- .qc_eval_term(
    d,
    "scientific_name",
    "taxon_not_within",
    "Genus:Zostera"
  )

  expect_equal(within, c(TRUE, FALSE, FALSE, FALSE))
  # A conditional rule must not fire on a row that could not be classified, so
  # the negation excludes them too -- the two forms are not complements.
  expect_equal(not_within, c(FALSE, FALSE, FALSE, FALSE))
})

test_that("taxon_not_within matches placeable rows outside the clade", {
  d <- taxa_df(c("Zostera marina", "Halodule wrightii"))

  expect_equal(
    .qc_eval_term(d, "scientific_name", "taxon_not_within", "Genus:Zostera"),
    c(FALSE, TRUE)
  )
})

# ---------------------------------------------------------------------------
# Functional groups
# ---------------------------------------------------------------------------

test_that("in_functional_group matches members of the named group", {
  d <- taxa_df(c("Zostera marina", "Halodule wrightii", "Gastropoda"))

  expect_equal(
    .qc_eval_term(
      d,
      "scientific_name",
      "in_functional_group",
      "vegetation:Marine Seagrass"
    ),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("a bare functional group label is refused, naming its trees", {
  d <- taxa_df("Zostera marina")

  expect_warning(
    mask <- .qc_eval_term(
      d,
      "scientific_name",
      "in_functional_group",
      "Algae"
    ),
    "needs a 'tree:Label' prefix"
  )
  # Not applied -- never silently widened to everything.
  expect_false(any(mask))

  expect_warning(
    utl_mg_expand_taxon_set("Algae", hierarchy = "functional_group"),
    "occurs in tree\\(s\\)"
  )
})

test_that("an unknown tree or label is refused", {
  expect_warning(
    utl_mg_expand_taxon_set("vegetation:Nope", hierarchy = "functional_group"),
    "not in tree 'vegetation'"
  )
  expect_warning(
    utl_mg_expand_taxon_set("nope:Algae", hierarchy = "functional_group"),
    "Known trees"
  )
})

# ---------------------------------------------------------------------------
# Unresolvable references warn and match nothing
# ---------------------------------------------------------------------------

test_that("a misspelled taxon warns and is not applied", {
  d <- taxa_df("Zostera marina")

  expect_warning(
    mask <- .qc_eval_term(d, "scientific_name", "taxon_within", "Genus:Zostra"),
    "found no 'Genus' named 'Zostra'"
  )
  expect_false(any(mask))
})

test_that("the right name at the wrong rank says where the name does live", {
  expect_warning(
    utl_mg_expand_taxon_set("Order:Zostera"),
    "that name exists at rank: Genus"
  )
})

test_that("a bad LSID and a reference with no rank are both refused", {
  expect_warning(
    utl_mg_expand_taxon_set("urn:lsid:marinespecies.org:taxname:999999999"),
    "not in `taxonomic_lookup`"
  )
  expect_warning(
    utl_mg_expand_taxon_set("Zostera"),
    "Expected 'Rank:Name'"
  )
})

test_that("an ambiguous Rank:Name is refused rather than guessed", {
  dup <- data.frame(
    scientific_id = c("ID:1", "ID:2"),
    name = c("Ambigua", "Ambigua"),
    rank = c("Genus", "Genus"),
    parent_id = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    marinegeo_metadata = list(taxonomic_lookup = dup),
    .package = "marinegeo.utils"
  )

  expect_warning(
    expect_null(.qc_resolve_taxon_node("Genus:Ambigua")),
    "is ambiguous"
  )
})

# ---------------------------------------------------------------------------
# Robustness of the closure
# ---------------------------------------------------------------------------

test_that("a cyclic parent_id terminates instead of hanging", {
  cyclic <- data.frame(
    scientific_id = c("A", "B", "C"),
    name = c("a", "b", "c"),
    rank = c("Genus", "Family", "Order"),
    parent_id = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- .qc_taxon_closure(cyclic, max_depth = 10L),
    "may contain a cycle"
  )
  expect_true(is.list(out))
})

test_that("a missing taxonomy table warns rather than erroring", {
  local_mocked_bindings(
    marinegeo_metadata = list(),
    .package = "marinegeo.utils"
  )

  expect_warning(
    expect_null(.qc_taxon_sets("Genus:Zostera")),
    "unavailable"
  )
  expect_warning(
    expect_null(.qc_fg_sets("vegetation:Algae")),
    "unavailable"
  )
})

# ---------------------------------------------------------------------------
# utl_mg_expand_taxon_set contract
# ---------------------------------------------------------------------------

test_that("expand returns the canonical two columns for both hierarchies", {
  taxonomy <- utl_mg_expand_taxon_set("Genus:Zostera")
  groups <- utl_mg_expand_taxon_set(
    "vegetation:Marine Seagrass",
    hierarchy = "functional_group"
  )

  expect_named(taxonomy, c("scientific_name", "scientific_id"))
  expect_named(groups, c("scientific_name", "scientific_id"))
  expect_gt(nrow(groups), 0L)
  # Ordered by name, with row names reset.
  expect_equal(taxonomy$scientific_name, sort(taxonomy$scientific_name))
})

test_that("expand validates its arguments", {
  expect_error(utl_mg_expand_taxon_set(42), "non-empty character vector")
  expect_error(utl_mg_expand_taxon_set(character(0)), "non-empty character")
  expect_error(utl_mg_expand_taxon_set(NA_character_), "no NA values")
  expect_error(
    utl_mg_expand_taxon_set("Genus:Zostera", hierarchy = "nope"),
    "should be one of"
  )
})

# ---------------------------------------------------------------------------
# End to end through qc_run()
# ---------------------------------------------------------------------------

test_that("a taxon-conditioned range fires only for the clade, with true row numbers", {
  struct <- data.frame(
    table_id = "leaf_v1",
    column_name = c("scientific_name", "leaf_length_mm"),
    data_type = c("STRING", "DOUBLE"),
    stringsAsFactors = FALSE
  )
  ranges <- data.frame(
    table_id = "leaf_v1",
    column_name = "leaf_length_mm",
    range_type = "inclusive",
    max_fail = 1200,
    min_fail = 20,
    max_warn = NA_real_,
    min_warn = NA_real_,
    condition_id = "zostera",
    stringsAsFactors = FALSE
  )
  conditions <- data.frame(
    condition_id = "zostera",
    table_id = "leaf_v1",
    column_name = "scientific_name",
    operator = "taxon_within",
    value = "Genus:Zostera",
    description = "Leaf of a Zostera species",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    marinegeo_metadata = list(
      database_structure = struct,
      categorical_values = data.frame(
        table_id = character(0),
        column_name = character(0),
        value = character(0),
        stringsAsFactors = FALSE
      ),
      numeric_ranges = ranges,
      qc_conditions = conditions,
      taxonomic_lookup = real_meta$taxonomic_lookup,
      observation_lookup = real_meta$observation_lookup
    ),
    .package = "marinegeo.utils"
  )

  # Only the last row is a Zostera, and only it breaches the band. An
  # un-remapped row index would report row 1.
  df <- data.frame(
    scientific_name = c(
      "Halodule wrightii",
      "Halodule wrightii",
      "Heterozostera nigricaulis"
    ),
    leaf_length_mm = c(5000, 5000, 5000),
    stringsAsFactors = FALSE
  )

  result <- qc_run(df, table_id = "leaf_v1")
  ranges_found <- result[result$check == "qc_check_numeric_ranges", ]

  expect_equal(ranges_found$row, 3L)
  expect_equal(ranges_found$rule_id, "zostera")
  expect_match(ranges_found$detail, "Zostera")
})
