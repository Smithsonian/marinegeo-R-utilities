# ---------------------------------------------------------------------------
# Conditional QC rules: a rule that applies to a subset of rows, selected by a
# predicate on another column.
# ---------------------------------------------------------------------------

shoots_struct <- data.frame(
  table_id = "shoots_v1",
  column_name = c("site_name", "scientific_name", "shoot_length"),
  data_type = c("STRING", "STRING", "DOUBLE"),
  stringsAsFactors = FALSE
)

# A loose unconditional bound plus a tighter Zostera-only one on the same
# column -- the shape the metadata design is built around.
shoots_ranges <- data.frame(
  table_id = "shoots_v1",
  column_name = c("shoot_length", "shoot_length"),
  min_fail = c(0, 20),
  max_fail = c(200, 90),
  min_warn = NA_real_,
  max_warn = NA_real_,
  range_type = "inclusive",
  condition_id = c(NA, "zostera"),
  stringsAsFactors = FALSE
)

zostera_cond <- data.frame(
  condition_id = "zostera",
  table_id = "shoots_v1",
  column_name = "scientific_name",
  operator = "in",
  value = "Zostera marina|Zostera sp.",
  description = "Zostera shoots",
  stringsAsFactors = FALSE
)

empty_cats <- data.frame(
  table_id = character(0),
  column_name = character(0),
  value = character(0),
  stringsAsFactors = FALSE
)

cond_metadata <- function(
  ranges = shoots_ranges,
  conditions = zostera_cond,
  cats = empty_cats,
  struct = shoots_struct
) {
  out <- list(database_structure = struct, categorical_values = cats)
  if (!is.null(ranges)) {
    out$numeric_ranges <- ranges
  }
  if (!is.null(conditions)) {
    out$qc_conditions <- conditions
  }
  out
}

local_cond_metadata <- function(..., env = parent.frame()) {
  local_mocked_bindings(
    marinegeo_metadata = cond_metadata(...),
    .package = "marinegeo.utils",
    .env = env
  )
}

shoots_df <- function(scientific_name, shoot_length, site_name = "Site A") {
  data.frame(
    site_name = site_name,
    scientific_name = scientific_name,
    shoot_length = shoot_length,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# .qc_split_list: the shared `|` parser
# ---------------------------------------------------------------------------

test_that(".qc_split_list splits, trims and drops empty segments", {
  expect_equal(.qc_split_list("a|b|c"), c("a", "b", "c"))
  expect_equal(.qc_split_list(" a | b "), c("a", "b"))
  expect_equal(.qc_split_list("a||b"), c("a", "b"))
  expect_equal(.qc_split_list("solo"), "solo")
  expect_equal(.qc_split_list(NA), character(0))
})

# ---------------------------------------------------------------------------
# .qc_eval_term: the operator vocabulary
# ---------------------------------------------------------------------------

term_df <- data.frame(
  taxon = c("Zostera marina", "Halodule wrightii", NA),
  depth = c(1, 5, 10),
  stringsAsFactors = FALSE
)

test_that("set and equality operators evaluate against character values", {
  expect_equal(
    .qc_eval_term(term_df, "taxon", "in", "Zostera marina|Thalassia"),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(
    .qc_eval_term(term_df, "taxon", "==", "Zostera marina"),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(
    .qc_eval_term(term_df, "taxon", "!=", "Zostera marina"),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    .qc_eval_term(term_df, "taxon", "matches", "^Zostera"),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("ordered operators evaluate numerically", {
  expect_equal(.qc_eval_term(term_df, "depth", ">", "4"), c(FALSE, TRUE, TRUE))
  expect_equal(.qc_eval_term(term_df, "depth", "<=", "5"), c(TRUE, TRUE, FALSE))
})

test_that("an NA predicate value never matches, including under negation", {
  # The row-3 taxon is NA. Were `not_in` evaluated naively, !(NA %in% set)
  # would be TRUE and the rule would silently apply to a row we know nothing
  # about.
  expect_equal(
    .qc_eval_term(term_df, "taxon", "not_in", "Zostera marina"),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    .qc_eval_term(term_df, "taxon", "!=", "Zostera marina"),
    c(FALSE, TRUE, FALSE)
  )
})

test_that("is_na and not_na are the exception and do match NA", {
  expect_equal(
    .qc_eval_term(term_df, "taxon", "is_na", NA),
    c(FALSE, FALSE, TRUE)
  )
  expect_equal(
    .qc_eval_term(term_df, "taxon", "not_na", NA),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("bad metadata warns and matches nothing rather than erroring", {
  expect_warning(
    mask <- .qc_eval_term(term_df, "taxon", "approximately", "x"),
    "Unknown QC condition operator"
  )
  expect_false(any(mask))

  expect_warning(
    mask <- .qc_eval_term(term_df, "not_a_column", "in", "x"),
    "not present in the data"
  )
  expect_false(any(mask))

  expect_warning(
    mask <- .qc_eval_term(term_df, "taxon", "in", NA),
    "has no value"
  )
  expect_false(any(mask))
})

# ---------------------------------------------------------------------------
# .qc_eval_condition: terms are AND-ed
# ---------------------------------------------------------------------------

test_that("multiple rows sharing a condition_id are AND-ed", {
  two_terms <- data.frame(
    condition_id = c("deep_zostera", "deep_zostera"),
    table_id = "t",
    column_name = c("taxon", "depth"),
    operator = c("in", ">"),
    value = c("Zostera marina", "4"),
    stringsAsFactors = FALSE
  )
  # Row 1 is Zostera but shallow; row 2 is deep but not Zostera.
  expect_equal(
    .qc_eval_condition(term_df, two_terms),
    c(FALSE, FALSE, FALSE)
  )

  deep_zostera <- data.frame(
    taxon = c("Zostera marina", "Zostera marina"),
    depth = c(1, 9),
    stringsAsFactors = FALSE
  )
  expect_equal(.qc_eval_condition(deep_zostera, two_terms), c(FALSE, TRUE))
})

test_that("a condition with no terms matches nothing, never everything", {
  # A dangling condition_id must not widen its rule to the whole table.
  expect_equal(
    .qc_eval_condition(term_df, data.frame()),
    c(FALSE, FALSE, FALSE)
  )
})

# ---------------------------------------------------------------------------
# End to end through qc_run()
# ---------------------------------------------------------------------------

test_that("unconditional metadata is unaffected by the conditional layer", {
  local_cond_metadata(ranges = shoots_ranges[1, ], conditions = NULL)

  result <- qc_run(
    shoots_df(c("Zostera marina", "Halodule wrightii"), c(150, 10)),
    table_id = "shoots_v1"
  )

  expect_qc_issues(result)
  expect_equal(attr(result, "status"), "pass")
  expect_equal(nrow(result), 0L)
})

test_that("a conditional range flags only the rows matching its condition", {
  local_cond_metadata()

  result <- qc_run(
    shoots_df(
      c(
        "Halodule wrightii",
        "Zostera marina",
        "Halodule wrightii",
        "Zostera marina"
      ),
      c(150, 150, 10, 10)
    ),
    table_id = "shoots_v1"
  )

  ranges <- result[result$check == "qc_check_numeric_ranges", ]
  # 150 and 10 are both fine under the unconditional 0-200 bound; only the
  # Zostera rows breach the tighter 20-90 one.
  expect_equal(ranges$row, c(2L, 4L))
  expect_equal(unique(ranges$rule_id), "zostera")
  expect_equal(unique(ranges$severity), "fail")
})

test_that("reported rows index the full table, not the condition subset", {
  local_cond_metadata()

  # Only the last row is Zostera. An un-remapped index would report row 1,
  # which is a perfectly plausible-looking answer -- hence the dedicated test.
  result <- qc_run(
    shoots_df(
      c(
        "Halodule wrightii",
        "Halodule wrightii",
        "Halodule wrightii",
        "Zostera marina"
      ),
      c(10, 10, 10, 10)
    ),
    table_id = "shoots_v1"
  )

  ranges <- result[result$check == "qc_check_numeric_ranges", ]
  expect_equal(ranges$row, 4L)
  expect_equal(ranges$value, "10")
})

test_that("unconditional and conditional rules both fire and are told apart", {
  local_cond_metadata()

  result <- qc_run(
    shoots_df("Zostera marina", 250),
    table_id = "shoots_v1"
  )

  ranges <- result[result$check == "qc_check_numeric_ranges", ]
  expect_equal(nrow(ranges), 2L)
  expect_setequal(ranges$rule_id, c(NA_character_, "zostera"))
  expect_true(all(ranges$row == 1L))
})

test_that("the condition description rides along in `detail`", {
  local_cond_metadata()

  result <- qc_run(shoots_df("Zostera marina", 150), table_id = "shoots_v1")
  conditional <- result[!is.na(result$rule_id), ]

  expect_match(conditional$detail, "Zostera shoots")
})

test_that("`detail` falls back to the terms when metadata has no description", {
  local_cond_metadata(
    conditions = zostera_cond[, names(zostera_cond) != "description"]
  )

  result <- qc_run(shoots_df("Zostera marina", 150), table_id = "shoots_v1")
  conditional <- result[!is.na(result$rule_id), ]

  expect_match(conditional$detail, "scientific_name in Zostera marina")
})

test_that("an NA in the predicate column leaves the conditional rule unapplied", {
  local_cond_metadata()

  result <- qc_run(
    shoots_df(NA_character_, 150),
    table_id = "shoots_v1"
  )

  # 150 passes the unconditional 0-200 bound, and we cannot know whether the
  # tighter Zostera bound applies, so nothing is reported.
  expect_equal(nrow(result[result$check == "qc_check_numeric_ranges", ]), 0L)
})

test_that("an unknown operator warns, skips the rule, and completes the run", {
  bad <- zostera_cond
  bad$operator <- "approximately"
  local_cond_metadata(conditions = bad)

  expect_warning(
    result <- qc_run(shoots_df("Zostera marina", 150), table_id = "shoots_v1"),
    "Unknown QC condition operator"
  )

  expect_qc_issues(result)
  expect_equal(nrow(result[result$check == "qc_check_numeric_ranges", ]), 0L)
  expect_true("qc_check_numeric_ranges" %in% attr(result, "checks_run"))
})

test_that("a rule referencing a missing condition_id warns and is not applied", {
  local_cond_metadata(conditions = NULL)

  expect_warning(
    result <- qc_run(shoots_df("Zostera marina", 150), table_id = "shoots_v1"),
    "reference condition_id"
  )
  expect_equal(nrow(result[result$check == "qc_check_numeric_ranges", ]), 0L)
})

test_that("conditional categorical values are supported by the same mechanism", {
  cats <- data.frame(
    table_id = "shoots_v1",
    column_name = "site_name",
    value = c("Site A", "Site Z"),
    condition_id = c("zostera", "zostera"),
    stringsAsFactors = FALSE
  )
  local_cond_metadata(ranges = NULL, cats = cats)

  result <- qc_run(
    shoots_df(
      c("Halodule wrightii", "Zostera marina"),
      c(10, 10),
      site_name = c("Site Q", "Site Q")
    ),
    table_id = "shoots_v1"
  )

  cat_issues <- result[result$check == "qc_check_categorical_values", ]
  expect_equal(cat_issues$row, 2L)
  expect_equal(cat_issues$rule_id, "zostera")
})

# ---------------------------------------------------------------------------
# Column scope: a condition cannot be evaluated from one column
# ---------------------------------------------------------------------------

test_that("qc_check_variable drops the conditional rule and records it", {
  local_cond_metadata()

  # 10 would breach the Zostera minimum of 20, but with only shoot_length in
  # hand there is no way to know the taxon -- applying it anyway would validate
  # every species against Zostera bounds.
  result <- qc_check_variable(
    c(10, 150),
    variable = "shoot_length",
    table_id = "shoots_v1"
  )

  expect_equal(nrow(result), 0L)
  expect_equal(attr(result, "rules_skipped"), "zostera")
})

test_that("the unconditional rule still applies in column scope", {
  local_cond_metadata()

  result <- qc_check_variable(
    c(250),
    variable = "shoot_length",
    table_id = "shoots_v1"
  )

  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$rule_id))
  expect_equal(attr(result, "rules_skipped"), "zostera")
})

test_that("partial = TRUE keeps the conditional rule when both columns are given", {
  local_cond_metadata()

  result <- qc_run(
    shoots_df(c("Zostera marina", "Halodule wrightii"), c(150, 150)),
    table_id = "shoots_v1",
    partial = TRUE
  )

  ranges <- result[result$check == "qc_check_numeric_ranges", ]
  expect_equal(ranges$row, 1L)
  expect_equal(ranges$rule_id, "zostera")
  expect_equal(attr(result, "rules_skipped"), character(0))
})

test_that("rules_skipped is empty for a full table run", {
  local_cond_metadata()

  result <- qc_run(shoots_df("Zostera marina", 50), table_id = "shoots_v1")

  expect_equal(attr(result, "rules_skipped"), character(0))
})
