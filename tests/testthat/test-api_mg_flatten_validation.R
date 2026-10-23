# ---------------------------------------------------------------------------
# Schema validation must error, never degrade
#
# Every QC consumer guards with
#   is.null(tbl) || !is.data.frame(tbl) || !all(required %in% colnames(tbl)) ||
#   nrow(tbl) == 0
# and falls back to "no rules" (R/qc_mg_rules.R:83-90). So a flattener that
# returned a partial table on bad input would make qc_run() report a clean pass
# having run zero checks. These tests pin the loud failures.
# ---------------------------------------------------------------------------

.load_yaml_str <- function(txt) {
  skip_if_not_installed("yaml")
  list(`test.yaml` = yaml::yaml.load(txt, handlers = .mg_yaml_handlers()))
}

.minimal <- function(extra_col = "", checks = "", conditions = "") {
  paste0(
    "protocol: Test Protocol\n",
    "tables:\n",
    "- table_id: test-table-v1\n",
    "  columns:\n",
    "  - name: site_code\n",
    "    type: STRING\n",
    "    na_values: fail\n",
    "  - name: count\n",
    "    type: INT\n",
    extra_col,
    checks,
    conditions
  )
}

test_that("the minimal fixture is valid, so later failures are attributable", {
  docs <- .load_yaml_str(.minimal())
  expect_true(.mg_validate_table_metadata(docs))
  expect_identical(
    nrow(.mg_flatten_table_metadata(docs)$database_structure),
    2L
  )
})

# ---------------------------------------------------------------------------
# Predicates
# ---------------------------------------------------------------------------

test_that("`any:` is rejected rather than silently ANDed", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: 10\n      when:\n        any:\n        - column: site_code\n          in: [a, b]\n"
  ))

  # .qc_eval_condition is Reduce(`&`, masks): there is no OR mechanism, so
  # accepting `any:` would invert the rule's meaning.
  expect_error(.mg_flatten_table_metadata(docs), "`any:` is not supported")
})

test_that("an unknown operator is rejected against the closed vocabulary", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: 10\n      when:\n        all:\n        - column: site_code\n          sql_injection: DROP TABLE\n"
  ))

  expect_error(.mg_flatten_table_metadata(docs), "is not a known operator")
})

test_that("a term with no operator, or two, is rejected", {
  none <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      when:\n        all:\n        - column: site_code\n"
  ))
  expect_error(.mg_flatten_table_metadata(none), "exactly one operator key")

  two <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      when:\n        all:\n        - column: site_code\n          in: [a]\n          not_in: [b]\n"
  ))
  expect_error(.mg_flatten_table_metadata(two), "exactly one operator key")
})

test_that("an empty operator list is rejected", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      when:\n        all:\n        - column: site_code\n          in: []\n"
  ))

  # .qc_split_list() drops empty segments, so "" would yield character(0) and
  # the condition would match no rows at all -- a silent no-op.
  expect_error(.mg_flatten_table_metadata(docs), "needs at least one value")
})

test_that("a multi-value operator flattens to a pipe-delimited cell", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: 10\n      when:\n        all:\n        - column: site_code\n          in: [\"Zostera marina\", \"Zostera sp.\"]\n"
  ))
  flat <- .mg_flatten_table_metadata(docs)

  expect_identical(flat$qc_conditions$value, "Zostera marina|Zostera sp.")
  expect_identical(
    .qc_split_list(flat$qc_conditions$value),
    c("Zostera marina", "Zostera sp.")
  )
})

test_that("NA operators flatten to NA, not an empty string or 'true'", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: 10\n      when:\n        all:\n        - column: site_code\n          not_na: true\n"
  ))
  flat <- .mg_flatten_table_metadata(docs)

  expect_identical(flat$qc_conditions$operator, "not_na")
  expect_true(is.na(flat$qc_conditions$value))
})

test_that("multiple when terms become multiple AND-ed condition rows", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: 10\n      when:\n        all:\n        - column: site_code\n          in: [a]\n        - column: count\n          not_na: true\n"
  ))
  flat <- .mg_flatten_table_metadata(docs)

  expect_identical(nrow(flat$qc_conditions), 2L)
  expect_identical(length(unique(flat$qc_conditions$condition_id)), 1L)
})

# ---------------------------------------------------------------------------
# Condition id synthesis
# ---------------------------------------------------------------------------

test_that("an unlabelled condition gets a deterministic table-scoped id", {
  txt <- .minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: 10\n      when:\n        all:\n        - column: site_code\n          in: [a]\n"
  )
  first <- .mg_flatten_table_metadata(.load_yaml_str(txt))
  second <- .mg_flatten_table_metadata(.load_yaml_str(txt))

  expect_identical(first$qc_conditions$condition_id, "test-table-v1.c1")
  expect_identical(first$qc_conditions, second$qc_conditions)
})

test_that("identical when blocks collapse to a single condition", {
  docs <- .load_yaml_str(.minimal(
    paste0(
      "    numeric_range:\n",
      "    - range_type: inclusive\n      max_fail: 10\n      when:\n        all:\n        - column: site_code\n          in: [a]\n",
      "    - range_type: exclusive\n      min_fail: 0\n      when:\n        all:\n        - column: site_code\n          in: [a]\n"
    )
  ))
  flat <- .mg_flatten_table_metadata(docs)

  expect_identical(nrow(flat$qc_conditions), 1L)
  expect_identical(unique(flat$numeric_ranges$condition_id), "test-table-v1.c1")
})

test_that("a duplicate label within a file is rejected", {
  docs <- .load_yaml_str(.minimal(
    paste0(
      "    numeric_range:\n",
      "    - range_type: inclusive\n      max_fail: 10\n      when:\n        label: dup\n        all:\n        - column: site_code\n          in: [a]\n",
      "    - range_type: exclusive\n      min_fail: 0\n      when:\n        label: dup\n        all:\n        - column: site_code\n          in: [b]\n"
    )
  ))

  expect_error(.mg_flatten_table_metadata(docs), "duplicate condition id")
})

# ---------------------------------------------------------------------------
# Types and enums
# ---------------------------------------------------------------------------

test_that("a quoted boolean still coerces, and junk is rejected", {
  ok <- .load_yaml_str(.minimal("    uuid_identity: \"TRUE\"\n"))
  expect_true(any(
    .mg_flatten_table_metadata(ok)$database_structure$uuid_identity
  ))

  # as.logical("yes") is NA, which would silently drop the column from the
  # row-uuid identity set; the YAML 1.1 token set is handled explicitly.
  yes <- .load_yaml_str(.minimal("    uuid_identity: yes\n"))
  expect_true(any(
    .mg_flatten_table_metadata(yes)$database_structure$uuid_identity
  ))

  bad <- .load_yaml_str(.minimal("    uuid_identity: maybe\n"))
  expect_error(.mg_flatten_table_metadata(bad), "expected a boolean")
})

test_that("a non-numeric bound is rejected rather than silently NA", {
  docs <- .load_yaml_str(.minimal(
    "    numeric_range:\n      range_type: inclusive\n      max_fail: lots\n"
  ))

  expect_error(.mg_flatten_table_metadata(docs), "expected a number")
})

test_that("the pre-rename `missing:` key is refused, not ignored", {
  # The flattener validates values, not key names, so a missed rename would
  # otherwise read as "no rule" and the column would silently lose its NA
  # check.
  expect_error(
    .mg_flatten_table_metadata(
      .load_yaml_str(.minimal("    missing: enforce\n"))
    ),
    "renamed to `na_values:`"
  )
})

test_that("the pre-rename `enforce` value is refused", {
  expect_error(
    .mg_flatten_table_metadata(
      .load_yaml_str(.minimal("    na_values: enforce\n"))
    ),
    "is not one of"
  )
})

test_that("closed enums are enforced", {
  expect_error(
    .mg_flatten_table_metadata(.load_yaml_str(.minimal(
      "    na_values: sometimes\n"
    ))),
    "is not one of"
  )
  expect_error(
    .mg_flatten_table_metadata(
      .load_yaml_str(.minimal("    numeric_range:\n      range_type: fuzzy\n"))
    ),
    "is not one of"
  )
})

test_that("a vocabulary value that looks numeric or boolean survives verbatim", {
  docs <- .load_yaml_str(.minimal(
    "    one_of: [\"0\", \"0.1\", \"TRUE\", \"N\", \"no\", 50x50cm]\n"
  ))
  flat <- .mg_flatten_table_metadata(docs)

  # The whole reason scalars are parsed as strings: as.character() on the
  # parsed value would turn N into "FALSE".
  expect_identical(
    flat$categorical_values$value,
    c("0", "0.1", "TRUE", "N", "no", "50x50cm")
  )
})

test_that("columns must be a sequence, because order is significant", {
  txt <- paste0(
    "protocol: Test Protocol\n",
    "tables:\n",
    "- table_id: test-table-v1\n",
    "  columns:\n",
    "    site_code:\n",
    "      type: STRING\n"
  )

  expect_error(
    .mg_flatten_table_metadata(.load_yaml_str(txt)),
    "must be a sequence"
  )
})

test_that("required fields are required", {
  no_type <- paste0(
    "protocol: Test\ntables:\n- table_id: t-v1\n  columns:\n  - name: a\n"
  )
  expect_error(
    .mg_flatten_table_metadata(.load_yaml_str(no_type)),
    "missing required value"
  )

  no_protocol <- paste0(
    "tables:\n- table_id: t-v1\n  columns:\n  - name: a\n    type: STRING\n"
  )
  expect_error(
    .mg_flatten_table_metadata(.load_yaml_str(no_protocol)),
    "missing `protocol"
  )
})

# ---------------------------------------------------------------------------
# Aggregate rules
# ---------------------------------------------------------------------------

test_that("an aggregate rule needs exactly one target", {
  both <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n    - rule_id: r1\n      group_by: [site_code]\n",
      "      value: count\n      function: sum\n      comparison: \"==\"\n",
      "      target_column: count\n      target_value: 10\n"
    )
  ))
  expect_error(.mg_flatten_table_metadata(both), "not both")

  neither <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n    - rule_id: r1\n      group_by: [site_code]\n",
      "      value: count\n      function: sum\n      comparison: \"==\"\n"
    )
  ))
  expect_error(.mg_flatten_table_metadata(neither), "is required")
})

test_that("group_by becomes a pipe-delimited cell the engine can split", {
  docs <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n    - rule_id: r1\n      group_by: [site_code, count]\n",
      "      value: count\n      function: sum\n      comparison: \"<=\"\n      target_value: 100\n"
    )
  ))
  flat <- .mg_flatten_table_metadata(docs)

  expect_identical(flat$aggregate_rules$group_columns, "site_code|count")
  expect_identical(
    .qc_split_list(flat$aggregate_rules$group_columns),
    c("site_code", "count")
  )
})

test_that("an unknown aggregate function or comparison is rejected", {
  bad_fn <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n    - rule_id: r1\n      group_by: [site_code]\n",
      "      value: count\n      function: median\n      comparison: \"==\"\n      target_value: 1\n"
    )
  ))
  expect_error(.mg_flatten_table_metadata(bad_fn), "is not one of")

  bad_cmp <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n    - rule_id: r1\n      group_by: [site_code]\n",
      "      value: count\n      function: sum\n      comparison: \"=~\"\n      target_value: 1\n"
    )
  ))
  expect_error(.mg_flatten_table_metadata(bad_cmp), "is not one of")
})

# ---------------------------------------------------------------------------
# Referential checks
# ---------------------------------------------------------------------------

test_that("a rule naming a column that does not exist is rejected", {
  docs <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n    - rule_id: r1\n      group_by: [not_a_column]\n",
      "      value: count\n      function: sum\n      comparison: \"<=\"\n      target_value: 100\n"
    )
  ))

  expect_error(.mg_validate_table_metadata(docs), "unknown column")
})

test_that("a cross-table rule naming an unknown right table is rejected", {
  docs <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    cross_table:\n    - rule_id: x1\n      relation: subset\n",
      "      right_table: does-not-exist-v1\n      on:\n      - left: site_code\n        right: site_code\n"
    )
  ))

  expect_error(.mg_validate_table_metadata(docs), "unknown right table")
})

test_that("a duplicate rule_id is rejected", {
  docs <- .load_yaml_str(.minimal(
    checks = paste0(
      "  checks:\n    aggregate:\n",
      "    - rule_id: same\n      group_by: [site_code]\n      value: count\n      function: sum\n      comparison: \"<=\"\n      target_value: 1\n",
      "    - rule_id: same\n      group_by: [site_code]\n      value: count\n      function: sum\n      comparison: \">=\"\n      target_value: 0\n"
    )
  ))

  expect_error(.mg_validate_table_metadata(docs), "duplicate rule_id")
})

test_that("a directory with no YAML errors rather than returning nothing", {
  skip_if_not_installed("yaml")
  empty <- withr::local_tempdir()

  expect_error(.mg_parse_table_metadata_dir(empty), "no YAML files found")
  expect_error(
    .mg_parse_table_metadata_dir(file.path(empty, "nope")),
    "directory not found"
  )
})
