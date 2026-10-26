# ---------------------------------------------------------------------------
# Grouped aggregate rules: a property of a *set* of rows sharing a key, rather
# than of a single cell.
#
# Grouping, aggregation and the comparison all come from metadata, so the
# oyster point-count total and the structurally identical fouling panel total
# are two rows in `aggregate_rules` rather than two hand-written functions.
# Row filtering is not duplicated here: a rule carries a `condition_id` and
# .qc_apply_conditional() has already narrowed `data` by the time this check
# runs, which is also what makes the aggregate correct -- it is computed over
# the filtered rows only.
# ---------------------------------------------------------------------------

# Closed vocabulary of aggregates. `na.rm = TRUE` throughout, matching how the
# value checks skip NA rather than letting one NA collapse a whole group.
.qc_aggregate_functions <- list(
  sum = function(x) sum(x, na.rm = TRUE),
  mean = function(x) mean(x, na.rm = TRUE),
  min = function(x) suppressWarnings(min(x, na.rm = TRUE)),
  max = function(x) suppressWarnings(max(x, na.rm = TRUE)),
  n = function(x) length(x),
  n_distinct = function(x) length(unique(x[!is.na(x)]))
)

# Closed vocabulary of comparisons. Each returns TRUE when the rule *holds*;
# an issue is raised where it does not. `tolerance` always widens the accepted
# region, so a rule never becomes stricter by being given slack.
.qc_aggregate_comparisons <- list(
  "==" = function(observed, expected, tol) abs(observed - expected) <= tol,
  "!=" = function(observed, expected, tol) abs(observed - expected) > tol,
  ">" = function(observed, expected, tol) observed > expected - tol,
  ">=" = function(observed, expected, tol) observed >= expected - tol,
  "<" = function(observed, expected, tol) observed < expected + tol,
  "<=" = function(observed, expected, tol) observed <= expected + tol
)

# Format a number for a human-readable `detail` without scientific notation or
# trailing zeros.
.qc_fmt_num <- function(x) {
  vapply(
    x,
    function(v) {
      if (is.na(v)) "NA" else format(v, trim = TRUE, scientific = FALSE)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# A composite group key. "\r" cannot occur in a parsed CSV field, so it cannot
# collide with the values themselves the way "_" or "-" could.
.qc_group_key <- function(data, group_cols) {
  if (length(group_cols) == 0L) {
    return(rep("", nrow(data)))
  }
  do.call(
    paste,
    c(lapply(group_cols, function(g) as.character(data[[g]])), sep = "\r")
  )
}

# Scalar field accessor that tolerates an absent optional column.
.qc_rule_field <- function(rule, field, default = NA) {
  if (field %in% colnames(rule)) rule[[field]][[1]] else default
}

#' Check grouped aggregates against an expected value
#'
#' @description
#' Validates a property of a *group* of rows rather than of a single value: that
#' the sum, count, mean, minimum or maximum of a column, taken within a grouping
#' key, stands in a declared relation to an expected value. The expected value
#' may be another column in the data (a declared total such as
#' `points_in_quadrat`) or a literal from metadata.
#'
#' Driven by `aggregate_rules` metadata, so a new grouped rule is a CSV row
#' rather than a new function. Can be called directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#' @param rules A data frame of rules, one per row, with columns:
#'   \describe{
#'     \item{`group_columns`}{`|`-delimited grouping key, e.g.
#'       `"site_name|transect|quadrat"`.}
#'     \item{`value_column`}{The column to aggregate.}
#'     \item{`aggregate_function`}{One of `"sum"`, `"mean"`, `"min"`, `"max"`,
#'       `"n"`, `"n_distinct"`.}
#'     \item{`comparison`}{One of `"=="`, `"!="`, `">"`, `">="`, `"<"`, `"<="`,
#'       read as "the aggregate must be _ the expected value".}
#'     \item{`target_column`}{Column holding the expected value, assumed
#'       constant within a group. Mutually exclusive with `target_value`.}
#'     \item{`target_value`}{Literal expected value. Mutually exclusive with
#'       `target_column`.}
#'     \item{`tolerance`}{Optional numeric slack; `NA` means exact.}
#'     \item{`severity`}{`"fail"` or `"warn"`. Defaults to `"fail"`.}
#'     \item{`rule_id`}{Optional identifier, reported in the result.}
#'     \item{`description`}{Optional prose, appended in parentheses to
#'       `detail` so a finding says what the rule is for.}
#'   }
#'
#' @return A [qc_issues] tibble with one `issue = "aggregate_mismatch"` row per
#'   offending data row — not one per group — so `row` and `col_index` stay
#'   usable for highlighting the offending cells. `value` holds the row's own
#'   value of `value_column`; `detail` carries the group-level explanation, e.g.
#'   `"sum(point_count) = 30, expected == points_in_quadrat = 81"`, or
#'   `"n_distinct(quadrat) = 4, expected == 5"` for a literal target.
#'
#' @details
#' Malformed metadata never aborts a run. A rule naming a column that is absent,
#' an unknown aggregate or comparison, a non-numeric `value_column`, a
#' `target_column` that varies within a group, or an ambiguous target
#' specification each emit a warning and either skip the rule or report it as a
#' single `"warn"` row with `issue = "unevaluable_rule"` and `row = NA`. That
#' distinguishes "this rule could not be checked" from "this data is clean" —
#' the conflation that the hard-coded predecessor of this check made silently.
#'
#' @seealso [qc_run()], [qc_issues]
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   site_name = "Site A",
#'   transect = 1,
#'   quadrat = 5,
#'   point_count = c(9, 13, 20, 34, 4),
#'   points_in_quadrat = 81
#' )
#'
#' rules <- data.frame(
#'   group_columns = "site_name|transect|quadrat",
#'   value_column = "point_count",
#'   aggregate_function = "sum",
#'   comparison = "==",
#'   target_column = "points_in_quadrat",
#'   target_value = NA_real_,
#'   tolerance = NA_real_,
#'   severity = "fail",
#'   stringsAsFactors = FALSE
#' )
#'
#' # The five points sum to 80, not 81.
#' qc_check_aggregate(data, rules)
qc_check_aggregate <- function(data, rules) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop(
      "`rules` must be a data frame with columns `group_columns`, ",
      "`value_column`, `aggregate_function`, and `comparison`."
    )
  }
  required_cols <- c(
    "group_columns",
    "value_column",
    "aggregate_function",
    "comparison"
  )
  if (!all(required_cols %in% colnames(rules))) {
    stop("`rules` must have columns: ", paste(required_cols, collapse = ", "))
  }

  chunks <- lapply(seq_len(nrow(rules)), function(i) {
    .qc_aggregate_rule(data, rules[i, , drop = FALSE])
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_aggregate"
  )
}

# Evaluate one aggregate rule. Returns NULL (no issues) or a .qc_issue() chunk.
.qc_aggregate_rule <- function(data, rule) {
  rule_id <- .qc_rule_field(rule, "rule_id", NA_character_)
  label <- if (is.na(rule_id)) {
    "aggregate rule"
  } else {
    paste0("rule '", rule_id, "'")
  }

  severity <- .qc_rule_field(rule, "severity", "fail")
  if (is.na(severity) || !severity %in% c("fail", "warn")) {
    severity <- "fail"
  }

  group_cols <- .qc_split_list(rule$group_columns[[1]])
  value_col <- rule$value_column[[1]]
  agg_name <- rule$aggregate_function[[1]]
  cmp_name <- rule$comparison[[1]]

  unevaluable <- function(issue_detail) {
    warning("QC ", label, " was not applied: ", issue_detail)
    .qc_issue(
      check = "qc_check_aggregate",
      severity = "warn",
      issue = "unevaluable_rule",
      column = if (is.na(value_col)) NA_character_ else value_col,
      rule_id = rule_id,
      detail = issue_detail
    )
  }

  # --- Vocabulary ------------------------------------------------------------
  if (is.na(agg_name) || !agg_name %in% names(.qc_aggregate_functions)) {
    return(unevaluable(paste0(
      "unknown aggregate_function '",
      agg_name,
      "'. Valid: ",
      paste(names(.qc_aggregate_functions), collapse = ", "),
      "."
    )))
  }
  if (is.na(cmp_name) || !cmp_name %in% names(.qc_aggregate_comparisons)) {
    return(unevaluable(paste0(
      "unknown comparison '",
      cmp_name,
      "'. Valid: ",
      paste(names(.qc_aggregate_comparisons), collapse = ", "),
      "."
    )))
  }

  # --- Target specification: exactly one of column or literal ----------------
  target_col <- .qc_rule_field(rule, "target_column", NA_character_)
  target_val <- .qc_rule_field(rule, "target_value", NA)
  if (!is.na(target_col) && !nzchar(trimws(target_col))) {
    target_col <- NA_character_
  }
  has_col <- !is.na(target_col)
  has_val <- !is.na(target_val)

  if (has_col && has_val) {
    return(unevaluable(
      "target_column and target_value are both set; exactly one is required."
    ))
  }
  if (!has_col && !has_val) {
    return(unevaluable(
      "neither target_column nor target_value is set; exactly one is required."
    ))
  }

  # --- Columns present -------------------------------------------------------
  needed <- c(group_cols, value_col, if (has_col) target_col)
  missing <- setdiff(needed, colnames(data))
  if (length(missing) > 0) {
    return(unevaluable(paste0(
      "the data is missing column(s) ",
      paste(missing, collapse = ", "),
      "."
    )))
  }
  if (nrow(data) == 0L) {
    return(NULL)
  }

  values <- data[[value_col]]
  if (!agg_name %in% c("n", "n_distinct") && !is.numeric(values)) {
    return(unevaluable(paste0(
      "value_column '",
      value_col,
      "' is not numeric, so '",
      agg_name,
      "' cannot be computed."
    )))
  }

  # --- Aggregate within group ------------------------------------------------
  key <- .qc_group_key(data, group_cols)
  per_group <- tapply(values, key, .qc_aggregate_functions[[agg_name]])
  observed <- as.numeric(per_group[key])

  # --- Expected value --------------------------------------------------------
  if (has_col) {
    expected_raw <- data[[target_col]]
    if (!is.numeric(expected_raw)) {
      expected_raw <- suppressWarnings(as.numeric(expected_raw))
    }
    # A target that varies inside a group has no single meaning, so the rule is
    # reported as unevaluable rather than silently compared against whichever
    # value happens to sit on each row.
    varies <- tapply(
      expected_raw,
      key,
      function(x) length(unique(x[!is.na(x)])) > 1L
    )
    if (any(varies, na.rm = TRUE)) {
      return(unevaluable(paste0(
        "target_column '",
        target_col,
        "' is not constant within the group (",
        paste(group_cols, collapse = ", "),
        ")."
      )))
    }
    expected <- expected_raw
    target_label <- target_col
  } else {
    expected <- rep(suppressWarnings(as.numeric(target_val)), nrow(data))
    # A literal target names nothing, so there is no label to print: "expected
    # == 5" reads correctly where "expected == expected value = 5" does not.
    target_label <- NA_character_
  }

  tol <- suppressWarnings(as.numeric(.qc_rule_field(rule, "tolerance", NA)))
  if (is.na(tol)) {
    tol <- 0
  }

  holds <- .qc_aggregate_comparisons[[cmp_name]](observed, expected, tol)
  bad <- which(!holds & !is.na(holds))
  if (length(bad) == 0L) {
    return(NULL)
  }

  # A column target is worth naming ("expected == points_in_quadrat = 81");
  # a literal one is just the number ("expected == 5").
  expected_text <- if (is.na(target_label)) {
    .qc_fmt_num(expected[bad])
  } else {
    paste0(target_label, " = ", .qc_fmt_num(expected[bad]))
  }

  # The rule's own prose, when metadata carries it, says what the rule is
  # *for* -- "5 quadrats per transect" rather than a bare arithmetic mismatch.
  why <- .qc_rule_field(rule, "description", NA_character_)
  suffix <- if (is.na(why) || !nzchar(trimws(why))) {
    ""
  } else {
    paste0(" (", trimws(why), ")")
  }

  .qc_issue(
    check = "qc_check_aggregate",
    severity = severity,
    issue = "aggregate_mismatch",
    row = bad,
    column = value_col,
    col_index = which(colnames(data) == value_col),
    value = as.character(values[bad]),
    rule_id = rule_id,
    detail = paste0(
      agg_name,
      "(",
      value_col,
      ") = ",
      .qc_fmt_num(observed[bad]),
      ", expected ",
      cmp_name,
      " ",
      expected_text,
      suffix
    )
  )
}
