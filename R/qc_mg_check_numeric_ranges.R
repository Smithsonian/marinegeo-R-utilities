#' Check numeric column values against expected ranges
#'
#' @description
#' Validates that numeric columns in a data frame fall within expected bounds
#' defined in `numeric_ranges` metadata. Supports both inclusive and exclusive
#' range types, separate fail and warn thresholds, taxonomy-aware rule scopes,
#' and per-column NA-safe evaluation. Designed for use in MarineGEO QA/QC
#' pipelines and can be called directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#' @param rules A data frame with columns `column_name`, `max_fail`,
#'   `min_fail`, `max_warn`, `min_warn`, `range_type`, `scope_type`, and
#'   `scope_value`. Each row specifies one range rule, and a column may carry
#'   more than one rule:
#'   \itemize{
#'     \item `range_type == "inclusive"`: fail if `value > max_fail` or
#'       `value < min_fail`; warn if outside warn bounds but within fail bounds.
#'     \item `range_type == "exclusive"`: fail if `value >= max_fail` or
#'       `value <= min_fail`; warn if outside warn bounds but within fail bounds.
#'     \item `range_type == NA`: row is silently skipped.
#'   }
#'   Bound columns (`max_fail`, `min_fail`, `max_warn`, `min_warn`) may be `NA`
#'   individually — only non-`NA` bounds are applied.
#'
#'   `scope_type` and `scope_value` are **required columns**, but both may be
#'   `NA`, which makes the rule unconditional. A non-`NA` `scope_type` is one of
#'   `"taxon_within"`, `"taxon_not_within"`, `"in_functional_group"`, or
#'   `"not_in_functional_group"`, and limits the rule to the rows whose
#'   `scientific_name` falls inside (or outside) the clade or functional group
#'   named by `scope_value` — see [utl_mg_expand_taxon_set()].
#'
#'   The optional columns `rule_id` and `description`, when present, are carried
#'   onto each issue as `rule_id` and `detail`.
#'
#' @return A [qc_issues] tibble with one row per out-of-range value
#'   (`issue = "out_of_range"`), or zero rows if all values are within range.
#'   Values breaching a fail threshold are `"fail"` rows; values breaching only
#'   a warn threshold are `"warn"` rows. `row` and `col_index` are 1-based, and
#'   `value` holds the offending number.
#'
#' @details
#' Only rules whose `column_name` exists in `data` are evaluated; the rest are
#' silently skipped. Rows in `rules` with `range_type == NA` are also silently
#' skipped.
#'
#' `NA` values in the data column being evaluated are silently ignored and not
#' counted as violations.
#'
#' Fail thresholds are evaluated first. A value that breaches a fail threshold
#' is recorded as `"fail"` and not additionally evaluated for warn. A value
#' that does not breach any fail threshold but breaches a warn threshold is
#' recorded as `"warn"`.
#'
#' A scoped rule is evaluated against the whole table and the out-of-scope rows
#' are masked out, so `row` is always the row number in `data`. A scope that
#' cannot be honoured — an unknown `scope_type`, an unresolvable `scope_value`,
#' no `scientific_name` column, or no taxonomy registry — warns and matches **no
#' rows**, so the rule reports nothing rather than firing on every species.
#'
#' `col_index` reflects the column's position in the original `data` argument,
#' not its rank among only the validated columns.
#'
#' This function is called automatically by [qc_run()] when `numeric_ranges`
#' metadata exists for the given `table_id` and has non-`NA` `range_type`
#' values.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   percent_cover = c(10, 50, 110, -5),
#'   depth_m       = c(1, 5, 20, 3)
#' )
#'
#' rules <- data.frame(
#'   column_name = c("percent_cover", "depth_m"),
#'   min_fail    = c(0,  NA),
#'   max_fail    = c(100, 50),
#'   min_warn    = c(NA, NA),
#'   max_warn    = c(80,  30),
#'   range_type  = c("inclusive", "inclusive"),
#'   scope_type  = c(NA_character_, NA_character_),
#'   scope_value = c(NA_character_, NA_character_),
#'   stringsAsFactors = FALSE
#' )
#'
#' # percent_cover 110 > 100 (fail), -5 < 0 (fail), 50 > 80 (warn within fail)
#' qc_check_numeric_ranges(df, rules)
qc_check_numeric_ranges <- function(data, rules) {
  # --- Input validation -------------------------------------------------------
  required_cols <- c(
    "column_name",
    "max_fail",
    "min_fail",
    "max_warn",
    "min_warn",
    "range_type",
    "scope_type",
    "scope_value"
  )
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop(
      "`rules` must be a data frame with columns ",
      paste0("`", required_cols, "`", collapse = ", "),
      "."
    )
  }
  # `scope_type`/`scope_value` are required even when every value is NA. Were
  # they optional, a scoped rule whose scope columns had been stripped upstream
  # would be indistinguishable from an unconditional rule, and would then be
  # applied to every row -- the one failure mode worth failing closed over.
  if (!all(required_cols %in% colnames(rules))) {
    stop("`rules` must have columns: ", paste(required_cols, collapse = ", "))
  }

  active_rules <- rules[!is.na(rules$range_type), , drop = FALSE]
  active_rules <- active_rules[
    active_rules$column_name %in% colnames(data),
    ,
    drop = FALSE
  ]

  # One iteration per rule row, not per column: a column may carry several
  # rules (say an unconditional floor plus a clade-specific band) and each has
  # to be evaluated on its own.
  chunks <- lapply(seq_len(nrow(active_rules)), function(i) {
    rule <- active_rules[i, , drop = FALSE]
    col <- rule$column_name
    col_pos <- which(colnames(data) == col)
    rtype <- rule$range_type
    values <- data[[col]]

    # The mask is over the full table, so valid_idx IS the row number in
    # `data` and the issue rows need no remapping.
    in_scope <- .qc_scope_mask(data, rule$scope_type, rule$scope_value)
    valid_idx <- which(!is.na(values) & in_scope)
    v <- values[valid_idx]

    over <- function(x, bound) {
      if (rtype == "inclusive") x > bound else x >= bound
    }
    under <- function(x, bound) {
      if (rtype == "inclusive") x < bound else x <= bound
    }

    over_max_fail <- !is.na(rule$max_fail) & over(v, rule$max_fail)
    under_min_fail <- !is.na(rule$min_fail) & under(v, rule$min_fail)
    fail_mask <- over_max_fail | under_min_fail

    over_max_warn <- !is.na(rule$max_warn) & over(v, rule$max_warn)
    under_min_warn <- !is.na(rule$min_warn) & under(v, rule$min_warn)
    warn_mask <- (over_max_warn | under_min_warn) & !fail_mask

    .range_issue <- function(mask, severity) {
      if (!any(mask)) {
        return(NULL)
      }
      rows <- valid_idx[mask]
      .qc_issue(
        check = "qc_check_numeric_ranges",
        severity = severity,
        issue = "out_of_range",
        row = rows,
        column = col,
        col_index = col_pos,
        value = as.character(values[rows]),
        rule_id = .qc_rule_field(rule, "rule_id"),
        detail = .qc_range_detail(rule)
      )
    }

    dplyr::bind_rows(
      .range_issue(fail_mask, "fail"),
      .range_issue(warn_mask, "warn")
    )
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_numeric_ranges"
  )
}

# A rule's optional character field, as a length-1 value, NA when the column is
# absent or blank.
.qc_rule_field <- function(rule, field) {
  if (!field %in% colnames(rule)) {
    return(NA_character_)
  }
  x <- as.character(rule[[field]])
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) NA_character_ else x
}

# What to put in an issue's `detail`: the rule's authored description, or, for a
# scoped rule without one, the scope itself, so a reader can see why the rule
# applied to these rows and not others.
.qc_range_detail <- function(rule) {
  desc <- .qc_rule_field(rule, "description")
  if (!is.na(desc)) {
    return(desc)
  }
  scope_type <- .qc_rule_field(rule, "scope_type")
  if (is.na(scope_type)) {
    return(NA_character_)
  }
  sprintf(
    "scientific_name %s %s",
    scope_type,
    .qc_rule_field(rule, "scope_value")
  )
}
