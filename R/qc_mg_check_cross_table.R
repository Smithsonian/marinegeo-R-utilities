# ---------------------------------------------------------------------------
# Cross-table rules: referential integrity between two MarineGEO tables.
#
# A relation is a property of the *pair*, which is why these rules are resolved
# by their own metadata table rather than hanging off either table's rule set,
# and why they are dispatched from qc_run_dataset() rather than qc_run().
#
# Keys are compared as composite character strings rather than with a
# dplyr::anti_join(). A metadata-driven join reaches across protocols whose
# shared columns are not guaranteed to share a storage type -- `quadrat` is
# TINYINT in some tables and INT in others, and a CSV-read column can arrive as
# character -- and a join would abort on the type mismatch. Comparing on
# as.character() makes 1L, 1.0 and "1" the same key, which is the intended
# semantics here.
# ---------------------------------------------------------------------------

.qc_cross_table_relations <- c("subset", "equal_set")

# Render a composite key for a human-readable message.
.qc_key_display <- function(key) {
  gsub("\r", "/", key, fixed = TRUE)
}

# Partition the relations touching a set of table_ids into those that can run
# and those that cannot.
#
# A rule is applicable only when BOTH its tables are supplied, which is what
# makes a partial submission skip the rule rather than fail it. The
# `incomplete` half is kept so the caller can report "not checked" instead of
# leaving the gap silent.
.qc_cross_table_rule_sets <- function(table_ids) {
  tbl <- .mg_get_registry_table("cross_table_rules")
  required <- c(
    "relation_type",
    "left_table_id",
    "right_table_id",
    "left_columns",
    "right_columns"
  )
  empty <- list(applicable = data.frame(), incomplete = data.frame())
  if (
    is.null(tbl) ||
      !is.data.frame(tbl) ||
      !all(required %in% colnames(tbl)) ||
      nrow(tbl) == 0
  ) {
    return(empty)
  }

  touches <- tbl$left_table_id %in%
    table_ids |
    tbl$right_table_id %in% table_ids
  both <- tbl$left_table_id %in% table_ids & tbl$right_table_id %in% table_ids

  list(
    applicable = tbl[both, , drop = FALSE],
    incomplete = tbl[touches & !both, , drop = FALSE]
  )
}

#' Check referential integrity between two tables
#'
#' @description
#' Validates that the key combinations recorded in one table are present in
#' another — "every quadrat surveyed in the quadrat table has shoot records in
#' the shoots table". Driven by `cross_table_rules` metadata and dispatched by
#' [qc_run_dataset()], which supplies both tables.
#'
#' @param left,right Data frames to compare.
#' @param rule A one-row data frame describing the relation, with columns:
#'   \describe{
#'     \item{`relation_type`}{`"subset"` — every `left` key must exist in
#'       `right`; or `"equal_set"` — the key sets must match both ways.}
#'     \item{`group_columns`}{`|`-delimited columns shared by both tables that
#'       scope the comparison, e.g. `"site_name|survey_date|transect"` for "a
#'       given sample event". May be empty for a whole-table relation.}
#'     \item{`left_columns`,`right_columns`}{`|`-delimited compared columns,
#'       paired positionally.}
#'     \item{`left_condition_id`,`right_condition_id`}{Optional `qc_conditions`
#'       ids restricting which rows of each side participate.}
#'     \item{`severity`}{`"fail"` or `"warn"`. Defaults to `"fail"`.}
#'     \item{`rule_id`,`left_table_id`,`right_table_id`,`description`}{Optional
#'       labels, reported in the result.}
#'   }
#' @param conditions Optional `qc_conditions` rows backing
#'   `left_condition_id` / `right_condition_id`. Each side's condition is
#'   evaluated against its own table.
#'
#' @return A [qc_issues] tibble with one row per unmatched record:
#'   `issue = "orphan_key"` for a `left` row with no counterpart in `right`,
#'   and `issue = "unmatched_key"` for a `right` row with no counterpart in
#'   `left` (`"equal_set"` only). `table` names the table the row is in, and
#'   `row` indexes that table.
#'
#' @details
#' Keys are compared as text, so a column stored as `INT` in one table and
#' `STRING` in another still matches. `NA` in a key component takes part in the
#' comparison like any other value — two rows that are `NA` in the same place
#' match — because a missing key is the business of
#' [qc_check_missing_values()], not of this check.
#'
#' A rule naming columns absent from either side, or with mismatched
#' `left_columns` / `right_columns` lengths, emits a warning and one `"warn"`
#' row with `issue = "unevaluable_rule"` and `row = NA`, rather than erroring.
#'
#' @seealso [qc_run_dataset()], [qc_issues]
#'
#' @export
#'
#' @examples
#' quadrats <- data.frame(
#'   site_name = "Site A",
#'   transect = 1,
#'   quadrat = c(1, 2, 3)
#' )
#' shoots <- data.frame(
#'   site_name = "Site A",
#'   transect = 1,
#'   quadrat = c(1, 1, 2)
#' )
#'
#' rule <- data.frame(
#'   rule_id = "quadrat_in_shoots",
#'   relation_type = "subset",
#'   left_table_id = "quadrats",
#'   right_table_id = "shoots",
#'   group_columns = "site_name|transect",
#'   left_columns = "quadrat",
#'   right_columns = "quadrat",
#'   severity = "fail",
#'   stringsAsFactors = FALSE
#' )
#'
#' # Quadrat 3 was surveyed but has no shoot records.
#' qc_check_cross_table(quadrats, shoots, rule)
qc_check_cross_table <- function(left, right, rule, conditions = NULL) {
  if (!is.data.frame(left) || !is.data.frame(right)) {
    stop("`left` and `right` must both be data frames.")
  }
  if (!is.data.frame(rule) || nrow(rule) != 1L) {
    stop("`rule` must be a one-row data frame.")
  }
  required <- c("relation_type", "left_columns", "right_columns")
  if (!all(required %in% colnames(rule))) {
    stop("`rule` must have columns: ", paste(required, collapse = ", "))
  }

  rule_id <- .qc_rule_field(rule, "rule_id", NA_character_)
  left_label <- .qc_rule_field(rule, "left_table_id", NA_character_)
  right_label <- .qc_rule_field(rule, "right_table_id", NA_character_)
  relation <- rule$relation_type[[1]]

  severity <- .qc_rule_field(rule, "severity", "fail")
  if (is.na(severity) || !severity %in% c("fail", "warn")) {
    severity <- "fail"
  }

  label <- if (is.na(rule_id)) {
    "cross-table rule"
  } else {
    paste0("cross-table rule '", rule_id, "'")
  }

  unevaluable <- function(issue_detail) {
    warning("QC ", label, " was not applied: ", issue_detail)
    new_qc_issues(
      .qc_issue(
        check = "qc_check_cross_table",
        severity = "warn",
        issue = "unevaluable_rule",
        rule_id = rule_id,
        detail = issue_detail,
        table = left_label
      ),
      n_rows = nrow(left),
      checks_run = "qc_check_cross_table"
    )
  }

  if (is.na(relation) || !relation %in% .qc_cross_table_relations) {
    return(unevaluable(paste0(
      "unknown relation_type '",
      relation,
      "'. Valid: ",
      paste(.qc_cross_table_relations, collapse = ", "),
      "."
    )))
  }

  group_cols <- .qc_split_list(.qc_rule_field(rule, "group_columns", NA))
  left_cols <- .qc_split_list(rule$left_columns[[1]])
  right_cols <- .qc_split_list(rule$right_columns[[1]])

  if (length(left_cols) == 0L || length(left_cols) != length(right_cols)) {
    return(unevaluable(
      "left_columns and right_columns must be non-empty and name the same number of columns."
    ))
  }

  missing_left <- setdiff(c(group_cols, left_cols), colnames(left))
  missing_right <- setdiff(c(group_cols, right_cols), colnames(right))
  if (length(missing_left) > 0 || length(missing_right) > 0) {
    return(unevaluable(paste0(
      "missing column(s): ",
      paste(
        c(
          if (length(missing_left) > 0) {
            paste0(
              "in the left table (",
              left_label,
              "): ",
              paste(missing_left, collapse = ", ")
            )
          },
          if (length(missing_right) > 0) {
            paste0(
              "in the right table (",
              right_label,
              "): ",
              paste(missing_right, collapse = ", ")
            )
          }
        ),
        collapse = "; "
      ),
      "."
    )))
  }

  # Each side's condition is evaluated against its own table, then the surviving
  # row positions are kept so findings can be reported against the full table.
  side_rows <- function(data, condition_id, tbl_id) {
    if (is.na(condition_id) || !nzchar(trimws(as.character(condition_id)))) {
      return(seq_len(nrow(data)))
    }
    cond_rows <- .qc_condition_rows(
      conditions,
      condition_id,
      table_id = tbl_id
    )
    if (nrow(cond_rows) == 0L) {
      warning(
        "QC ",
        label,
        " references condition_id '",
        condition_id,
        "' with no rows in `qc_conditions`; that side was not filtered."
      )
      return(seq_len(nrow(data)))
    }
    which(.qc_eval_condition(data, cond_rows))
  }

  left_idx <- side_rows(
    left,
    .qc_rule_field(rule, "left_condition_id", NA),
    left_label
  )
  right_idx <- side_rows(
    right,
    .qc_rule_field(rule, "right_condition_id", NA),
    right_label
  )

  left_key <- .qc_group_key(
    left[left_idx, , drop = FALSE],
    c(group_cols, left_cols)
  )
  right_key <- .qc_group_key(
    right[right_idx, , drop = FALSE],
    c(group_cols, right_cols)
  )

  described <- .qc_rule_field(rule, "description", NA_character_)

  side_issues <- function(
    data,
    idx,
    key,
    other_key,
    cols,
    other_cols,
    tbl_label,
    other_label,
    issue
  ) {
    unmatched <- which(!(key %in% other_key))
    if (length(unmatched) == 0L) {
      return(NULL)
    }
    rows <- idx[unmatched]
    detail <- paste0(
      "no matching row in '",
      other_label,
      "' for ",
      paste(c(group_cols, cols), collapse = "/"),
      " = ",
      .qc_key_display(key[unmatched]),
      if (!is.na(described) && nzchar(trimws(described))) {
        paste0(" (", trimws(described), ")")
      } else {
        ""
      }
    )
    .qc_issue(
      check = "qc_check_cross_table",
      severity = severity,
      issue = issue,
      row = rows,
      column = cols[1],
      col_index = which(colnames(data) == cols[1]),
      value = as.character(data[[cols[1]]][rows]),
      rule_id = rule_id,
      detail = detail,
      table = tbl_label
    )
  }

  chunks <- list(
    side_issues(
      left,
      left_idx,
      left_key,
      right_key,
      left_cols,
      right_cols,
      left_label,
      right_label,
      "orphan_key"
    )
  )

  if (identical(relation, "equal_set")) {
    chunks[[length(chunks) + 1L]] <- side_issues(
      right,
      right_idx,
      right_key,
      left_key,
      right_cols,
      left_cols,
      right_label,
      left_label,
      "unmatched_key"
    )
  }

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(left),
    checks_run = "qc_check_cross_table"
  )
}
