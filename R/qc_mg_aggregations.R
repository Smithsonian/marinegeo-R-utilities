# ---------------------------------------------------------------------------
# Aggregation QC: checks that validate an aggregate over a GROUP of rows
# rather than a single cell.
#
# Unlike every other check in this package, aggregation rules are authored in R
# -- one function per table, in R/qc_mg_agg_*.R -- rather than resolved from
# `marinegeo-metadata`. Grouped logic reads far better as dplyr than as rule
# table columns, and the three tables covered here each want materially
# different logic: a taxon exclusion for oyster substrate, a method filter for
# seagrass cover, and neither for fouling panels. The retired metadata-driven
# predecessor needed a rule grammar plus a second row-filter table to express
# the first of those, and could not express it at all once rules were re-keyed
# to `column_id`.
#
# Nothing here reads a registry table, so an aggregation result is never stale
# and no entry is needed in `.qc_registry_tables`.
# ---------------------------------------------------------------------------

# The registered aggregations, keyed by `table_id`.
#
# A function rather than a top-level list on purpose: a list literal referencing
# the `.qc_agg_*` functions would be evaluated when the package is installed, in
# collation order, which is locale-sensitive. Resolving at call time means the
# file this registry lives in can be collated before or after the functions it
# names.
#
# Each spec carries the hand-written function plus the little the shared wrapper
# needs to build issue rows:
#   fn              - takes `data`, returns a tidy frame of FAILING GROUPS only
#   requires        - columns the check needs; absent ones mean it does not apply
#   column          - the column an issue is reported against
#   expected_column - the column holding each group's declared total
#   label           - how the aggregate reads in `detail`, e.g. "sum(point_count)"
#   issue           - default issue code, when `fn` does not set one per row
#   severity        - "fail" or "warn"
.qc_aggregations <- function() {
  list(
    `oyster-substrate-composition-monitoring-v1` = list(
      fn = .qc_agg_oyster_substrate_points,
      requires = c(
        "sample_event_id",
        "transect",
        "quadrat",
        "sample_collection_date",
        "cover_type",
        "point_count",
        "points_in_quadrat"
      ),
      column = "point_count",
      expected_column = "points_in_quadrat",
      label = "sum(point_count)",
      issue = "aggregate_mismatch",
      severity = "fail"
    ),
    `seagrass-cover-monitoring-v1` = list(
      fn = .qc_agg_seagrass_cover_points,
      requires = c(
        "sample_event_id",
        "transect",
        "quadrat",
        "sample_collection_date",
        "cover_method",
        "point_count",
        "points_in_quadrat"
      ),
      column = "point_count",
      expected_column = "points_in_quadrat",
      label = "sum(point_count)",
      issue = "aggregate_mismatch",
      severity = "fail"
    ),
    `fouling-cover-v1` = list(
      fn = .qc_agg_fouling_cover_points,
      requires = c(
        "sample_event_id",
        "panel_id",
        "deployment_period",
        "point_count",
        "points_in_grid"
      ),
      column = "point_count",
      expected_column = "points_in_grid",
      label = "sum(point_count)",
      issue = "aggregate_mismatch",
      severity = "fail"
    )
  )
}

# Does this table_id have a registered aggregation? Used by .qc_dispatch() in
# place of the "does the rule table have rows" gate every other check uses.
.qc_has_aggregations <- function(table_id) {
  length(table_id) == 1L &&
    !is.na(table_id) &&
    table_id %in% names(.qc_aggregations())
}

# A group's declared total: its single non-NA value, or NA where the group
# declares none.
#
# `max(x, na.rm = TRUE)` would do the same job, but on an all-NA group it warns
# and returns -Inf. A QC check must not emit warnings over ordinary missing
# data, and -Inf would then have to be filtered as carefully as NA.
.qc_agg_declared <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    NA_real_
  } else {
    as.numeric(x[[1]])
  }
}

# Turn a tidy frame of failing groups into qc_issues rows.
#
# One issue per group, not per member row: `row` is NA because the finding spans
# a group, and the group key goes into `detail`. `col_index` still points at the
# offending column, so a consumer can highlight the column even though it has no
# single row to highlight.
#
# Group columns are whatever `fn` returned that is not one of the three reserved
# names, so a per-table function names its own grouping and nothing has to be
# declared twice.
.qc_aggregation_issues <- function(spec, failures, data) {
  reserved <- c("observed", "expected", "issue")
  group_cols <- setdiff(names(failures), reserved)

  labels <- if (length(group_cols) == 0L) {
    rep(NA_character_, nrow(failures))
  } else {
    col_strs <- lapply(group_cols, function(cn) {
      paste0(cn, "=", as.character(failures[[cn]]))
    })
    do.call(paste, c(col_strs, list(sep = ", ")))
  }

  issue <- if ("issue" %in% names(failures)) {
    as.character(failures$issue)
  } else {
    rep(as.character(spec$issue), nrow(failures))
  }

  # An inconsistent total has no single expected value to report, so it gets its
  # own sentence rather than one claiming a total the group does not agree on.
  detail <- ifelse(
    issue == "inconsistent_total",
    paste0(
      labels,
      ": ",
      spec$expected_column,
      " is not constant within the group; ",
      spec$label,
      " = ",
      as.character(failures$observed)
    ),
    paste0(
      labels,
      ": ",
      spec$label,
      " = ",
      as.character(failures$observed),
      ", expected ",
      spec$expected_column,
      " = ",
      as.character(failures$expected)
    )
  )

  .qc_issue(
    check = "qc_check_aggregation",
    severity = spec$severity,
    issue = issue,
    column = spec$column,
    col_index = which(colnames(data) == spec$column)[1],
    value = as.character(failures$observed),
    detail = detail
  )
}

#' Check that grouped aggregates match their declared totals
#'
#' @description
#' Validates an aggregate computed over a *group* of rows, rather than a single
#' value in a single row. The archetype is point-count data: the counts recorded
#' for each taxon or cover type in a quadrat must sum to the number of points
#' that quadrat declares.
#'
#' Which aggregation applies is looked up by `table_id` in an in-package
#' registry of hand-written functions, not in `marinegeo_metadata`. Three tables
#' are covered:
#' \describe{
#'   \item{`oyster-substrate-composition-monitoring-v1`}{`point_count` must sum
#'     to `points_in_quadrat` per quadrat, **excluding** `cover_type ==
#'     "canopy taxa"` — canopy is scored over the top of the primary layer and
#'     is not part of the primary total.}
#'   \item{`seagrass-cover-monitoring-v1`}{`point_count` must sum to
#'     `points_in_quadrat` per quadrat, for those quadrats recorded with
#'     `cover_method == "point count"` only. The other methods
#'     (Braun-Blanquet, percent bins, visual estimation) record no points and
#'     are not checked.}
#'   \item{`fouling-cover-v1`}{`point_count` must sum to `points_in_grid` per
#'     panel and deployment period.}
#' }
#'
#' @param data A data frame to validate. If any column the table's aggregation
#'   needs is absent, the check returns zero issues rather than erroring —
#'   missing columns are [qc_check_columns()]'s to report.
#' @param table_id Character scalar. The MarineGEO table identifier. Must be one
#'   of the tables listed above; any other value is an error, because an
#'   unregistered table silently returning a pass is indistinguishable from a
#'   genuine one.
#'
#' @return A [qc_issues] tibble with one row per offending **group** (zero rows
#'   if every group adds up), carrying one of two `issue` codes:
#'   \describe{
#'     \item{`"aggregate_mismatch"`}{The group's aggregate does not equal its
#'       declared total.}
#'     \item{`"inconsistent_total"`}{The group's rows do not agree on what the
#'       declared total is, so no comparison is meaningful.}
#'   }
#'   Both are `"fail"`. `row` is `NA` because a finding spans a group rather
#'   than sitting in one row; `column` and `col_index` identify the aggregated
#'   column (`point_count`); `value` holds the observed aggregate; and `detail`
#'   names the group key, the observed aggregate and the expected total.
#'   `rule_id` is `NA` — these issues come from code, not from a rule table.
#'
#' @details
#' A group whose declared total is missing throughout is **skipped**, not
#' failed: there is nothing to compare against. This matters for
#' `seagrass-cover-monitoring-v1`, where `point_count` and `points_in_quadrat`
#' are both optional and most partners leave them empty.
#'
#' `NA` counts are summed as zero rather than poisoning the total, so a group
#' with a missing count is reported short instead of passing silently.
#'
#' Grouping keys include the collection date. Depending on `sample_event_id`
#' alone would merge two visits to the same quadrat within one season, since
#' that identifier is season-level.
#'
#' This function is called automatically by [qc_run()] for a registered table.
#' It is table-scoped — it needs the whole grouping key — so it is skipped under
#' `qc_run(partial = TRUE)` and by [qc_check_variable()], and listed in the
#' result's `checks_skipped` attribute.
#'
#' @seealso [qc_run()] to run every applicable check; [qc_issues] for the result
#'   format.
#'
#' @export
#'
#' @examples
#' # Two fouling panels; the second is 10 points short of its grid.
#' panels <- data.frame(
#'   sample_event_id = "USA-XYZ_SITE-001_2025",
#'   panel_id = c("P1", "P1", "P1", "P2", "P2"),
#'   deployment_period = "3-month",
#'   point_count = c(40L, 30L, 30L, 50L, 40L),
#'   points_in_grid = 100L,
#'   stringsAsFactors = FALSE
#' )
#'
#' qc_check_aggregation(panels, table_id = "fouling-cover-v1")
qc_check_aggregation <- function(data, table_id) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(table_id) || length(table_id) != 1L || is.na(table_id)) {
    stop("`table_id` must be a single non-NA character string.")
  }

  specs <- .qc_aggregations()
  spec <- specs[[table_id]]
  if (is.null(spec)) {
    stop(
      "No aggregation check is defined for table_id '",
      table_id,
      "'. Defined for: ",
      paste(names(specs), collapse = ", "),
      "."
    )
  }

  empty <- function() {
    new_qc_issues(
      n_rows = nrow(data),
      checks_run = "qc_check_aggregation"
    )
  }

  # Missing columns are caught by qc_check_columns; an aggregation that cannot
  # see its grouping key simply does not apply. Same contract as
  # qc_check_row_uniqueness().
  if (!all(spec$requires %in% colnames(data)) || nrow(data) == 0L) {
    return(empty())
  }

  failures <- spec$fn(data)
  if (!is.data.frame(failures) || nrow(failures) == 0L) {
    return(empty())
  }

  new_qc_issues(
    .qc_aggregation_issues(spec, failures, data),
    n_rows = nrow(data),
    checks_run = "qc_check_aggregation"
  )
}
