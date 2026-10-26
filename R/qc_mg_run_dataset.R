#' Run QC across several related MarineGEO tables at once
#'
#' @description
#' Validates a set of tables together: every check [qc_run()] performs on each
#' table individually, plus the cross-table relations that can only be seen with
#' both tables in hand — "every quadrat surveyed in the quadrat table has shoot
#' records in the shoots table".
#'
#' A relation is a property of a *pair* of tables, so it cannot be dispatched
#' from [qc_run()], which is handed one table and has no way to reach the other.
#' This is the entry point a submission check wants; [qc_run()] remains the
#' single-table contract.
#'
#' @param tables A named list of tables to validate. Each name must be a
#'   MarineGEO `table_id`, and each element may be anything [qc_run()] accepts:
#'   a data frame, a file path (CSV, XLSX, Parquet), or an Arrow Dataset/Table.
#' @param sheet Integer or character. Passed to [readxl::read_excel()] for any
#'   element that is an `.xlsx`/`.xls` path. Defaults to `1`.
#' @param verbose Logical scalar. If `TRUE` (the default), each table's run
#'   reports to the console as it happens — see [qc_run()] — followed by one
#'   report for the dataset as a whole, covering the cross-table checks and the
#'   combined status. Purely informational. Use `verbose = FALSE` or
#'   [suppressMessages()] to silence it.
#'
#' @return A [qc_issues] tibble combining every table's issues with the
#'   cross-table ones, where the `table` column names the table each issue is
#'   in. Run-level metadata rides along as attributes: `table_id` (the vector of
#'   validated identifiers), `n_rows` (total rows across all tables),
#'   `checks_run`, `checks_skipped`, `rules_skipped`, `columns` (a named list of
#'   each table's columns) and `status`.
#'
#' @details
#' **Which relations run.** Rules come from `cross_table_rules` metadata. A rule
#' runs only when *both* of its tables are supplied; one naming a table that is
#' absent from `tables` is skipped rather than failed, and
#' `"qc_check_cross_table"` is then added to the `checks_skipped` attribute so a
#' partial submission does not look fully checked.
#'
#' **Per-table checks** are exactly those of [qc_run()] with `partial = FALSE`,
#' including the warning each emits for a `table_id` with no metadata.
#'
#' @seealso [qc_run()] for a single table, [qc_check_cross_table()] for one
#'   relation, and [qc_issues] for the result format.
#'
#' @export
#'
#' @examples
#' # qc_run_dataset(list(
#' #   `seagrass-quadrat-v1` = df_quadrat,
#' #   `seagrass-shoots-v1`  = df_shoots
#' # ))
#'
#' # File paths work the same way:
#' # qc_run_dataset(list(`seagrass-shoots-v1` = "shoots.csv"))
qc_run_dataset <- function(tables, sheet = 1L, verbose = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.list(tables) || is.data.frame(tables)) {
    stop(
      "`tables` must be a named list of tables, one per table_id. ",
      "To validate a single table, use `qc_run()`."
    )
  }
  if (length(tables) == 0L) {
    stop("`tables` must contain at least one table.")
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be a single `TRUE` or `FALSE`.")
  }

  ids <- names(tables)
  if (is.null(ids) || any(is.na(ids)) || !all(nzchar(ids))) {
    stop(
      "Every element of `tables` must be named with its MarineGEO table_id. ",
      "Call `utl_mg_list_tables()` to see the available identifiers."
    )
  }
  if (anyDuplicated(ids) > 0L) {
    stop(
      "`tables` has duplicate table_id name(s): ",
      paste(unique(ids[duplicated(ids)]), collapse = ", "),
      "."
    )
  }

  # Ingest once. The cross-table checks need the resolved data frames, and
  # re-reading a file per rule would be wasteful and could even disagree.
  data_list <- lapply(tables, .qc_ingest, sheet = sheet)
  names(data_list) <- ids

  # --- Per-table checks -------------------------------------------------------
  per_table <- lapply(ids, function(id) {
    out <- qc_run(data_list[[id]], table_id = id, verbose = verbose)
    if (nrow(out) > 0) {
      out$table <- id
    }
    out
  })

  # --- Cross-table relations --------------------------------------------------
  rule_sets <- .qc_cross_table_rule_sets(ids)
  conditions <- .mg_get_registry_table("qc_conditions")
  if (!is.data.frame(conditions)) {
    conditions <- NULL
  }

  cross <- list()
  if (nrow(rule_sets$applicable) > 0) {
    cross <- lapply(seq_len(nrow(rule_sets$applicable)), function(i) {
      rule <- rule_sets$applicable[i, , drop = FALSE]
      qc_check_cross_table(
        left = data_list[[rule$left_table_id[[1]]]],
        right = data_list[[rule$right_table_id[[1]]]],
        rule = rule,
        conditions = conditions
      )
    })
  }

  # --- Combine ----------------------------------------------------------------
  gather <- function(results, attribute) {
    out <- unique(unlist(
      lapply(results, function(x) attr(x, attribute)),
      use.names = FALSE
    ))
    if (is.null(out)) character(0) else out
  }

  checks_skipped <- gather(per_table, "checks_skipped")
  if (nrow(rule_sets$incomplete) > 0) {
    # Metadata defines a relation involving one of these tables, but its
    # counterpart was not supplied, so it went unchecked rather than passing.
    checks_skipped <- unique(c(checks_skipped, "qc_check_cross_table"))
  }

  columns <- lapply(data_list, colnames)
  names(columns) <- ids

  result <- new_qc_issues(
    dplyr::bind_rows(c(per_table, cross)),
    table_id = ids,
    n_rows = sum(vapply(data_list, nrow, integer(1))),
    checks_run = gather(c(per_table, cross), "checks_run"),
    checks_skipped = checks_skipped,
    columns = columns,
    rules_skipped = gather(per_table, "rules_skipped"),
    # Recomputed rather than gathered: the cross-table rules are resolved here,
    # after the per-table runs stamped their own attribute.
    stale_registries = .mg_stale_registry_tables(.qc_registry_tables)
  )

  # Dataset-level report, after the per-table ones each qc_run() already
  # emitted: this is where the cross-table checks and the combined status show.
  .qc_report(result, verbose = verbose)

  result
}
