#' Run all applicable QC tests on a MarineGEO data table
#'
#' @description
#' Single entry point for the MarineGEO QA/QC test suite. Given a data object
#' and a `table_id`, `qc_run()` queries `marinegeo_metadata` to determine
#' which tests apply, assembles test parameters from metadata, and returns a
#' structured result list.
#'
#' Accepts a data frame, a file path (CSV, XLSX, Parquet), or an Arrow
#' Dataset/Table.
#'
#' With `partial = TRUE` it also accepts an arbitrary **subset** of a table's
#' columns, running only the checks that work on a column in isolation. To
#' validate a single field, see [qc_check_variable()].
#'
#' @param x A data frame, tibble, file path (character string), Arrow Dataset,
#'   or Arrow Table.
#' @param table_id Character scalar. The MarineGEO table identifier (from
#'   `marinegeo_metadata$data_index$table_id`). Used to look up expected
#'   columns, data types, and controlled vocabularies.
#' @param sheet Integer or character. Only used when `x` is an `.xlsx` or
#'   `.xls` file path. Passed to [readxl::read_excel()]. Defaults to `1`.
#' @param partial Logical scalar. If `FALSE` (the default), `x` is treated as a
#'   complete table and all applicable checks run. If `TRUE`, `x` is treated as
#'   a subset of the table's columns: the three table-scoped checks
#'   (`qc_check_columns()`, `qc_check_row_uniqueness()`,
#'   `qc_check_aggregate()`) are skipped rather than reporting the absent
#'   columns as failures, and are listed in the result's `checks_skipped`
#'   attribute.
#' @param verbose Logical scalar. If `TRUE` (the default), a summary of the run
#'   is written to the console with [message()]: one line per check that ran
#'   with its status (`PASS`, `WARN` or `FAIL`) and issue count, then an overall
#'   status footer. Purely informational — it does not affect the returned
#'   object. Use `verbose = FALSE` or wrap the call in [suppressMessages()] to
#'   silence it.
#'
#' @return A [qc_issues] tibble: one row per detected problem across all
#'   applicable checks (zero rows if the data is clean), with the producing
#'   check in the `check` column. Run-level metadata rides along as attributes
#'   — `table_id`, `n_rows` (rows validated), `checks_run` (which checks
#'   executed), `checks_skipped` (checks not applicable to the input),
#'   `columns` (the columns validated), and `status` (the worst severity
#'   present: `"fail"` > `"warn"` > `"pass"`). See [qc_issues] for the full
#'   column description.
#'
#' @details
#' **Metadata-driven dispatch:** Tests are not hard-coded to table types.
#' Instead, the dispatcher queries `marinegeo_metadata` for the given
#' `table_id` and infers which tests to run:
#' \itemize{
#'   \item Column presence/order test: runs if `database_structure` has rows
#'     for `table_id`.
#'   \item Data type test: runs if `database_structure` has rows with non-`NA`
#'     `data_type` values for `table_id`.
#'   \item Categorical values test: runs if `categorical_values` has rows for
#'     `table_id`.
#'   \item Missing values test: runs if `database_structure` has rows with
#'     `missing_values` equal to `"enforce"` or `"warn"` for `table_id`.
#'   \item Numeric ranges test: runs if `numeric_ranges` has rows with non-`NA`
#'     `range_type` for `table_id`.
#'   \item Grouped aggregate test: runs if `aggregate_rules` has rows for
#'     `table_id`.
#'   \item Lookup values test: runs if `data` contains any of the columns
#'     `partner_code`, `site_name`, `site_code`, or `scientific_name`, which are
#'     validated against the corresponding global entity registries in
#'     `marinegeo_metadata` (`partner_codes`, `site_codes`,
#'     `observation_lookup`).
#' }
#' Adding a new table type requires only updating the metadata CSVs and
#' rebuilding sysdata — no R code changes required.
#'
#' **Partial tables:** under `partial = TRUE` each rule set is additionally
#' narrowed to the columns actually supplied, so a check appears in
#' `checks_run` only when a rule applied to one of them. If none of the
#' supplied columns are defined for `table_id`, a warning is emitted — an empty
#' result there means "nothing was checkable", not "clean".
#'
#' **Console report:** because the result holds one row per *problem*, a clean
#' run is an empty tibble, which is indistinguishable at a glance from a run
#' where nothing was checked. With `verbose = TRUE` the run therefore reports
#' which checks executed and how each fared, so a passing check is visible
#' rather than merely silent. A run where no checks applied reports
#' `NOT CHECKED` instead of a pass.
#'
#' **Arrow support:** Arrow Datasets and Tables are collected into memory via
#' [dplyr::collect()] before testing. This requires the `arrow` package.
#'
#' **File path support:** CSV files are read with [readr::read_csv()]; XLSX/XLS
#' files with [readxl::read_excel()]; Parquet files with
#' [arrow::read_parquet()] (requires `arrow`).
#'
#' @seealso [qc_check_variable()] to validate a single variable's values.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   site_name       = "Example Site",
#'   survey_date     = as.Date("2024-06-01"),
#'   scientific_name = "Zostera marina",
#'   percent_cover   = 45.2
#' )
#'
#' # Supply a valid table_id from marinegeo_metadata$data_index
#' # result <- qc_run(df, table_id = "sav_cover_v1")
#'
#' # Validate only the columns present, ignoring the rest of the table
#' # result <- qc_run(df, table_id = "sav_cover_v1", partial = TRUE)
qc_run <- function(x, table_id, sheet = 1L, partial = FALSE, verbose = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(table_id) || length(table_id) != 1) {
    stop("`table_id` must be a single character string.")
  }
  if (!is.logical(partial) || length(partial) != 1L || is.na(partial)) {
    stop("`partial` must be a single `TRUE` or `FALSE`.")
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be a single `TRUE` or `FALSE`.")
  }

  # --- Ingest: resolve x to a data frame -------------------------------------
  data <- .qc_ingest(x, sheet = sheet)

  # --- Dispatch ---------------------------------------------------------------
  result <- .qc_dispatch(
    data,
    table_id = table_id,
    scope = if (partial) "column" else "table"
  )

  # --- Report -----------------------------------------------------------------
  # Derived from `result`, so this single call site covers every way
  # .qc_dispatch() can return (including its two early exits).
  .qc_report(result, verbose = verbose)

  result
}
# ---------------------------------------------------------------------------
# Internal: resolve x to a plain data frame
# ---------------------------------------------------------------------------
.qc_ingest <- function(x, sheet = 1L) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("File not found: '", x, "'")
    }
    ext <- tolower(tools::file_ext(x))

    if (ext == "csv") {
      return(readr::read_csv(x, show_col_types = FALSE))
    } else if (ext %in% c("xlsx", "xls")) {
      return(readxl::read_excel(x, sheet = sheet))
    } else if (ext == "parquet") {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop(
          "The 'arrow' package is required to read Parquet files. ",
          "Install it with: install.packages('arrow')"
        )
      }
      return(arrow::read_parquet(x))
    } else {
      stop(
        "Unsupported file extension '.",
        ext,
        "'. ",
        "Supported: csv, xlsx, xls, parquet."
      )
    }
  }

  # Arrow Dataset or Table
  if (
    inherits(
      x,
      c(
        "ArrowObject",
        "Dataset",
        "arrow_dplyr_query",
        "RecordBatch",
        "Table",
        "ArrowTabular"
      )
    )
  ) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop(
        "The 'arrow' package is required to process Arrow objects. ",
        "Install it with: install.packages('arrow')"
      )
    }
    return(dplyr::collect(x))
  }

  stop(
    "`x` must be a data frame, a file path (character), or an Arrow object. ",
    "Got: ",
    paste(class(x), collapse = "/")
  )
}
# ---------------------------------------------------------------------------
# Internal: metadata-driven dispatch
#
# `scope = "table"` treats `data` as a complete table and runs every
# applicable check. `scope = "column"` treats it as a subset of columns: only
# the column-scoped checks run, each narrowed to the columns supplied. See
# R/qc_mg_rules.R for the scope declaration and the rule resolver.
# ---------------------------------------------------------------------------
.qc_dispatch <- function(data, table_id, scope = c("table", "column")) {
  scope <- match.arg(scope)

  rules <- .qc_resolve_rules(table_id)
  tbl_struct <- rules$structure
  tbl_cats <- rules$categorical
  tbl_ranges <- rules$ranges
  tbl_conds <- rules$conditions
  tbl_aggs <- rules$aggregates
  lookup_map <- rules$lookups

  table_scope <- identical(scope, "table")
  checks_skipped <- if (table_scope) {
    character(0)
  } else {
    .qc_table_scoped_checks()
  }

  # Conditional rules dropped because a predicate column was not supplied.
  # Populated in column scope only; empty_result() closes over it, so it is
  # declared before that closure.
  rules_skipped <- character(0)

  empty_result <- function() {
    new_qc_issues(
      table_id = table_id,
      n_rows = nrow(data),
      checks_run = character(0),
      checks_skipped = checks_skipped,
      columns = colnames(data),
      rules_skipped = rules_skipped
    )
  }

  if (nrow(tbl_struct) == 0 && nrow(tbl_cats) == 0) {
    warning(
      "No metadata found for table_id '",
      table_id,
      "'. ",
      "No tests will be run. ",
      "Check `marinegeo_metadata$data_index` for valid table_id values."
    )
    return(empty_result())
  }

  # In column scope every rule set is narrowed to the columns actually
  # supplied, so a check lands in `checks_run` only when a rule applied to one
  # of them.
  if (!table_scope) {
    present <- colnames(data)
    tbl_struct <- .qc_filter_by_column(tbl_struct, present)
    tbl_cats <- .qc_filter_by_column(tbl_cats, present)
    tbl_ranges <- .qc_filter_by_column(tbl_ranges, present)

    # A conditional rule needs its predicate column too, which a column subset
    # may not include. Such a rule is dropped rather than applied as though it
    # were unconditional, and reported in `rules_skipped`. This is the path
    # qc_check_variable() takes, where only one column is ever supplied.
    cats_avail <- .qc_filter_unavailable_conditions(
      tbl_cats,
      tbl_conds,
      present
    )
    ranges_avail <- .qc_filter_unavailable_conditions(
      tbl_ranges,
      tbl_conds,
      present
    )
    tbl_cats <- cats_avail$rules
    tbl_ranges <- ranges_avail$rules
    rules_skipped <- unique(c(cats_avail$skipped, ranges_avail$skipped))

    nothing_checkable <- nrow(tbl_struct) == 0 &&
      nrow(tbl_cats) == 0 &&
      nrow(tbl_ranges) == 0 &&
      length(intersect(names(lookup_map), present)) == 0
    if (nothing_checkable) {
      warning(
        "None of the supplied column(s) (",
        paste(present, collapse = ", "),
        ") are defined for table_id '",
        table_id,
        "'. No tests were run. ",
        "Call `utl_mg_column_order('",
        table_id,
        "')` to list its columns."
      )
      return(empty_result())
    }
  }

  results <- list()

  # --- Test 1: column presence and order -------------------------------------
  if (table_scope && nrow(tbl_struct) > 0) {
    results$qc_check_columns <- qc_check_columns(
      data = data,
      expected_columns = tbl_struct$column_name
    )
  }

  # --- Test 2: data types ----------------------------------------------------
  if (nrow(tbl_struct) > 0) {
    type_rows <- tbl_struct[!is.na(tbl_struct$data_type), ]
    if (nrow(type_rows) > 0) {
      type_map <- stats::setNames(type_rows$data_type, type_rows$column_name)
      results$qc_check_data_types <- qc_check_data_types(
        data = data,
        type_map = type_map
      )
    }
  }

  # --- Test 3: categorical values --------------------------------------------
  # Tests 3-5 run through .qc_apply_conditional(), which splits the rules by
  # `condition_id`, runs the check once per condition against the matching rows
  # and remaps the reported row numbers. Rules without a condition (and rule
  # tables with no `condition_id` column at all) pass straight through, so this
  # is a no-op for unconditional metadata.
  if (nrow(tbl_cats) > 0) {
    results$qc_check_categorical_values <- .qc_apply_conditional(
      qc_check_categorical_values,
      data = data,
      rules = .qc_keep_cols(tbl_cats, c("column_name", "value")),
      conditions = tbl_conds
    )
  }

  # --- Test 4: missing values ------------------------------------------------
  miss_rows <- tbl_struct[tbl_struct$missing_values %in% c("enforce", "warn"), ]
  if (nrow(miss_rows) > 0) {
    results$qc_check_missing_values <- .qc_apply_conditional(
      qc_check_missing_values,
      data = data,
      rules = .qc_keep_cols(miss_rows, c("column_name", "missing_values")),
      conditions = tbl_conds
    )
  }

  # --- Test 5: numeric ranges ------------------------------------------------
  if (nrow(tbl_ranges) > 0) {
    results$qc_check_numeric_ranges <- .qc_apply_conditional(
      qc_check_numeric_ranges,
      data = data,
      rules = .qc_keep_cols(
        tbl_ranges,
        c(
          "column_name",
          "max_fail",
          "min_fail",
          "max_warn",
          "min_warn",
          "range_type"
        )
      ),
      conditions = tbl_conds
    )
  }

  # --- Test 6: lookup values ---------------------------------------------------
  present_lookup_cols <- intersect(names(lookup_map), colnames(data))
  if (length(present_lookup_cols) > 0) {
    results$qc_check_lookup_values <- qc_check_lookup_values(
      data = data,
      lookups = lookup_map[present_lookup_cols]
    )
  }

  # --- Test 7: row uniqueness ------------------------------------------------
  if (table_scope) {
    uuid_cols <- if ("uuid_identity" %in% colnames(tbl_struct)) {
      tbl_struct$column_name[which(tbl_struct$uuid_identity)]
    } else {
      character(0)
    }
    if (length(uuid_cols) > 0) {
      results$qc_check_row_uniqueness <- qc_check_row_uniqueness(
        data = data,
        id_cols = uuid_cols
      )
    }
  }

  # --- Test 8: grouped aggregates --------------------------------------------
  # Table-scoped: an aggregate rule needs its whole grouping key present, which
  # a column subset cannot guarantee. Which rules exist comes from metadata, so
  # this no longer sniffs for particular column names the way the hard-coded
  # point-count check did.
  if (table_scope && nrow(tbl_aggs) > 0) {
    results$qc_check_aggregate <- .qc_apply_conditional(
      qc_check_aggregate,
      data = data,
      rules = tbl_aggs,
      conditions = tbl_conds
    )
  }

  # --- Combine into a single issues table ------------------------------------
  # Each check returns a qc_issues tibble; bind them and re-wrap with run-level
  # metadata. `status` is derived from the combined rows (fail > warn > pass).
  new_qc_issues(
    dplyr::bind_rows(results),
    table_id = table_id,
    n_rows = nrow(data),
    checks_run = names(results),
    checks_skipped = checks_skipped,
    columns = colnames(data),
    rules_skipped = rules_skipped
  )
}
