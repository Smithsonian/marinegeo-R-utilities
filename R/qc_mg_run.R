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
#'   a subset of the table's columns: the two table-scoped checks
#'   (`qc_check_columns()`, `qc_check_row_uniqueness()`) are skipped rather
#'   than reporting the absent columns as failures, and are listed in the
#'   result's `checks_skipped` attribute.
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
#'   \item Categorical values test: runs if `controlled_vocabularies` has rows
#'     for one of the `column_id` values this table defines.
#'   \item Missing values test: runs if `database_structure` has rows with
#'     `missing_values` equal to `"fail"` or `"warn"` for `table_id`.
#'   \item Numeric ranges test: runs if `numeric_ranges` has rows with non-`NA`
#'     `range_type` for one of this table's `column_id` values.
#'   \item Lookup values test: runs for each column this table binds to an
#'     entity registry in `column_lookups` (e.g. `site_code` to `site_codes`).
#'     A column absent from `column_lookups` gets no registry check, which is
#'     how a table using a partner's own coding opts out.
#' }
#' The rule tables are keyed on `column_id` -- what a column holds -- and
#' resolved to column names through this table's `database_structure` rows, so
#' two columns sharing a name but not a meaning no longer share rules.
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
  lookup_map <- rules$lookups
  lookup_scope_map <- rules$lookup_scopes

  table_scope <- identical(scope, "table")
  checks_skipped <- if (table_scope) {
    character(0)
  } else {
    .qc_table_scoped_checks()
  }

  # Scoped range rules dropped because their predicate column was not supplied.
  # Populated in column scope only; empty_result() closes over it, so it is
  # declared before that closure.
  rules_skipped <- character(0)

  # Rule tables that came from the offline snapshot rather than live metadata.
  # Recorded as a side effect of .qc_resolve_rules() above. A rule table that
  # could not be loaded is indistinguishable downstream from one with no rules,
  # so without this the result would claim a clean pass having checked nothing.
  stale_registries <- .mg_stale_registry_tables(.qc_registry_tables)

  # Which dist/ schema version those rule tables were published under, for the
  # report footer. NULL when no registry table was resolved this session.
  rules_schema <- .mg_rules_schema_label(.qc_registry_tables)

  empty_result <- function() {
    new_qc_issues(
      table_id = table_id,
      n_rows = nrow(data),
      checks_run = character(0),
      checks_skipped = checks_skipped,
      columns = colnames(data),
      rules_skipped = rules_skipped,
      stale_registries = stale_registries,
      rules_schema = rules_schema
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

    # A scoped range rule needs its predicate column, `scientific_name`, which
    # a column subset may not include. Such a rule is dropped rather than
    # applied as though it were unconditional -- that would validate
    # `leaf_length_mm` against Zostera bounds for every species -- and is
    # reported in `rules_skipped`. This is the path qc_check_variable() takes,
    # where only one column is ever supplied.
    scoped <- if ("scope_type" %in% colnames(tbl_ranges)) {
      !is.na(tbl_ranges$scope_type) & nzchar(trimws(tbl_ranges$scope_type))
    } else {
      rep(FALSE, nrow(tbl_ranges))
    }
    unavail <- scoped & !("scientific_name" %in% present)
    rules_skipped <- unique(as.character(tbl_ranges$rule_id[unavail]))
    tbl_ranges <- tbl_ranges[!unavail, , drop = FALSE]

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
  if (nrow(tbl_cats) > 0) {
    results$qc_check_categorical_values <- qc_check_categorical_values(
      data = data,
      rules = .qc_keep_cols(tbl_cats, c("column_name", "value"))
    )
  }

  # --- Test 4: missing values ------------------------------------------------
  miss_rows <- tbl_struct[tbl_struct$missing_values %in% .qc_missing_active, ]
  if (nrow(miss_rows) > 0) {
    results$qc_check_missing_values <- qc_check_missing_values(
      data = data,
      rules = .qc_keep_cols(miss_rows, c("column_name", "missing_values"))
    )
  }

  # --- Test 5: numeric ranges ------------------------------------------------
  if (nrow(tbl_ranges) > 0) {
    results$qc_check_numeric_ranges <- qc_check_numeric_ranges(
      data = data,
      rules = .qc_keep_cols(tbl_ranges, .qc_range_rule_cols)
    )
  }

  # --- Test 6: lookup values ---------------------------------------------------
  present_lookup_cols <- intersect(names(lookup_map), colnames(data))
  if (length(present_lookup_cols) > 0) {
    results$qc_check_lookup_values <- qc_check_lookup_values(
      data = data,
      lookups = lookup_map[present_lookup_cols],
      scopes = lookup_scope_map[
        intersect(names(lookup_scope_map), present_lookup_cols)
      ]
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

  # --- Test 8: grouped aggregations ------------------------------------------
  # Table-scoped: an aggregation needs its whole grouping key present, which a
  # column subset cannot guarantee. Unlike every other check here the rules are
  # hand-authored in R/qc_mg_agg_*.R rather than resolved from the metadata
  # repo, so the gate is the code registry rather than a rule table's row count.
  if (table_scope && .qc_has_aggregations(table_id)) {
    results$qc_check_aggregation <- qc_check_aggregation(
      data = data,
      table_id = table_id
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
    rules_skipped = rules_skipped,
    stale_registries = stale_registries,
    rules_schema = rules_schema
  )
}
