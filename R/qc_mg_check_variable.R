#' Run all applicable QC tests on a single variable
#'
#' @description
#' Validates the values of one MarineGEO variable against the metadata rules
#' for that variable, without requiring the other columns of its table. Where
#' [qc_run()] validates a whole data table, `qc_check_variable()` validates a
#' single field — the shape a data entry application needs, when a user is
#' entering `cover_code` values and nothing else.
#'
#' @param x An atomic vector of values to validate, or a one-column data frame
#'   or tibble. To validate several columns at once, use
#'   `qc_run(x, table_id, partial = TRUE)`.
#' @param variable Character scalar. The variable (metadata `column_name`) that
#'   `x` holds values for, e.g. `"cover_code"`. Must be defined for `table_id`.
#' @param table_id Character scalar. The MarineGEO table identifier the
#'   variable belongs to, e.g. `"seagrass-cover-monitoring-v1"`. **Required** —
#'   see Details.
#'
#' @return A [qc_issues] tibble with one row per offending value (zero rows if
#'   every value passes). `row` is the 1-based index into `x`, `column` is
#'   `variable`, and `col_index` is always `1`. Run-level metadata rides along
#'   as attributes: `table_id`, `n_rows`, `checks_run`, `checks_skipped`,
#'   `columns`, `rules_skipped`, and `status`. See [qc_issues].
#'
#' @details
#' **Which checks run.** Five of the eight MarineGEO checks work on a column in
#' isolation and are dispatched from metadata exactly as [qc_run()] dispatches
#' them: [qc_check_data_types()], [qc_check_categorical_values()],
#' [qc_check_missing_values()], [qc_check_numeric_ranges()], and
#' [qc_check_lookup_values()]. A check only appears in the `checks_run`
#' attribute when metadata actually defines that kind of rule for the variable
#' — `cover_code`, for instance, has categorical values but no numeric range.
#'
#' The two remaining checks need the whole table ([qc_check_columns()],
#' [qc_check_row_uniqueness()]). They are skipped and reported in the
#' `checks_skipped` attribute, so an empty result can be distinguished from a
#' clean one.
#'
#' **Taxon-scoped range rules are dropped, not guessed.** A numeric range whose
#' `scope_type` restricts it to a clade is evaluated against `scientific_name`,
#' which a single column never supplies. Rather than apply such a bound to
#' every row as though it were unconditional, the rule is dropped and its
#' `rule_id` reported in the `rules_skipped` attribute — the only record that a
#' bound exists for the variable but went unchecked. Validate the whole table
#' with [qc_run()], or pass `scientific_name` alongside, to exercise it.
#'
#' **Why `table_id` is required.** Metadata is keyed on
#' `(table_id, column_name)`, and rules genuinely differ between table types for
#' the same variable name: `percent_cover` carries different bounds under
#' seagrass than under oyster substrate, `quadrat` is `TINYINT` in some tables
#' and `INT` in others, and `density_quadrat_dimensions` allows a different set
#' of values in each. A variable name alone is therefore not enough to resolve
#' its rules. Use [utl_mg_column_order()] to list a table's variables and
#' [utl_mg_list_tables()] to browse table identifiers.
#'
#' **Type coercion is the caller's job.** Controlled vocabularies are stored as
#' character even for numeric variables — `cover_code` is declared `DOUBLE` but
#' its allowed values are `"0"`, `"0.1"`, `"0.5"`, `"1"` … Comparison relies on
#' R's own coercion, so a character vector read straight from a text input will
#' additionally trip [qc_check_data_types()] with a `type_mismatch`, and a
#' string like `"0.50"` will not match `"0.5"`. Cast `x` to the declared type
#' first; `utl_mg_get_registry("database_structure", table_id = ...)` gives each
#' variable's `data_type`.
#'
#' @seealso [qc_run()] for whole tables and for column subsets
#'   (`partial = TRUE`); [qc_issues] for the result format.
#'
#' @export
#'
#' @examples
#' # Validate three cover codes; 9 is not in the controlled vocabulary.
#' # qc_check_variable(
#' #   c(0, 0.5, 9),
#' #   variable = "cover_code",
#' #   table_id = "seagrass-cover-monitoring-v1"
#' # )
#'
#' # A one-column data frame works the same way.
#' # qc_check_variable(
#' #   data.frame(cover_code = c(0, 0.5, 9)),
#' #   variable = "cover_code",
#' #   table_id = "seagrass-cover-monitoring-v1"
#' # )
#'
#' # Browse valid variables for a table first:
#' # utl_mg_column_order("seagrass-cover-monitoring-v1")
qc_check_variable <- function(x, variable, table_id) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(variable) || length(variable) != 1L || is.na(variable)) {
    stop("`variable` must be a single non-NA character string.")
  }
  if (!is.character(table_id) || length(table_id) != 1L || is.na(table_id)) {
    stop("`table_id` must be a single non-NA character string.")
  }

  values <- .qc_as_values(x)

  # --- Resolve the variable against metadata ---------------------------------
  known_cols <- .qc_known_columns(table_id)
  if (length(known_cols) == 0L) {
    stop(
      "No metadata found for table_id '",
      table_id,
      "'. ",
      "Call `utl_mg_list_tables()` to see all available table IDs."
    )
  }
  if (!variable %in% known_cols) {
    stop(
      "'",
      variable,
      "' is not a known variable for table_id '",
      table_id,
      "'. Call `utl_mg_column_order('",
      table_id,
      "')` to list its variables."
    )
  }

  # --- Dispatch as a one-column table ----------------------------------------
  data <- list(values)
  names(data) <- variable
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  .qc_dispatch(data, table_id = table_id, scope = "column")
}

# Internal: coerce the `x` argument of qc_check_variable() to a bare vector.
.qc_as_values <- function(x) {
  if (is.data.frame(x)) {
    if (ncol(x) != 1L) {
      stop(
        "`x` must be a vector or a one-column data frame; got ",
        ncol(x),
        " columns. ",
        "To validate several columns at once, use ",
        "`qc_run(x, table_id, partial = TRUE)`."
      )
    }
    return(x[[1]])
  }

  # POSIXlt is a list under the hood, so it fails is.atomic() despite being a
  # legitimate vector of values.
  if (is.atomic(x) || inherits(x, "POSIXlt")) {
    return(x)
  }

  stop(
    "`x` must be an atomic vector or a one-column data frame. Got: ",
    paste(class(x), collapse = "/")
  )
}
