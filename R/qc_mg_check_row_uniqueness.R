#' Check that rows are unique across identity columns
#'
#' @description
#' Validates that no two rows share the same combination of identity-column
#' values. Identity columns are the subset of columns whose `uuid_identity`
#' flag is `TRUE` in `marinegeo_metadata$database_structure` — the same
#' columns used by [utl_mg_generate_row_uuid()] to produce stable row UUIDs.
#'
#' Duplicate identity combinations represent data-entry errors or pipeline
#' faults and are always reported as a `"fail"`.
#'
#' @param data A data frame to validate.
#' @param id_cols Character vector. Names of the columns that together form
#'   each row's identity (i.e., the columns with `uuid_identity = TRUE` for
#'   the relevant table). All named columns must be present in `data`.
#' @param detail Logical. If `TRUE` (default), the `failures` element contains
#'   a data frame with the row indices and identity-column values of every row
#'   involved in a duplicate group. If `FALSE`, `failures` is `NULL`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_row_uniqueness"`.}
#'     \item{`status`}{Character. `"fail"` if any duplicate identity
#'       combinations are found; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with one row containing:
#'       `n_rows` (total row count), `n_id_cols` (number of identity columns),
#'       `n_duplicate_rows` (rows involved in at least one duplicate group),
#'       and `n_duplicate_groups` (number of distinct duplicated identity
#'       combinations).}
#'     \item{`failures`}{Data frame with one row per duplicated row. Columns
#'       are `row_index` (1-based position in `data`) followed by one column
#'       per identity column showing the duplicated values. `NULL` if
#'       `status == "pass"` or `detail == FALSE`.}
#'   }
#'
#' @details
#' All rows that are members of a duplicate group are included in `failures`,
#' not just the second (and later) occurrences. This makes it easy to locate
#' every affected row in the source data.
#'
#' This function is called automatically by [qc_run()] when the
#' `database_structure` metadata contains one or more columns with
#' `uuid_identity = TRUE` for the given `table_id`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   site_code   = c("BIS-001", "BIS-001", "CCN-001"),
#'   transect_id = c(1L, 1L, 1L),        # row 1 and 2 are duplicates
#'   cover       = c(45.2, 30.1, 60.0),
#'   stringsAsFactors = FALSE
#' )
#'
#' qc_check_row_uniqueness(df, id_cols = c("site_code", "transect_id"))
qc_check_row_uniqueness <- function(data, id_cols, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(id_cols) || length(id_cols) == 0L) {
    stop("`id_cols` must be a non-empty character vector.")
  }
  if (!is.logical(detail) || length(detail) != 1L || is.na(detail)) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # --- Column presence check --------------------------------------------------
  missing_cols <- setdiff(id_cols, colnames(data))
  if (length(missing_cols) > 0L) {
    stop(
      "Column(s) in `id_cols` are not present in `data`: ",
      paste(paste0('"', missing_cols, '"'), collapse = ", "),
      "."
    )
  }

  # --- Empty data fast-path ---------------------------------------------------
  if (nrow(data) == 0L) {
    return(list(
      test    = "qc_check_row_uniqueness",
      status  = "pass",
      message = "No rows to check.",
      summary = data.frame(
        n_rows             = 0L,
        n_id_cols          = length(id_cols),
        n_duplicate_rows   = 0L,
        n_duplicate_groups = 0L,
        stringsAsFactors   = FALSE
      ),
      failures = NULL
    ))
  }

  # --- Duplicate detection ----------------------------------------------------
  id_df   <- data[, id_cols, drop = FALSE]
  is_dup  <- duplicated(id_df) | duplicated(id_df, fromLast = TRUE)

  n_dup_rows   <- sum(is_dup)
  n_dup_groups <- if (n_dup_rows > 0L) nrow(unique(id_df[is_dup, , drop = FALSE])) else 0L

  summary_df <- data.frame(
    n_rows             = nrow(data),
    n_id_cols          = length(id_cols),
    n_duplicate_rows   = n_dup_rows,
    n_duplicate_groups = n_dup_groups,
    stringsAsFactors   = FALSE
  )

  # --- Build result -----------------------------------------------------------
  if (n_dup_rows == 0L) {
    return(list(
      test     = "qc_check_row_uniqueness",
      status   = "pass",
      message  = paste0(
        "All ", nrow(data), " rows are unique across ",
        length(id_cols), " identity column(s)."
      ),
      summary  = summary_df,
      failures = NULL
    ))
  }

  msg <- paste0(
    n_dup_rows, " row(s) involved in ", n_dup_groups,
    " duplicate identity group(s) across column(s): ",
    paste(id_cols, collapse = ", ")
  )

  failures_df <- if (detail) {
    cbind(
      data.frame(row_index = which(is_dup), stringsAsFactors = FALSE),
      id_df[is_dup, , drop = FALSE]
    )
  } else {
    NULL
  }

  list(
    test     = "qc_check_row_uniqueness",
    status   = "fail",
    message  = msg,
    summary  = summary_df,
    failures = failures_df
  )
}
