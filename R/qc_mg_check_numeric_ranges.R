#' Check numeric column values against expected ranges
#'
#' @description
#' Validates that numeric columns in a data frame fall within expected bounds
#' defined in `numeric_ranges` metadata. Supports both inclusive and exclusive
#' range types, separate fail and warn thresholds, and per-column NA-safe
#' evaluation. Designed for use in MarineGEO QA/QC pipelines and can be called
#' directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#' @param rules A data frame with columns `column_name`, `max_fail`,
#'   `min_fail`, `max_warn`, `min_warn`, and `range_type`. Each row specifies
#'   the range rule for one column:
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
#' @return A [qc_issues] tibble with one row per out-of-range value
#'   (`issue = "out_of_range"`), or zero rows if all values are within range.
#'   Values breaching a fail threshold are `"fail"` rows; values breaching only
#'   a warn threshold are `"warn"` rows. `row` and `col_index` are 1-based, and
#'   `value` holds the offending number.
#'
#' @details
#' Only columns named in `rules$column_name` that also exist in `data` are
#' validated; other columns are silently skipped. Rows in `rules` with
#' `range_type == NA` are also silently skipped.
#'
#' `NA` values in the data column being evaluated are silently ignored and not
#' counted as violations.
#'
#' Fail thresholds are evaluated first. A value that breaches a fail threshold
#' is recorded as `"fail"` and not additionally evaluated for warn. A value
#' that does not breach any fail threshold but breaches a warn threshold is
#' recorded as `"warn"`.
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
#'   stringsAsFactors = FALSE
#' )
#'
#' # percent_cover 110 > 100 (fail), -5 < 0 (fail), 50 > 80 (warn within fail)
#' qc_check_numeric_ranges(df, rules)
qc_check_numeric_ranges <- function(data, rules) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop(
      "`rules` must be a data frame with columns `column_name`, `max_fail`, ",
      "`min_fail`, `max_warn`, `min_warn`, and `range_type`."
    )
  }
  required_cols <- c(
    "column_name",
    "max_fail",
    "min_fail",
    "max_warn",
    "min_warn",
    "range_type"
  )
  if (!all(required_cols %in% colnames(rules))) {
    stop("`rules` must have columns: ", paste(required_cols, collapse = ", "))
  }

  active_rules <- rules[!is.na(rules$range_type), ]
  cols_to_check <- intersect(active_rules$column_name, colnames(data))

  chunks <- lapply(cols_to_check, function(col) {
    col_pos <- which(colnames(data) == col)
    rule <- active_rules[active_rules$column_name == col, ][1, ]
    rtype <- rule$range_type
    values <- data[[col]]

    valid_idx <- which(!is.na(values))
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

    .range_issue <- function(
      mask,
      severity,
      over_max,
      under_min,
      max_b,
      min_b
    ) {
      if (!any(mask)) {
        return(NULL)
      }
      rows <- valid_idx[mask]
      vals <- values[rows]
      bound_label <- if (severity == "fail") {
        c("max_fail", "min_fail")
      } else {
        c("max_warn", "min_warn")
      }
      msg <- ifelse(
        over_max[mask],
        paste0("Value ", vals, " above ", bound_label[1], " (", max_b, ")."),
        paste0("Value ", vals, " below ", bound_label[2], " (", min_b, ").")
      )
      .qc_issue(
        check = "qc_check_numeric_ranges",
        severity = severity,
        issue = "out_of_range",
        message = msg,
        row = rows,
        column = col,
        col_index = col_pos,
        value = as.character(vals)
      )
    }

    dplyr::bind_rows(
      .range_issue(
        fail_mask,
        "fail",
        over_max_fail,
        under_min_fail,
        rule$max_fail,
        rule$min_fail
      ),
      .range_issue(
        warn_mask,
        "warn",
        over_max_warn,
        under_min_warn,
        rule$max_warn,
        rule$min_warn
      )
    )
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_numeric_ranges"
  )
}
