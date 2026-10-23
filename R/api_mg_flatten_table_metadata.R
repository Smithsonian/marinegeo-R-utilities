# Flatten per-protocol YAML table metadata into the flat registry tibbles the QC
# engine consumes.
#
# `table-metadata/*.yaml` is the *authoring* format: one file per protocol, nested
# table -> columns[] -> rules, with predicates written inline where they are used.
# The flat tibbles in `marinegeo_metadata` remain the *internal* format, because
# every qc_mg_check_*() function is written against rectangular rule tables
# (`active_rules[active_rules$column_name == col, ][1, ]`, `seq_len(nrow(rules))`).
# This file is the single component that knows both shapes, which is what lets the
# authoring format change without touching a single check.
#
# Two YAML 1.1 hazards drive the design, both verified against yaml 2.3.12:
#
#   1. Unquoted scalars are type-resolved aggressively: `N`, `y`, `no`, `on` all
#      become logical, and `0.1` becomes numeric. Calling as.character() on the
#      *parsed* value is therefore silent data corruption -- `N` comes back as
#      "FALSE" and the authored token is unrecoverable. So we parse every scalar
#      as a string (see .mg_yaml_handlers()) and coerce per field from the schema
#      below, rather than trusting the parser's inference.
#   2. Because booleans arrive as strings, `uuid_identity` must be coerced with
#      the YAML 1.1 boolean token set, not as.logical() -- as.logical("yes") is
#      NA, which would silently drop a column from the row-uuid identity set.
#
# Field types are declared, not inferred. This is stricter than the CSV path it
# replaces, where `uuid_identity` arrived logical only because readr guessed
# right; R/qc_mg_run.R calls which() on it, which errors on a character vector.

# YAML 1.1 boolean token sets (the `y|n` forms are why as.logical() is unsafe).
.mg_yaml_true_tokens <- c(
  "y",
  "Y",
  "yes",
  "Yes",
  "YES",
  "true",
  "True",
  "TRUE",
  "on",
  "On",
  "ON"
)
.mg_yaml_false_tokens <- c(
  "n",
  "N",
  "no",
  "No",
  "NO",
  "false",
  "False",
  "FALSE",
  "off",
  "Off",
  "OFF"
)

# Column orders of the six tibbles this file produces. Must match the bundled
# `marinegeo_metadata` exactly -- the round-trip test asserts on these.
.mg_flat_schema <- list(
  database_structure = c(
    "protocol",
    "table_id",
    "column_name",
    "data_type",
    "missing_values",
    "uuid_identity"
  ),
  categorical_values = c("table_id", "condition_id", "column_name", "value"),
  numeric_ranges = c(
    "table_id",
    "condition_id",
    "column_name",
    "range_type",
    "max_fail",
    "min_fail",
    "max_warn",
    "min_warn"
  ),
  qc_conditions = c(
    "condition_id",
    "table_id",
    "column_name",
    "operator",
    "value",
    "description"
  ),
  aggregate_rules = c(
    "rule_id",
    "table_id",
    "group_columns",
    "value_column",
    "aggregate_function",
    "comparison",
    "target_column",
    "target_value",
    "tolerance",
    "condition_id",
    "severity",
    "description"
  ),
  cross_table_rules = c(
    "rule_id",
    "relation_type",
    "left_table_id",
    "right_table_id",
    "group_columns",
    "left_columns",
    "right_columns",
    "left_condition_id",
    "right_condition_id",
    "severity",
    "description"
  )
)

# Closed enums that are not already defined elsewhere in the package. The
# operator, aggregate-function, comparison and relation vocabularies are
# deliberately *not* duplicated here -- they are validated against the real
# objects in R/qc_mg_conditional.R, R/qc_mg_check_aggregate.R and
# R/qc_mg_check_cross_table.R, so the schema can never drift from the engine.
.mg_missing_values_enum <- c("fail", "warn", "allow")
.mg_range_type_enum <- c("inclusive", "exclusive")
.mg_severity_enum <- c("fail", "warn")

# ---------------------------------------------------------------------------
# Scalar coercion
# ---------------------------------------------------------------------------

#' Handlers that stop libyaml from type-resolving scalars
#'
#' Returns the raw token for every type YAML 1.1 would otherwise coerce, so the
#' authored text survives to the coercion step.
#'
#' @return Named list of handler functions for [yaml::yaml.load()].
#' @noRd
.mg_yaml_handlers <- function() {
  keep <- function(x) x
  list(
    "bool#yes" = keep,
    "bool#no" = keep,
    bool = keep,
    int = keep,
    float = keep
  )
}

#' Is a parsed YAML value absent?
#' @noRd
.mg_absent <- function(x) {
  is.null(x) ||
    length(x) == 0L ||
    (length(x) == 1L && !is.list(x) && is.na(x))
}

#' Coerce a parsed YAML value to character, element-wise
#'
#' Element-wise via vapply rather than `as.character(unlist(x))`: unlisting a
#' mixed list can reformat values across elements.
#'
#' @return Character vector, or `NA_character_` when absent.
#' @noRd
.mg_chr <- function(x) {
  if (.mg_absent(x)) {
    return(NA_character_)
  }
  vapply(
    x,
    function(v) if (.mg_absent(v)) NA_character_ else as.character(v)[[1]],
    character(1),
    USE.NAMES = FALSE
  )
}

#' Coerce a parsed YAML value to a single character scalar
#' @noRd
.mg_chr1 <- function(x) {
  v <- .mg_chr(x)
  if (length(v) == 0L) NA_character_ else v[[1]]
}

#' Coerce a parsed YAML value to a numeric scalar
#'
#' Errors rather than returning NA for an unparseable value: a silent NA here
#' would disable a bound without any signal.
#'
#' @noRd
.mg_num1 <- function(x, where) {
  if (.mg_absent(x)) {
    return(NA_real_)
  }
  raw <- .mg_chr1(x)
  out <- suppressWarnings(as.numeric(raw))
  if (is.na(out)) {
    stop(
      "expected a number but got '",
      raw,
      "' at ",
      where,
      call. = FALSE
    )
  }
  out
}

#' Coerce a parsed YAML value to logical using the YAML 1.1 token set
#'
#' as.logical() is not usable here: scalars arrive as strings (see
#' .mg_yaml_handlers) and as.logical("yes") is NA, which would silently drop the
#' column from the row-uuid identity set rather than fail.
#'
#' @noRd
.mg_lgl1 <- function(x, where) {
  if (.mg_absent(x)) {
    return(NA)
  }
  raw <- .mg_chr1(x)
  if (raw %in% .mg_yaml_true_tokens) {
    return(TRUE)
  }
  if (raw %in% .mg_yaml_false_tokens) {
    return(FALSE)
  }
  stop(
    "expected a boolean but got '",
    raw,
    "' at ",
    where,
    call. = FALSE
  )
}

#' Validate a scalar against a closed enum
#' @noRd
.mg_enum1 <- function(x, allowed, where, required = FALSE) {
  v <- .mg_chr1(x)
  if (is.na(v)) {
    if (required) {
      stop("missing required value at ", where, call. = FALSE)
    }
    return(NA_character_)
  }
  if (!v %in% allowed) {
    stop(
      "'",
      v,
      "' is not one of [",
      paste(allowed, collapse = ", "),
      "] at ",
      where,
      call. = FALSE
    )
  }
  v
}

#' Require a character scalar, erroring when absent
#' @noRd
.mg_req_chr1 <- function(x, where) {
  v <- .mg_chr1(x)
  if (is.na(v) || !nzchar(v)) {
    stop("missing required value at ", where, call. = FALSE)
  }
  v
}

# ---------------------------------------------------------------------------
# Slot normalisation
# ---------------------------------------------------------------------------

#' Normalise a polymorphic rule slot to a list of rule mappings
#'
#' A mapping is one rule; a sequence of mappings is several. Detected by whether
#' the parsed value carries names.
#'
#' @noRd
.mg_norm_slot <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(list())
  }
  if (!is.null(names(x))) {
    return(list(x))
  }
  x
}

#' Normalise a `one_of:` slot
#'
#' Accepts the sugar form (a sequence of bare scalars = one unconditional value
#' list) as well as the general `{when:, values:}` mapping form.
#'
#' @noRd
.mg_norm_one_of <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(list())
  }
  if (!is.null(names(x))) {
    return(list(x))
  }
  bare <- vapply(
    x,
    function(e) is.null(names(e)) && length(e) == 1L && !is.list(e),
    logical(1)
  )
  if (all(bare)) {
    return(list(list(values = x)))
  }
  x
}

# ---------------------------------------------------------------------------
# Condition registry
# ---------------------------------------------------------------------------

#' A per-document registry of `when:` blocks
#'
#' Collects inline predicates, deduplicates identical ones, assigns ids, and
#' accumulates the `qc_conditions` rows in emission order.
#'
#' @noRd
.mg_cond_registry <- function() {
  reg <- new.env(parent = emptyenv())
  reg$canon_to_id <- list()
  reg$rows <- list()
  reg$counts <- list()
  reg$ids <- character(0)
  reg
}

#' Parse a `when:` block into its ordered predicate terms
#'
#' `all:` is the only combinator. `any:` is rejected outright: the engine's
#' .qc_eval_condition() is Reduce(`&`, masks) with no OR mechanism, so silently
#' ANDing an `any:` block would invert the rule's meaning.
#'
#' @noRd
.mg_when_terms <- function(when, where) {
  if (!is.null(when$any)) {
    stop(
      "`any:` is not supported at ",
      where,
      ": the QC engine only composes predicate terms with AND. ",
      "Express a disjunction as a multi-value `in:` instead.",
      call. = FALSE
    )
  }
  terms <- when$all
  if (is.null(terms) || length(terms) == 0L) {
    stop("`when:` needs a non-empty `all:` at ", where, call. = FALSE)
  }
  if (!is.null(names(terms))) {
    terms <- list(terms)
  }

  lapply(seq_along(terms), function(i) {
    term <- terms[[i]]
    at <- paste0(where, " all[", i, "]")
    column <- .mg_req_chr1(term$column, paste0(at, " $column"))

    ops <- setdiff(names(term), "column")
    if (length(ops) != 1L) {
      stop(
        "each `when:` term needs exactly one operator key, got [",
        paste(ops, collapse = ", "),
        "] at ",
        at,
        call. = FALSE
      )
    }
    if (!ops %in% .qc_condition_operators) {
      stop(
        "'",
        ops,
        "' is not a known operator at ",
        at,
        ". Allowed: [",
        paste(.qc_condition_operators, collapse = ", "),
        "]",
        call. = FALSE
      )
    }

    if (ops %in% .qc_na_operators) {
      # These take no operand; the engine expects NA, not "" or "true".
      return(list(column = column, operator = ops, value = NA_character_))
    }

    parts <- .mg_chr(term[[ops]])
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0L) {
      stop(
        "operator '",
        ops,
        "' needs at least one value at ",
        at,
        " (an empty list would silently match no rows)",
        call. = FALSE
      )
    }
    list(
      column = column,
      operator = ops,
      value = paste(parts, collapse = "|")
    )
  })
}

#' Register a `when:` block and return its condition id
#'
#' Ids are `<label>` when the block names one, else `<table_id>.c<n>` with `n`
#' the ordinal of the distinct unlabelled block within that table in document
#' order. Deterministic (no hashing, no filesystem order) and globally unique,
#' since table_ids are.
#'
#' @noRd
.mg_register_when <- function(reg, when, table_id, where) {
  terms <- .mg_when_terms(when, where)

  canon <- paste(
    table_id,
    paste(
      vapply(
        terms,
        function(t) {
          paste(
            t$column,
            t$operator,
            if (is.na(t$value)) "<NA>" else t$value,
            sep = "\u001f"
          )
        },
        character(1)
      ),
      collapse = "\u001e"
    ),
    sep = "\u001d"
  )

  if (!is.null(reg$canon_to_id[[canon]])) {
    return(reg$canon_to_id[[canon]])
  }

  label <- .mg_chr1(when$label)
  if (!is.na(label)) {
    id <- label
  } else {
    n <- (if (is.null(reg$counts[[table_id]])) 0L else reg$counts[[table_id]]) +
      1L
    reg$counts[[table_id]] <- n
    id <- paste0(table_id, ".c", n)
  }
  if (id %in% reg$ids) {
    stop(
      "duplicate condition id '",
      id,
      "' at ",
      where,
      " -- labels must be unique within a protocol file",
      call. = FALSE
    )
  }

  description <- .mg_chr1(when$description)
  for (t in terms) {
    reg$rows[[length(reg$rows) + 1L]] <- list(
      condition_id = id,
      table_id = table_id,
      column_name = t$column,
      operator = t$operator,
      value = t$value,
      description = description
    )
  }
  reg$ids <- c(reg$ids, id)
  reg$canon_to_id[[canon]] <- id
  id
}

#' Resolve a rule's optional `when:` to a condition id
#' @noRd
.mg_rule_condition <- function(reg, rule, table_id, where) {
  if (is.null(rule$when)) {
    return(NA_character_)
  }
  .mg_register_when(reg, rule$when, table_id, paste0(where, " $when"))
}

# ---------------------------------------------------------------------------
# Parse
# ---------------------------------------------------------------------------

#' Parse a directory of per-protocol table-metadata YAML files
#'
#' Files are sorted by basename, which is what reproduces the row order of the
#' CSV path this replaces: the GitHub Trees API returns sorted paths and
#' `readr::read_csv()` row-binds them in that order.
#'
#' @param dir Character scalar. Directory holding `*.yaml`.
#' @return Named list of parsed documents, keyed and ordered by basename.
#' @noRd
.mg_parse_table_metadata_dir <- function(dir) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "the 'yaml' package is required to read table metadata sources",
      call. = FALSE
    )
  }
  if (!dir.exists(dir)) {
    stop("table metadata directory not found: '", dir, "'", call. = FALSE)
  }
  files <- list.files(
    dir,
    pattern = "\\.ya?ml$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) {
    stop("no YAML files found in '", dir, "'", call. = FALSE)
  }
  files <- files[order(basename(files))]

  docs <- lapply(files, function(f) {
    tryCatch(
      yaml::yaml.load_file(f, handlers = .mg_yaml_handlers()),
      error = function(e) {
        stop(
          "could not parse '",
          basename(f),
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  })
  names(docs) <- basename(files)
  docs
}

# ---------------------------------------------------------------------------
# Flatten
# ---------------------------------------------------------------------------

#' Bind a list of row lists into a tibble with a fixed column order
#' @noRd
.mg_bind_rows <- function(rows, cols, types) {
  if (length(rows) == 0L) {
    empty <- lapply(types, function(ty) {
      switch(ty, chr = character(0), num = numeric(0), lgl = logical(0))
    })
    names(empty) <- cols
    return(tibble::as_tibble(empty))
  }
  out <- lapply(seq_along(cols), function(i) {
    vals <- lapply(rows, function(r) r[[cols[[i]]]])
    switch(
      types[[i]],
      chr = vapply(
        vals,
        function(v) if (is.null(v)) NA_character_ else as.character(v)[[1]],
        character(1)
      ),
      num = vapply(
        vals,
        function(v) if (is.null(v)) NA_real_ else as.numeric(v)[[1]],
        numeric(1)
      ),
      lgl = vapply(
        vals,
        function(v) if (is.null(v)) NA else as.logical(v)[[1]],
        logical(1)
      )
    )
  })
  names(out) <- cols
  tibble::as_tibble(out)
}

#' Flatten parsed table-metadata documents into the six registry tibbles
#'
#' @param docs Named list from `.mg_parse_table_metadata_dir()`.
#' @return Named list of six tibbles matching `.mg_flat_schema`.
#' @noRd
.mg_flatten_table_metadata <- function(docs) {
  struct <- list()
  categorical <- list()
  ranges <- list()
  conditions <- list()
  aggregates <- list()
  cross <- list()

  for (fname in names(docs)) {
    doc <- docs[[fname]]
    reg <- .mg_cond_registry()

    file_protocol <- .mg_chr1(doc$protocol)

    tables <- doc$tables
    if (is.null(tables) || length(tables) == 0L) {
      stop("'", fname, "' has no `tables:`", call. = FALSE)
    }
    if (!is.null(names(tables))) {
      tables <- list(tables)
    }

    for (tbl in tables) {
      table_id <- .mg_req_chr1(
        tbl$table_id,
        paste0(fname, " $tables $table_id")
      )
      where_tbl <- paste0(fname, " [", table_id, "]")

      protocol <- if (is.null(tbl$protocol)) {
        file_protocol
      } else {
        .mg_chr1(tbl$protocol)
      }
      if (is.na(protocol)) {
        stop("missing `protocol:` at ", where_tbl, call. = FALSE)
      }
      cols <- tbl$columns
      if (is.null(cols) || length(cols) == 0L) {
        stop("no `columns:` at ", where_tbl, call. = FALSE)
      }
      if (!is.null(names(cols))) {
        stop(
          "`columns:` must be a sequence, not a mapping, at ",
          where_tbl,
          " -- column order is significant and a YAML mapping is unordered",
          call. = FALSE
        )
      }

      for (ci in seq_along(cols)) {
        col <- cols[[ci]]
        name <- .mg_req_chr1(
          col$name,
          paste0(where_tbl, " columns[", ci, "] $name")
        )
        where_col <- paste0(where_tbl, " ", name)

        # The flattener validates values, not key names, so an unrenamed
        # `missing:` would be read as "no rule" and the column would silently
        # lose its NA check. Refuse it instead.
        if (!is.null(col$missing)) {
          stop(
            "`missing:` was renamed to `na_values:` at ",
            where_col,
            " -- and its `enforce` value is now `fail`",
            call. = FALSE
          )
        }

        struct[[length(struct) + 1L]] <- list(
          protocol = protocol,
          table_id = table_id,
          column_name = name,
          data_type = .mg_req_chr1(col$type, paste0(where_col, " $type")),
          missing_values = .mg_enum1(
            col$na_values,
            .mg_missing_values_enum,
            paste0(where_col, " $na_values")
          ),
          uuid_identity = .mg_lgl1(
            col$uuid_identity,
            paste0(where_col, " $uuid_identity")
          )
        )

        for (entry in .mg_norm_one_of(col$one_of)) {
          cid <- .mg_rule_condition(
            reg,
            entry,
            table_id,
            paste0(where_col, " $one_of")
          )
          values <- .mg_chr(entry$values)
          values <- values[!is.na(values)]
          if (length(values) == 0L) {
            stop("`one_of:` has no values at ", where_col, call. = FALSE)
          }
          for (v in values) {
            categorical[[length(categorical) + 1L]] <- list(
              table_id = table_id,
              condition_id = cid,
              column_name = name,
              value = v
            )
          }
        }

        for (entry in .mg_norm_slot(col$numeric_range)) {
          cid <- .mg_rule_condition(
            reg,
            entry,
            table_id,
            paste0(where_col, " $numeric_range")
          )
          where_r <- paste0(where_col, " $numeric_range")
          ranges[[length(ranges) + 1L]] <- list(
            table_id = table_id,
            condition_id = cid,
            column_name = name,
            range_type = .mg_enum1(
              entry$range_type,
              .mg_range_type_enum,
              where_r
            ),
            max_fail = .mg_num1(entry$max_fail, paste0(where_r, " $max_fail")),
            min_fail = .mg_num1(entry$min_fail, paste0(where_r, " $min_fail")),
            max_warn = .mg_num1(entry$max_warn, paste0(where_r, " $max_warn")),
            min_warn = .mg_num1(entry$min_warn, paste0(where_r, " $min_warn"))
          )
        }
      }

      for (ai in seq_along(.mg_norm_slot(tbl$checks$aggregate))) {
        rule <- .mg_norm_slot(tbl$checks$aggregate)[[ai]]
        where_a <- paste0(where_tbl, " checks.aggregate[", ai, "]")
        cid <- .mg_rule_condition(reg, rule, table_id, where_a)

        group_cols <- .mg_chr(rule$group_by)
        group_cols <- group_cols[!is.na(group_cols) & nzchar(group_cols)]
        if (length(group_cols) == 0L) {
          stop("`group_by:` is empty at ", where_a, call. = FALSE)
        }

        target_column <- .mg_chr1(rule$target_column)
        has_value <- !is.null(rule$target_value)
        if (!is.na(target_column) && has_value) {
          stop(
            "set either `target_column:` or `target_value:`, not both, at ",
            where_a,
            call. = FALSE
          )
        }
        if (is.na(target_column) && !has_value) {
          stop(
            "one of `target_column:` or `target_value:` is required at ",
            where_a,
            call. = FALSE
          )
        }

        aggregates[[length(aggregates) + 1L]] <- list(
          rule_id = .mg_req_chr1(rule$rule_id, paste0(where_a, " $rule_id")),
          table_id = table_id,
          group_columns = paste(group_cols, collapse = "|"),
          value_column = .mg_req_chr1(rule$value, paste0(where_a, " $value")),
          aggregate_function = .mg_enum1(
            rule[["function"]],
            names(.qc_aggregate_functions),
            paste0(where_a, " $function"),
            required = TRUE
          ),
          comparison = .mg_enum1(
            rule$comparison,
            names(.qc_aggregate_comparisons),
            paste0(where_a, " $comparison"),
            required = TRUE
          ),
          target_column = target_column,
          target_value = .mg_num1(
            rule$target_value,
            paste0(where_a, " $target_value")
          ),
          tolerance = .mg_num1(rule$tolerance, paste0(where_a, " $tolerance")),
          condition_id = cid,
          severity = .mg_enum1(
            rule$severity,
            .mg_severity_enum,
            paste0(where_a, " $severity")
          ),
          description = .mg_chr1(rule$description)
        )
      }

      for (xi in seq_along(.mg_norm_slot(tbl$checks$cross_table))) {
        rule <- .mg_norm_slot(tbl$checks$cross_table)[[xi]]
        where_x <- paste0(where_tbl, " checks.cross_table[", xi, "]")

        pairs <- rule$on
        if (is.null(pairs) || length(pairs) == 0L) {
          stop("`on:` is required at ", where_x, call. = FALSE)
        }
        if (!is.null(names(pairs))) {
          pairs <- list(pairs)
        }
        left_cols <- vapply(
          seq_along(pairs),
          function(i) {
            .mg_req_chr1(pairs[[i]]$left, paste0(where_x, " on[", i, "] $left"))
          },
          character(1)
        )
        right_cols <- vapply(
          seq_along(pairs),
          function(i) {
            .mg_req_chr1(
              pairs[[i]]$right,
              paste0(where_x, " on[", i, "] $right")
            )
          },
          character(1)
        )

        group_cols <- .mg_chr(rule$group_by)
        group_cols <- group_cols[!is.na(group_cols) & nzchar(group_cols)]

        left_cid <- if (is.null(rule$when_left)) {
          NA_character_
        } else {
          .mg_register_when(
            reg,
            rule$when_left,
            table_id,
            paste0(where_x, " $when_left")
          )
        }
        right_table <- .mg_req_chr1(
          rule$right_table,
          paste0(where_x, " $right_table")
        )
        right_cid <- if (is.null(rule$when_right)) {
          NA_character_
        } else {
          .mg_register_when(
            reg,
            rule$when_right,
            right_table,
            paste0(where_x, " $when_right")
          )
        }

        cross[[length(cross) + 1L]] <- list(
          rule_id = .mg_req_chr1(rule$rule_id, paste0(where_x, " $rule_id")),
          relation_type = .mg_enum1(
            rule$relation,
            .qc_cross_table_relations,
            paste0(where_x, " $relation"),
            required = TRUE
          ),
          left_table_id = table_id,
          right_table_id = right_table,
          group_columns = if (length(group_cols) == 0L) {
            NA_character_
          } else {
            paste(group_cols, collapse = "|")
          },
          left_columns = paste(left_cols, collapse = "|"),
          right_columns = paste(right_cols, collapse = "|"),
          left_condition_id = left_cid,
          right_condition_id = right_cid,
          severity = .mg_enum1(
            rule$severity,
            .mg_severity_enum,
            paste0(where_x, " $severity")
          ),
          description = .mg_chr1(rule$description)
        )
      }
    }

    # File-level shared / not-yet-referenced predicates, emitted after the
    # inline ones so qc_conditions row order matches the CSV path it replaces.
    shared <- doc$conditions
    if (!is.null(shared) && length(shared) > 0L) {
      if (!is.null(names(shared))) {
        shared <- list(shared)
      }
      for (si in seq_along(shared)) {
        block <- shared[[si]]
        where_s <- paste0(fname, " conditions[", si, "]")
        tid <- .mg_req_chr1(block$table, paste0(where_s, " $table"))
        .mg_register_when(reg, block, tid, where_s)
      }
    }

    conditions <- c(conditions, reg$rows)
  }

  list(
    database_structure = .mg_bind_rows(
      struct,
      .mg_flat_schema$database_structure,
      c("chr", "chr", "chr", "chr", "chr", "lgl")
    ),
    categorical_values = .mg_bind_rows(
      categorical,
      .mg_flat_schema$categorical_values,
      c("chr", "chr", "chr", "chr")
    ),
    numeric_ranges = .mg_bind_rows(
      ranges,
      .mg_flat_schema$numeric_ranges,
      c("chr", "chr", "chr", "chr", "num", "num", "num", "num")
    ),
    qc_conditions = .mg_bind_rows(
      conditions,
      .mg_flat_schema$qc_conditions,
      c("chr", "chr", "chr", "chr", "chr", "chr")
    ),
    aggregate_rules = .mg_bind_rows(
      aggregates,
      .mg_flat_schema$aggregate_rules,
      c(
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "num",
        "num",
        "chr",
        "chr",
        "chr"
      )
    ),
    cross_table_rules = .mg_bind_rows(
      cross,
      .mg_flat_schema$cross_table_rules,
      c(
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr",
        "chr"
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Referential validation
# ---------------------------------------------------------------------------

#' Validate parsed table metadata, including cross-document references
#'
#' Structural and enum validation happens during `.mg_flatten_table_metadata()`,
#' which errors on the first problem. This adds the checks that need the whole
#' flattened picture. Every failure is an error: the QC engine treats a
#' malformed rule table as "no rules" and reports a clean pass, so a warning
#' here would be indistinguishable from success.
#'
#' @param docs Named list from `.mg_parse_table_metadata_dir()`.
#' @return Invisibly `TRUE`.
#' @noRd
.mg_validate_table_metadata <- function(docs) {
  flat <- .mg_flatten_table_metadata(docs)

  ds <- flat$database_structure
  known_tables <- unique(ds$table_id)

  dup_tables <- unique(ds$table_id[duplicated(
    paste(ds$table_id, ds$column_name, sep = "\u001f")
  )])
  if (length(dup_tables) > 0L) {
    stop(
      "duplicate column names within table(s): ",
      paste(dup_tables, collapse = ", "),
      call. = FALSE
    )
  }

  for (nm in c("aggregate_rules", "cross_table_rules")) {
    ids <- flat[[nm]]$rule_id
    if (anyDuplicated(ids)) {
      stop(
        "duplicate rule_id in ",
        nm,
        ": ",
        paste(unique(ids[duplicated(ids)]), collapse = ", "),
        call. = FALSE
      )
    }
  }

  missing_right <- setdiff(flat$cross_table_rules$right_table_id, known_tables)
  if (length(missing_right) > 0L) {
    stop(
      "cross-table rules reference unknown right table(s): ",
      paste(missing_right, collapse = ", "),
      call. = FALSE
    )
  }

  # Every column a rule names must exist in the table it is attached to.
  known_cols <- paste(ds$table_id, ds$column_name, sep = "\u001f")
  check_cols <- function(table_ids, columns, label) {
    wanted <- paste(table_ids, columns, sep = "\u001f")
    bad <- unique(wanted[!wanted %in% known_cols])
    if (length(bad) > 0L) {
      stop(
        label,
        " reference unknown column(s): ",
        paste(sub("\u001f", ".", bad), collapse = ", "),
        call. = FALSE
      )
    }
  }

  check_cols(
    flat$qc_conditions$table_id,
    flat$qc_conditions$column_name,
    "conditions"
  )
  check_cols(
    flat$aggregate_rules$table_id,
    flat$aggregate_rules$value_column,
    "aggregate rules"
  )

  agg <- flat$aggregate_rules
  has_target_col <- !is.na(agg$target_column)
  if (any(has_target_col)) {
    check_cols(
      agg$table_id[has_target_col],
      agg$target_column[has_target_col],
      "aggregate rule targets"
    )
  }
  for (i in seq_len(nrow(agg))) {
    check_cols(
      rep(agg$table_id[[i]], length(.qc_split_list(agg$group_columns[[i]]))),
      .qc_split_list(agg$group_columns[[i]]),
      paste0("aggregate rule '", agg$rule_id[[i]], "' group_by")
    )
  }

  xt <- flat$cross_table_rules
  for (i in seq_len(nrow(xt))) {
    left <- .qc_split_list(xt$left_columns[[i]])
    right <- .qc_split_list(xt$right_columns[[i]])
    check_cols(
      rep(xt$left_table_id[[i]], length(left)),
      left,
      paste0("cross-table rule '", xt$rule_id[[i]], "' left")
    )
    check_cols(
      rep(xt$right_table_id[[i]], length(right)),
      right,
      paste0("cross-table rule '", xt$rule_id[[i]], "' right")
    )
    grp <- .qc_split_list(xt$group_columns[[i]])
    if (length(grp) > 0L) {
      check_cols(
        rep(xt$left_table_id[[i]], length(grp)),
        grp,
        paste0("cross-table rule '", xt$rule_id[[i]], "' group_by (left)")
      )
      check_cols(
        rep(xt$right_table_id[[i]], length(grp)),
        grp,
        paste0("cross-table rule '", xt$rule_id[[i]], "' group_by (right)")
      )
    }
  }

  # Conditions referenced by a rule must exist.
  defined <- unique(flat$qc_conditions$condition_id)
  referenced <- unique(c(
    flat$numeric_ranges$condition_id,
    flat$categorical_values$condition_id,
    flat$aggregate_rules$condition_id,
    flat$cross_table_rules$left_condition_id,
    flat$cross_table_rules$right_condition_id
  ))
  referenced <- referenced[!is.na(referenced)]
  dangling <- setdiff(referenced, defined)
  if (length(dangling) > 0L) {
    stop(
      "rules reference undefined condition(s): ",
      paste(dangling, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
