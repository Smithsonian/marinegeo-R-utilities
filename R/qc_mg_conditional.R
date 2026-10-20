# ---------------------------------------------------------------------------
# Conditional QC rules: evaluating a rule on a subset of rows.
#
# A condition is not a check. It is a row-subsetting layer: a rule row in
# `numeric_ranges`, `categorical_values` (or any later rule table) may carry a
# `condition_id` referencing predicate terms in `qc_conditions`. Rules are
# grouped by that id, the matching rows are selected, and the ordinary
# `qc_check_*()` function runs against that subset -- so conditional ranges,
# conditional controlled vocabularies and conditional grouped aggregates all
# come from one mechanism and the checks themselves need no knowledge of it.
#
# `condition_id = NA` means "applies to every row", which is what every rule
# predating this file is, so the layer is backward compatible by construction.
# ---------------------------------------------------------------------------

# The closed set of predicate operators. Deliberately closed: `qc_conditions`
# is fetched over the network from the metadata repo at runtime, so an operator
# that evaluated R code (eval(parse(...))) would turn that repo into a remote
# code-execution path into every user's session. Extending the vocabulary is a
# deliberate edit here, not something metadata can do on its own.
.qc_condition_operators <- c(
  "in",
  "not_in",
  "==",
  "!=",
  ">",
  ">=",
  "<",
  "<=",
  "is_na",
  "not_na",
  "matches",
  # Taxonomy-aware: the value names a clade or a functional group and the
  # species set is derived from the registry. See R/qc_mg_taxon_sets.R. These
  # resolve metadata against lookup tables -- they still evaluate no code, so
  # the reason this set is closed is unaffected.
  "taxon_within",
  "taxon_not_within",
  "in_functional_group",
  "not_in_functional_group"
)

# Operators that are about NA itself, and so must not have their NA rows forced
# to FALSE the way every other operator does.
.qc_na_operators <- c("is_na", "not_na")

# Treat an empty CSV cell as "no condition", the same as a missing one.
.qc_normalize_ids <- function(x) {
  ids <- as.character(x)
  ids[!is.na(ids) & !nzchar(trimws(ids))] <- NA_character_
  ids
}

# Evaluate one predicate term against `data`, returning a logical mask of
# length nrow(data). Bad metadata degrades to an all-FALSE mask plus a warning
# -- "this rule did not apply" -- rather than erroring out the whole QC run.
.qc_eval_term <- function(data, column_name, operator, value) {
  n <- nrow(data)
  no_match <- rep(FALSE, n)

  operator <- if (is.na(operator)) "" else trimws(as.character(operator))
  if (!operator %in% .qc_condition_operators) {
    warning(
      "Unknown QC condition operator '",
      operator,
      "'; the condition was not applied. Valid operators are: ",
      paste(.qc_condition_operators, collapse = ", "),
      "."
    )
    return(no_match)
  }

  if (is.na(column_name) || !column_name %in% colnames(data)) {
    warning(
      "QC condition references column '",
      column_name,
      "', which is not present in the data; the condition was not applied."
    )
    return(no_match)
  }

  x <- data[[column_name]]
  rhs <- .qc_split_list(value)

  if (length(rhs) == 0L && !operator %in% .qc_na_operators) {
    warning(
      "QC condition on '",
      column_name,
      "' with operator '",
      operator,
      "' has no value; the condition was not applied."
    )
    return(no_match)
  }

  # Comparisons are made on character for set/equality operators and on numeric
  # for the ordered ones, mirroring how the value checks already rely on R's own
  # coercion rather than consulting the declared data_type.
  as_num <- function(v) suppressWarnings(as.numeric(v))

  # Expand the reference to an id set once, then test every row against it.
  # Returns NULL when the reference cannot be resolved, which is handled below
  # as "rule not applied".
  hierarchy <- function(set_fn, within) {
    sets <- set_fn(rhs)
    if (is.null(sets)) {
      return(NULL)
    }
    .qc_hierarchy_mask(x, sets, within)
  }

  mask <- switch(
    operator,
    "in" = as.character(x) %in% rhs,
    "not_in" = !(as.character(x) %in% rhs),
    "==" = as.character(x) == rhs[1],
    "!=" = as.character(x) != rhs[1],
    ">" = as_num(x) > as_num(rhs[1]),
    ">=" = as_num(x) >= as_num(rhs[1]),
    "<" = as_num(x) < as_num(rhs[1]),
    "<=" = as_num(x) <= as_num(rhs[1]),
    "is_na" = is.na(x),
    "not_na" = !is.na(x),
    "taxon_within" = hierarchy(.qc_taxon_sets, TRUE),
    "taxon_not_within" = hierarchy(.qc_taxon_sets, FALSE),
    "in_functional_group" = hierarchy(.qc_fg_sets, TRUE),
    "not_in_functional_group" = hierarchy(.qc_fg_sets, FALSE),
    "matches" = tryCatch(
      grepl(rhs[1], as.character(x)),
      error = function(e) {
        warning(
          "QC condition on '",
          column_name,
          "' has an invalid `matches` pattern ('",
          rhs[1],
          "'); the condition was not applied."
        )
        no_match
      }
    )
  )

  # A hierarchy operator whose reference did not resolve has already warned.
  if (is.null(mask)) {
    return(no_match)
  }

  # An NA in the predicate column is not evidence that the condition holds, so
  # it never matches -- the same stance the value checks take when they skip NA
  # rather than flag it. `is_na`/`not_na` are about NA itself and are already
  # NA-safe above.
  if (!operator %in% .qc_na_operators) {
    mask[is.na(x)] <- FALSE
  }
  mask & !is.na(mask)
}

# AND the terms of one condition together into a single row mask.
.qc_eval_condition <- function(data, cond_rows) {
  n <- nrow(data)
  if (
    is.null(cond_rows) || !is.data.frame(cond_rows) || nrow(cond_rows) == 0L
  ) {
    # A `condition_id` referenced by a rule but absent from `qc_conditions` must
    # not silently widen that rule to every row.
    return(rep(FALSE, n))
  }

  masks <- lapply(seq_len(nrow(cond_rows)), function(i) {
    .qc_eval_term(
      data,
      column_name = cond_rows$column_name[[i]],
      operator = cond_rows$operator[[i]],
      value = if ("value" %in% colnames(cond_rows)) {
        cond_rows$value[[i]]
      } else {
        NA_character_
      }
    )
  })

  Reduce(`&`, masks)
}

# The predicate columns one or more conditions need, used to decide whether a
# conditional rule is evaluable from the columns actually supplied.
.qc_condition_columns <- function(conditions, condition_id) {
  if (
    is.null(conditions) ||
      !is.data.frame(conditions) ||
      nrow(conditions) == 0L ||
      !all(c("condition_id", "column_name") %in% colnames(conditions))
  ) {
    return(character(0))
  }
  cols <- conditions$column_name[conditions$condition_id %in% condition_id]
  unique(cols[!is.na(cols)])
}

# The rows of `conditions` belonging to one id. `table_id` scopes the lookup
# where the caller holds conditions for more than one table at once (the
# cross-table check), since ids are only unique within a table.
.qc_condition_rows <- function(conditions, condition_id, table_id = NULL) {
  if (
    is.null(conditions) ||
      !is.data.frame(conditions) ||
      nrow(conditions) == 0L ||
      !"condition_id" %in% colnames(conditions)
  ) {
    return(data.frame())
  }
  keep <- !is.na(conditions$condition_id) &
    conditions$condition_id == condition_id
  if (!is.null(table_id) && "table_id" %in% colnames(conditions)) {
    keep <- keep &
      (is.na(conditions$table_id) | conditions$table_id %in% table_id)
  }
  conditions[keep, , drop = FALSE]
}

# A human-readable sentence for the issue's `detail`: the curated description
# from metadata where there is one, otherwise the terms themselves so a finding
# is still self-explanatory with a bare-bones CSV.
.qc_condition_detail <- function(cond_rows, condition_id) {
  if (
    "description" %in%
      colnames(cond_rows) &&
      nrow(cond_rows) > 0 &&
      !is.na(cond_rows$description[[1]]) &&
      nzchar(trimws(cond_rows$description[[1]]))
  ) {
    return(paste0(
      "condition '",
      condition_id,
      "': ",
      trimws(cond_rows$description[[1]])
    ))
  }

  terms <- vapply(
    seq_len(nrow(cond_rows)),
    function(i) {
      value <- if ("value" %in% colnames(cond_rows)) {
        cond_rows$value[[i]]
      } else {
        NA
      }
      paste(
        c(
          cond_rows$column_name[[i]],
          cond_rows$operator[[i]],
          if (!is.na(value)) as.character(value)
        ),
        collapse = " "
      )
    },
    character(1)
  )

  paste0("condition '", condition_id, "': ", paste(terms, collapse = " and "))
}

# Drop conditional rules that cannot be evaluated from `cols` because a
# predicate column was not supplied, returning the surviving rules and the ids
# dropped.
#
# Dropping is the only safe answer: silently keeping such a rule would apply a
# conditional bound as though it were unconditional -- validating shoot_length
# against Zostera bounds for every species. Reporting the dropped ids keeps
# "not applicable" distinguishable from "passed".
.qc_filter_unavailable_conditions <- function(rules, conditions, cols) {
  if (
    !is.data.frame(rules) ||
      nrow(rules) == 0L ||
      !"condition_id" %in% colnames(rules)
  ) {
    return(list(rules = rules, skipped = character(0)))
  }

  ids <- .qc_normalize_ids(rules$condition_id)
  keep <- rep(TRUE, length(ids))
  skipped <- character(0)

  for (id in unique(ids[!is.na(ids)])) {
    needed <- .qc_condition_columns(conditions, id)
    # length 0 means the id resolves to no terms at all -- a dangling reference,
    # equally unevaluable here.
    if (length(needed) == 0L || !all(needed %in% cols)) {
      keep[!is.na(ids) & ids == id] <- FALSE
      skipped <- c(skipped, id)
    }
  }

  list(rules = rules[keep, , drop = FALSE], skipped = unique(skipped))
}

# Run `check_fn` once per condition group and combine the results.
#
# The unconditional rules run against the whole table, exactly as before. Each
# conditional group runs against its matching subset, and the row numbers the
# check reports -- which are indices into whatever frame it was handed -- are
# mapped back to positions in the full table.
.qc_apply_conditional <- function(check_fn, data, rules, conditions, ...) {
  if (
    !is.data.frame(rules) ||
      nrow(rules) == 0L ||
      !"condition_id" %in% colnames(rules)
  ) {
    return(check_fn(data, rules, ...))
  }

  ids <- .qc_normalize_ids(rules$condition_id)
  chunks <- list()

  unconditional <- rules[is.na(ids), , drop = FALSE]
  if (nrow(unconditional) > 0) {
    chunks[[length(chunks) + 1L]] <- check_fn(data, unconditional, ...)
  }

  dangling <- character(0)

  for (id in unique(ids[!is.na(ids)])) {
    group <- rules[!is.na(ids) & ids == id, , drop = FALSE]
    cond_rows <- .qc_condition_rows(conditions, id)

    if (nrow(cond_rows) == 0L) {
      dangling <- c(dangling, id)
      next
    }

    mask <- .qc_eval_condition(data, cond_rows)
    if (!any(mask)) {
      next
    }

    out <- check_fn(data[mask, , drop = FALSE], group, ...)
    if (nrow(out) > 0) {
      # The only real trap in this file. `out$row` indexes the subset that was
      # handed to the check, so without this remap a conditional finding points
      # at the wrong record. `which(mask)[NA]` is NA, so the rule- and
      # column-level issues that carry no row stay NA.
      out$row <- which(mask)[out$row]
      # `col_index` deliberately needs no remap: the checks compute it as
      # which(colnames(data) == col), which is unchanged by row subsetting.
      # Fill in, never overwrite. A rule table that carries its own `rule_id`
      # (aggregate_rules, cross_table_rules) keeps it, and a check that wrote a
      # specific explanation keeps that too, with the condition appended as
      # context. Rule tables with no id of their own -- numeric_ranges,
      # categorical_values -- leave both NA, so the condition identifies the
      # rule there.
      cond_detail <- .qc_condition_detail(cond_rows, id)
      out$rule_id <- ifelse(is.na(out$rule_id), id, out$rule_id)
      out$detail <- ifelse(
        is.na(out$detail),
        cond_detail,
        paste0(out$detail, " (", cond_detail, ")")
      )
    }
    chunks[[length(chunks) + 1L]] <- out
  }

  if (length(dangling) > 0) {
    warning(
      "QC rule(s) reference condition_id(s) with no rows in `qc_conditions`: ",
      paste(unique(dangling), collapse = ", "),
      ". Those rules were not applied."
    )
  }

  dplyr::bind_rows(chunks)
}
