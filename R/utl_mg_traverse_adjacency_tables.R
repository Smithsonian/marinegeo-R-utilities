# Internal helpers for traversing MarineGEO taxonomic and functional group
# adjacency tables at sysdata build time.
#
# All functions in this file are internal (not exported). They are called from
# data-raw/assemble_marinegeo_metadata_sysdata.R via devtools::load_all().


# ---------------------------------------------------------------------------
# Taxonomic adjacency table traversal
# ---------------------------------------------------------------------------

#' Walk the taxonomic adjacency table upward from a given node
#'
#' @description
#' Collects the taxon name at each rank by walking from `node_id` up to the
#' root of the `taxonomic_lookup` adjacency table. Used by
#' `.get_taxonomic_classifications()`.
#'
#' @param node_id Character. The `id` of the starting node in `df`.
#' @param df Data frame. The adjacency table (e.g. `taxonomic_lookup`) with
#'   columns `id`, `parent_id`, `rank`, and `name`.
#'
#' @return A named list where each name is a rank label and each value is the
#'   taxon name at that rank.
#'
#' @keywords internal
.get_parent_rank <- function(node_id, df) {
  parent_ranks <- list()
  current_id <- node_id

  while (length(current_id) > 0 && !is.na(current_id)) {
    row <- dplyr::filter(df, .data$id == current_id)
    if (nrow(row) == 0) {
      break
    }
    parent_ranks[[row$rank]] <- row$name
    current_id <- row$parent_id
  }

  parent_ranks
}


#' Get wide-form taxonomic classifications for a set of scientific IDs
#'
#' @description
#' For each Aphia-based `scientific_id` supplied, walks the MarineGEO
#' `taxonomic_lookup` adjacency table to reconstruct the full taxonomic
#' hierarchy and returns the result as a wide data frame with one column per
#' standard rank. Called at sysdata build time to precompute classifications.
#'
#' Only IDs that begin with `"APHIA:"` are present in `taxonomic_lookup`.
#' Functional group IDs (`"FUNCTIONAL:"`) will not be matched and are silently
#' excluded from the output.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:374534", "APHIA:145792")`). `NA` values are removed with
#'   a message.
#' @param taxonomic_lookup Data frame. The `taxonomic_lookup` adjacency table.
#'   Must have columns `scientific_id`, `id`, `parent_id`, `rank`, and `name`.
#'
#' @return A data frame with one row per matched `scientific_id` and columns:
#'   \describe{
#'     \item{scientific_id}{The input identifier.}
#'     \item{rank}{The taxonomic rank of the input ID itself (e.g. `"Species"`).}
#'     \item{Kingdom, Phylum, Class, Order, Family, Genus, Species}{Taxon name
#'       at each standard rank. Columns are only present when at least one row
#'       has a value; missing ranks for a given row are `NA`.}
#'   }
#'   Returns a zero-row data frame with the expected columns if no IDs are
#'   matched.
#'
#' @details
#' `Phylum (Division)` — the WoRMS rank name used for some plant and algae
#' lineages — is mapped to `Phylum` before the standard columns are selected.
#'
#' Intermediate ranks (Tribe, Forma, super-, sub-, infra-, mega-, giga-, parv-,
#' subter- prefixed ranks) are dropped from the output.
#'
#' @keywords internal
.get_taxonomic_classifications <- function(scientific_ids, taxonomic_lookup) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(scientific_ids)) {
    stop("`scientific_ids` must be a character vector.")
  }

  na_count <- sum(is.na(scientific_ids))
  if (na_count > 0) {
    message(na_count, " NA value(s) removed from `scientific_ids`.")
    scientific_ids <- scientific_ids[!is.na(scientific_ids)]
  }

  if (length(scientific_ids) == 0) {
    return(
      data.frame(
        scientific_id = character(0),
        rank = character(0),
        Kingdom = character(0),
        Phylum = character(0),
        Class = character(0),
        Order = character(0),
        Family = character(0),
        Genus = character(0),
        Species = character(0)
      )
    )
  }

  # --- Lookup -----------------------------------------------------------------
  taxonomic_lookup <- taxonomic_lookup |>
    dplyr::mutate(
      rank = dplyr::case_when(
        rank == "Phylum (Division)" ~ "Phylum",
        T ~ rank
      )
    )

  classifications_df <- taxonomic_lookup |>
    dplyr::filter(.data$scientific_id %in% scientific_ids) |>
    dplyr::mutate(
      parent_ranks = purrr::map(.data$id, \(id) {
        .get_parent_rank(id, taxonomic_lookup)
      }),
      classifications_df = purrr::map(.data$parent_ranks, \(a) {
        tibble::as_tibble(as.list(a))
      })
    ) |>
    dplyr::select("scientific_id", "classifications_df") |>
    tidyr::unnest("classifications_df") |>
    dplyr::select(
      "scientific_id",
      dplyr::any_of(c(
        "Kingdom",
        "Phylum",
        "Class",
        "Order",
        "Family",
        "Genus",
        "Species"
      ))
    )

  ranks_df <- taxonomic_lookup |>
    dplyr::filter(.data$scientific_id %in% scientific_ids) |>
    dplyr::select("scientific_id", "rank")

  classifications_df |>
    dplyr::left_join(ranks_df, by = "scientific_id") |>
    dplyr::select("scientific_id", "rank", dplyr::everything())
}


# ---------------------------------------------------------------------------
# Functional group adjacency table traversal
# ---------------------------------------------------------------------------

#' Build functional group enrollment table
#'
#' @description
#' Precomputes a flat enrollment table linking each `scientific_id` to its
#' functional group(s). Called at sysdata build time (from
#' `data-raw/assemble_marinegeo_metadata_sysdata.R`) — never at package
#' runtime.
#'
#' Walks the taxonomic adjacency table downward (BFS from each
#' `enroll_all_lower_ranks` anchor) and the functional group hierarchy upward
#' (to build lineage strings), and returns one row per
#' (`scientific_id`, functional group) pair.
#'
#' @param tl Data frame. `taxonomic_lookup` as read from CSV. Must have columns
#'   `scientific_id` (character `"APHIA:X"`), `parent_id` (numeric Aphia ID of
#'   parent), `name`, and `rank`.
#' @param fg Data frame. `functional_group_lookup` as read from CSV. Must have
#'   columns `scientific_id` (character `"APHIA:X"` or `"FUNCTIONAL:X"`),
#'   `parent_id` (character `"APHIA:X"`, `"FUNCTIONAL:X"`, or `NA`),
#'   `functional_group_name`, and `enroll_all_lower_ranks` (logical).
#'
#' @return A data frame with columns: `scientific_id`, `functional_group_id`,
#'   `functional_group_name`, `lineage`, `enrolled_via`, `anchor_id`. Returns
#'   a zero-row data frame with those columns if `fg` produces no enrollment
#'   rows.
#'
#' @details
#' Two enrollment mechanisms are supported:
#' \describe{
#'   \item{direct}{Every node in `fg` is enrolled in itself.}
#'   \item{enroll_all_lower_ranks}{For `APHIA:`-prefixed anchor nodes with
#'     `enroll_all_lower_ranks = TRUE`, all taxonomic descendants (via BFS on
#'     `tl`) are also enrolled and marked `enrolled_via = "enroll_all_lower_ranks"`.}
#' }
#'
#' @keywords internal
.build_functional_group_enrollment <- function(tl, fg) {
  # --- Children index ---------------------------------------------------------
  # Maps each scientific_id in taxonomic_lookup to its children's scientific_ids.
  tl_valid <- tl[!is.na(tl$parent_id) & !is.na(tl$scientific_id), ]
  parent_sci_ids <- tl_valid$parent_id
  children_index <- split(tl_valid$scientific_id, parent_sci_ids)

  # Returns all taxonomic descendants of root_id (including root_id itself).
  .get_all_descendants <- function(root_id, children_idx) {
    visited <- character(0)
    queue <- root_id
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      if (current %in% visited) {
        next
      }
      visited <- c(visited, current)
      children <- children_idx[[current]]
      if (!is.null(children)) {
        queue <- c(queue, children)
      }
    }
    visited
  }

  # --- Functional group lookup maps -------------------------------------------
  # Assumes scientific_id is unique per row within functional_group_lookup
  # (duplicates are resolved by keeping the first occurrence via match()).
  fg_idx <- match(unique(fg$scientific_id), fg$scientific_id)
  fg_unique <- fg[fg_idx, ]
  fg_parent_map <- stats::setNames(fg_unique$parent_id, fg_unique$scientific_id)
  fg_name_map <- stats::setNames(
    fg_unique$functional_group_name,
    fg_unique$scientific_id
  )

  # Walk up functional_group_lookup from node_id to root; return lineage string
  # root > ... > node_id (root-to-leaf order).
  .get_fg_lineage_str <- function(node_id) {
    path <- character(0)
    current <- node_id
    repeat {
      name_val <- fg_name_map[current]
      if (is.na(name_val)) {
        break
      }
      path <- c(path, unname(name_val))
      parent_val <- fg_parent_map[current]
      if (is.na(parent_val)) {
        break
      }
      current <- unname(parent_val)
    }
    paste(rev(path), collapse = " > ")
  }

  # Walk up from node_id in fg to find the nearest FUNCTIONAL: ID (self or ancestor).
  .find_nearest_functional_id <- function(node_id) {
    current <- node_id
    repeat {
      if (grepl("^FUNCTIONAL:", current)) {
        return(current)
      }
      parent_val <- fg_parent_map[current]
      if (is.na(parent_val)) {
        return(NA_character_)
      }
      current <- unname(parent_val)
    }
  }

  # --- Build enrollment rows --------------------------------------------------
  rows <- list()

  for (i in seq_len(nrow(fg))) {
    row <- fg[i, ]
    anchor_id <- row$scientific_id
    enroll_all <- isTRUE(row$enroll_all_lower_ranks)

    lineage_str <- .get_fg_lineage_str(anchor_id)
    func_id <- .find_nearest_functional_id(anchor_id)
    func_name <- if (!is.na(func_id)) {
      unname(fg_name_map[func_id])
    } else {
      NA_character_
    }

    # Every fg node is a direct enrollment of itself
    rows[[length(rows) + 1]] <- data.frame(
      scientific_id = anchor_id,
      functional_group_id = func_id,
      functional_group_name = func_name,
      lineage = lineage_str,
      enrolled_via = "direct",
      anchor_id = anchor_id,
      stringsAsFactors = FALSE
    )

    # enroll_all_lower_ranks: BFS all taxonomic descendants (APHIA: anchors only)
    if (enroll_all && grepl("^APHIA:", anchor_id)) {
      desc_ids <- .get_all_descendants(anchor_id, children_index)
      desc_ids <- desc_ids[desc_ids != anchor_id] # anchor already added above

      if (length(desc_ids) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          scientific_id = desc_ids,
          functional_group_id = func_id,
          functional_group_name = func_name,
          lineage = lineage_str,
          enrolled_via = "enroll_all_lower_ranks",
          anchor_id = anchor_id,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      scientific_id = character(0),
      functional_group_id = character(0),
      functional_group_name = character(0),
      lineage = character(0),
      enrolled_via = character(0),
      anchor_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(rows) |> dplyr::distinct()
}
