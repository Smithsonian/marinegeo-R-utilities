# Script to bundle up MarineGEO tabular metadata and make it available to the `marinegeo.utils` package.

# This script should only be run on the `main` branch of `marinegeo-metadata` in most cases:
repo_path <- paste0(Sys.getenv("repository_filepath"), "marinegeo-metadata")
content <- readLines(file.path(repo_path, ".git", "HEAD"), warn = FALSE)
# HEAD contains "ref: refs/heads/<branch-name>" when on a branch
branch <- sub("ref: refs/heads/", "", content)

if (branch != "main") {
  message(
    "Warning: You are not on the 'main' branch.\n",
    "Current branch: '",
    branch,
    "'\n"
  )
}

# ---------------------------------------------------------------------------
# Build helper: precompute functional group enrollment
# ---------------------------------------------------------------------------
#
# Runs at sysdata build time — never at package runtime. Walks the taxonomic
# adjacency table (BFS downward from each enroll_all_lower_ranks anchor) and
# the functional group hierarchy (upward walk for lineage strings), and
# returns a flat data frame with one row per (scientific_id, functional_group)
# pair.
#
# @param tl Data frame. `taxonomic_lookup` as read from CSV. Must have
#   columns `scientific_id` (character "APHIA:X"), `parent_id` (numeric
#   Aphia ID of parent), `name`, and `rank`.
# @param fg Data frame. `functional_group_lookup` as read from CSV. Must have
#   columns `scientific_id` (character "APHIA:X" or "FUNCTIONAL:X"),
#   `parent_id` (character "APHIA:X", "FUNCTIONAL:X", or NA),
#   `functional_group_name`, and `enroll_all_lower_ranks` (logical).
# @return A data frame with columns: scientific_id, functional_group_id,
#   functional_group_name, lineage, enrolled_via, anchor_id.
.build_functional_group_enrollment <- function(tl, fg) {

  # --- Children index ---------------------------------------------------------
  # Maps each scientific_id in taxonomic_lookup to its children's scientific_ids.
  # tl$parent_id is a numeric Aphia ID; convert to "APHIA:X" to match
  # scientific_id format before splitting.
  tl_valid <- tl[!is.na(tl$parent_id) & !is.na(tl$scientific_id), ]
  parent_sci_ids <- paste0("APHIA:", tl_valid$parent_id)
  children_index <- split(tl_valid$scientific_id, parent_sci_ids)

  # BFS: returns all taxonomic descendants of root_id (including root_id itself)
  .get_all_descendants <- function(root_id, children_idx) {
    visited <- character(0)
    queue <- root_id
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      if (current %in% visited) next
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
  fg_name_map   <- stats::setNames(fg_unique$functional_group_name, fg_unique$scientific_id)

  # Walk up functional_group_lookup from node_id to root; return lineage string
  # root > ... > node_id (root-to-leaf order).
  .get_fg_lineage_str <- function(node_id) {
    path <- character(0)
    current <- node_id
    repeat {
      name_val <- fg_name_map[current]
      if (is.na(name_val)) break
      path <- c(path, unname(name_val))
      parent_val <- fg_parent_map[current]
      if (is.na(parent_val)) break
      current <- unname(parent_val)
    }
    paste(rev(path), collapse = " > ")
  }

  # Walk up from node_id in fg to find the nearest FUNCTIONAL: ID (self or ancestor).
  .find_nearest_functional_id <- function(node_id) {
    current <- node_id
    repeat {
      if (grepl("^FUNCTIONAL:", current)) return(current)
      parent_val <- fg_parent_map[current]
      if (is.na(parent_val)) return(NA_character_)
      current <- unname(parent_val)
    }
  }

  # --- Build enrollment rows --------------------------------------------------
  rows <- list()

  for (i in seq_len(nrow(fg))) {
    row        <- fg[i, ]
    anchor_id  <- row$scientific_id
    enroll_all <- isTRUE(row$enroll_all_lower_ranks)

    lineage_str <- .get_fg_lineage_str(anchor_id)
    func_id     <- .find_nearest_functional_id(anchor_id)
    func_name   <- if (!is.na(func_id)) unname(fg_name_map[func_id]) else NA_character_

    # Every fg node is a direct enrollment of itself
    rows[[length(rows) + 1]] <- data.frame(
      scientific_id         = anchor_id,
      functional_group_id   = func_id,
      functional_group_name = func_name,
      lineage               = lineage_str,
      enrolled_via          = "direct",
      anchor_id             = anchor_id,
      stringsAsFactors      = FALSE
    )

    # enroll_all_lower_ranks: BFS all taxonomic descendants (APHIA: anchors only)
    if (enroll_all && grepl("^APHIA:", anchor_id)) {
      desc_ids <- .get_all_descendants(anchor_id, children_index)
      desc_ids <- desc_ids[desc_ids != anchor_id]   # anchor already added above

      if (length(desc_ids) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          scientific_id         = desc_ids,
          functional_group_id   = func_id,
          functional_group_name = func_name,
          lineage               = lineage_str,
          enrolled_via          = "enroll_all_lower_ranks",
          anchor_id             = anchor_id,
          stringsAsFactors      = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      scientific_id         = character(0),
      functional_group_id   = character(0),
      functional_group_name = character(0),
      lineage               = character(0),
      enrolled_via          = character(0),
      anchor_id             = character(0),
      stringsAsFactors      = FALSE
    ))
  }

  dplyr::bind_rows(rows) |> dplyr::distinct()
}

# ---------------------------------------------------------------------------
# Assemble marinegeo_metadata
# ---------------------------------------------------------------------------

marinegeo_metadata <- list(
  observation_lookup = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/taxonomy-and-functional-groups/observation-lookup/"
      ),
      full.names = T
    )
  ),

  taxonomic_lookup = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/taxonomy-and-functional-groups/taxonomic-lookup/"
      ),
      full.names = T
    )
  ),

  functional_group_lookup = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/taxonomy-and-functional-groups/functional-group-lookup/"
      ),
      full.names = T
    )
  ),

  data_index = readr::read_csv(
    paste0(
      Sys.getenv("repository_filepath"),
      "marinegeo-metadata/marinegeo_data_index.csv"
    )
  ),

  database_structure = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/table-metadata/data-structure/"
      ),
      full.names = T
    )
  ),

  categorical_values = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/table-metadata/categorical-values/"
      ),
      full.names = T
    )
  ),

  partner_codes = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/sites-and-partners/partner-codes/"
      ),
      full.names = T
    )
  ),

  site_names = readr::read_csv(
    list.files(
      paste0(
        Sys.getenv("repository_filepath"),
        "marinegeo-metadata/sites-and-partners/site-names/"
      ),
      full.names = T
    )
  )
)

# Precompute functional group enrollment (build-time traversal)
marinegeo_metadata$functional_group_enrollment <- .build_functional_group_enrollment(
  tl = marinegeo_metadata$taxonomic_lookup,
  fg = marinegeo_metadata$functional_group_lookup
)

usethis::use_data(marinegeo_metadata, internal = TRUE, overwrite = TRUE)
