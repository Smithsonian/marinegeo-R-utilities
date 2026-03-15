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

# Load internal helper functions for traversing adjacency tables
source("R/utl_mg_traverse_adjacency_tables.R")

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

# Precompute wide-form taxonomic classifications (build-time adjacency table traversal)
marinegeo_metadata$taxonomic_classifications <- .get_taxonomic_classifications(
  scientific_ids = marinegeo_metadata$taxonomic_lookup$scientific_id,
  taxonomic_lookup = marinegeo_metadata$taxonomic_lookup
)

# Precompute functional group enrollment (build-time adjacency table traversal)
marinegeo_metadata$functional_group_enrollment <- .build_functional_group_enrollment(
  tl = marinegeo_metadata$taxonomic_lookup,
  fg = marinegeo_metadata$functional_group_lookup
)

usethis::use_data(marinegeo_metadata, internal = TRUE, overwrite = TRUE)
