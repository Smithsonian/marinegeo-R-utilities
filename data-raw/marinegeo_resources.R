## code to prepare `marinegeo_resources` dataset goes here
# The list is used internally by R functions

marinegeo_resources <- list(

  taxonomic_ids = readr::read_csv(
    list.files(paste0(Sys.getenv("repository_filepath"),
                      "marinegeo-taxonomy-lookup/taxonomic-ids/"),
               full.names = T)
  ),

  taxonomic_classifications = readr::read_csv(
    list.files(paste0(Sys.getenv("repository_filepath"),
                      "marinegeo-taxonomy-lookup/taxonomic-classifications/"),
               full.names = T)
  ),

  data_index = readr::read_csv("inst/marinegeo_data_index.csv"),

  table_relationships = readr::read_csv("inst/marinegeo_table_relationships.csv"),

  database_structure = readr::read_csv(
    list.files("inst/data-structure/",
               full.names = T)
  )

)

usethis::use_data(marinegeo_resources, internal = TRUE, overwrite = TRUE)
