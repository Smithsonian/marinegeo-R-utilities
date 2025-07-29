
utl_rls_run_all_qc <- function(df, output_table){
  
  if(output_table == "reef-life-survey-data-marinegeo-v1"){

    results_list <- list(
      
      "missing_metadata" = list(
        results = marinegeo.utils::qc_rls_empty_metadata(df),
        description = "Return any rows with NA or BLANK values",
        id = 1
      ),
      
      "missing_aphia_id" = list(
        results = marinegeo.utils::qc_mg_missing_taxonomic_id(df),
        description = "Return any rows with a missing Aphia ID",
        id = 2
      ),
      
      "verts_with_invert_count" = list(
        results = marinegeo.utils::qc_rls_verts_with_invert_count(df),
        description = "Return any rows with an verteberate that has an invertebrate count > 0",
        id = 3
      ),
      
      "inverts_with_fish_size" = list(
        results = marinegeo.utils::qc_rls_inverts_with_sizes(df),
        description = "Return any rows with an invertebrate that has a count > 0 in a fish count column",
        id = 4
      ),
      
      "total_count_of_0" = list(
        results = marinegeo.utils::qc_rls_total_count_0(df),
        description = "Return any rows with observations with a count of 0",
        id = 5
      ),
      
      "fish_count_over_100" = list(
        results = marinegeo.utils::qc_rls_fish_count_over_100(df),
        description = "Return any rows that have a count > 100 for a fishie",
        id = 6
      ),
      
      "inverts_over_20" = list(
        results = marinegeo.utils::qc_rls_inverts_over_20(df),
        description = "Return any rows that have a count > 20 for an invertebrate",
        id = 7
      )
   
    )   
  } else {
    results_list <- list()
  }
  
  return(results_list)
  
}
