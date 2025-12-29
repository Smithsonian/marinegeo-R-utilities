#return details from a selected row based qc test
utl_get_qc_details <- function(input_list){
if(input_list$selected_flag == "invalid_categorical_values"){

  table <- qc_mg_categorical_values_details(input_list$out_df, input_list$output_table_id)

}

  return(table)
}
