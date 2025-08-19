
build_select_columns_template <- function(){
  
  select_template_text <- paste("df %>%",
                                paste0("\tselect(all_of(column_order))"),
                                sep = "\n")
  
  return(select_template_text)
  
}

build_mutate_case_when_template <- function(selected_points_matrix, 
                                            selected_columns,
                                            expand_selected_values = FALSE,
                                            df){
  
  selected_column_index <- selected_points_matrix[1,2]
  selected_column <- colnames(df[selected_column_index])
  
  mutate_template_text <- paste("df %>%",
                                paste0("\tmutate(", selected_column, " = case_when("),
                                sep = "\n")
  
  if(!expand_selected_values){
    unique_value_indices <- as_tibble(selected_points_matrix) %>%
      filter(V2 == selected_column_index) %>%
      count(V1) %>%
      pull(V1)
    
    unique_values <- unique(df[[selected_column]][unique_value_indices])
    
  } else {
    unique_values <- unique(df[[selected_column]])
    
  }
  
  if(length(selected_columns) > 1){
    
    additional_column_indices <- setdiff(selected_points_matrix[,2], selected_column_index)
    additional_columns <- colnames(df)[additional_column_indices]
    additional_column_string <- c()
    
    for(i in 1:length(additional_columns)){
      
      column_name <- additional_columns[i]
      additional_column_index <- additional_column_indices[i]
      
      unique_value_indices <- as_tibble(selected_points_matrix) %>%
        filter(V2 == additional_column_index) %>%
        count(V1) %>%
        pull(V1)
      
      unique_values_additional_column <- unique(df[[column_name]][unique_value_indices])
      
      if(all(is.numeric(unique_values_additional_column))){
        unique_values_additional_column <- unique_values_additional_column
        
      } else {
        unique_values_additional_column <- paste0("\"", unique_values_additional_column, "\"")
        
      }
      
      if(length(unique_values_additional_column) == 1){
        
        additional_column_string <- c(additional_column_string,
                                      paste0(
                                        " & ", column_name, " == ", unique_values_additional_column
                                      ))
      } else {
        
        additional_column_string <- c(additional_column_string,
                                      paste0(
                                        " & ", column_name, " %in% c(", paste(unique_values_additional_column,
                                                                                collapse = ", "),
                                        ")"
                                      ))
        
      }
    }
    
    additional_column_string <- paste(additional_column_string, collapse = "")
    
  } else {
    additional_column_string <- ""
    
  }
  
  for(i in 1:length(unique_values)){
    
    if(is.numeric(unique_values[i])){
      
      unique_value_string <- unique_values[i]
      
    } else {
      
      unique_value_string <- paste0("\"", unique_values[i], "\"")
      
    }
    
    mutate_template_text <- c(mutate_template_text,
                              paste0(
                                "\t\t", selected_column, " == ", unique_value_string, additional_column_string, " ~ \"\","
                              ))
    
  }
  
  mutate_template_text <- c(mutate_template_text,
                            paste0("\t\tT ~ ", selected_column, "\n\t))")) 
  
  mutate_template_text <- paste(mutate_template_text, collapse = "\n")
  
  return(mutate_template_text)
  
}


build_filter_template <- function(selected_points_matrix, 
                                  df){
  
  filter_template_text <- "df %>%\n\tfilter("
  
  selected_points_tibble <- as_tibble(selected_points_matrix) %>%
    rename(row_index = V1,
           column_index = V2)
  
  unique_column_indices <- unique(selected_points_tibble$column_index)
  
  filter_conditional_string <- ""
  
  for(i in 1:length(unique_column_indices)){
    
    column_index <- unique_column_indices[i]
    
    column_name <- colnames(df)[column_index]
    
    unique_value_indices <- selected_points_tibble %>%
      filter(column_index == !!column_index) %>%
      count(row_index) %>%
      pull(row_index)
    
    unique_values <- unique(df[[column_name]][unique_value_indices])
    
    if(!all(is.numeric(unique_values))){
      unique_values <- paste0("\"", unique_values, "\"")
      
    }
    
    if(length(unique_values) == 1){
      filter_conditional_substring <- paste0(
        "\t\t", column_name, " == ", unique_values
      )
      
    } else {
      filter_conditional_substring <- paste0(
        "\t\t", column_name, " %in% c(", paste(unique_values,
                                                 collapse = ", "),
        ")"
      )
      
    }
    
    if(i != length(unique_column_indices)){
      filter_conditional_substring <- paste0(filter_conditional_substring, " & ")
    }
    
    filter_conditional_string <- c(filter_conditional_string, filter_conditional_substring)
    
  }
  
  filter_conditional_string <- paste(filter_conditional_string, collapse = "\n")
  filter_template_text <- paste0(filter_template_text, filter_conditional_string, "\n\t)")
  
  return(filter_template_text)
  
}
