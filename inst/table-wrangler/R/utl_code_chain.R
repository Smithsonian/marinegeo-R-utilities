# The code chain includes both the code and related attributes stored in input_list as a list()
# The order of the code_chain sub-items reflects the order of execution of the code chunks. 
# It can be re-ordered by the user. 
# input_list
# ... code_chain
# ... ... [[#]] ID name ("code_chain_x") assigned when code chunk is created, where x is an integer
# ... ... ... $code - a character vector to be evaluated as R code
# ... ... ... $attr - a list of attributes associated with the code chunk
# ... ... ... ... $active - T/F, specifying whether or not the code link is active and should be evaluated
# ... ... ... ... $type - DPLYR/CUSTOMFUNCTION
# ... ... ... ... $foo - additional attributes can be defined

import_R_script_to_code_chain <- function(script_path){
  
  script_lines <- readLines(script_path)
  
  # Find the line numbers
  start_line <- grep("## MarineGEO Table Wrangler Start", script_lines)
  end_line <- grep("## MarineGEO Table Wrangler End", script_lines)
  
  script_excerpt <- script_lines[(start_line+1):(end_line - 1)]
  
  code_link_line_nums <- grep("## MarineGEO Code Chain Link", script_excerpt)
  code_link_indices <- 1:length(code_link_line_nums)   
  code_link_ids <- paste0("code_chain_", code_link_indices)
  
  # Return an empty list if the R script has no MarineGEO Code Chain segments
  if(length(code_link_line_nums) == 0){
    return(list())
    
  } else {
    
    initial_code_chain <- setNames(
      lapply(code_link_indices, function(id){
        
        next_id <- id + 1
        code_start_line <- code_link_line_nums[id] + 1
        
        # If there are no more code chunks, the last line is the number of lines in the code excerpt. 
        if(length(code_link_line_nums) < next_id){
          code_end_line <- length(script_excerpt)
        } else {
          code_end_line <- code_link_line_nums[next_id] - 1
        }
        
        code_section <- script_excerpt[code_start_line:code_end_line]
        
        # Each link is evaluated independently in a safe R environment, therefore
        # the trailing "%>%" needs to be removed from each code section unless it's
        # the last code section (assumed to not have a trailing "%>%").
        # Although it violates syntax, we don't want to enforce rules that you cannot have 
        # two code blocks linked by "%>%" within a code section (this may change)
        # Unless it's the last code block, search through the lines starting from the last
        # line and remove the first %>% identified
        for(row_num in length(code_section):1){
          
          if(grepl("%>%", code_section[row_num])){
            code_section[row_num] <- gsub("%>%", "", code_section[row_num])
            break()
          }
        }
        
        # Add "df %>%" to the beginning of each code chain link
        code_text <- paste0(
          "df %>%",
          paste(code_section, collapse = "\n")
        )
        
        if(str_ends(code_text, "\n")){
          
          code_text <- str_sub(code_text, 1, (nchar(code_text) - 1))
          
        }
        
        code_text <- trimws(code_text)
        
        code_type <- "DPLYR"
        
        sub_list <- list(code = code_text,
                         attr = list(active = TRUE,
                                     type = code_type)
        )
        
        return(sub_list)
        
      }), code_link_ids
    )
    
    return(initial_code_chain)
  }
}

write_code_chain_to_R_script <- function(code_chain, file_path){
  
  # Create an empty script following the template
  if(!file.exists(file_path)) {
    file.copy("resources/script_template.R",
              file_path)
  }
  
  script_lines <- readLines(file_path)
  
  # Find the line numbers
  start_line <- grep("## MarineGEO Table Wrangler Start", script_lines)
  end_line <- grep("## MarineGEO Table Wrangler End", script_lines)
  
  length_code_chain <- length(code_chain)
  
  if(length_code_chain > 0){
    
    new_lines_prepended <- c(
      script_lines[1:start_line], 
      "df %>%\n"
    )
    
    code_chain_text_list <- lapply(1:length(code_chain), function(i){
      
      #if(code_chain[[i]]$attr$type == "dplyr"){
      
      text <- paste0("### MarineGEO Code Chain Link - DPLYR\n", 
                     gsub("df %>%", "", code_chain[[i]]$code))
      
      if(i != length_code_chain) {
        text <- paste0(text, " %>%\n")
      } else {
        text <- paste0(text, "\n")
      }
      
      # Prevent line breaks from getting added before the pipe
      text <- gsub("\n %>%", "%>%", text)
      
      return(text)
      
    })
    
    code_chain_text <- unlist(code_chain_text_list)
    
    # Replace the content between the markers (including the markers)
    new_lines <- c(
      new_lines_prepended,
      code_chain_text,
      script_lines[end_line:length(script_lines)]
    )
    
  } else {
    
    new_lines <- c(
      script_lines[1:start_line], 
      script_lines[end_line:length(script_lines)]
    )
    
  }
  # Write the modified content back to the file
  writeLines(new_lines, file_path)
  
}

evalute_in_safe_environment <- function(code_text, df){
  
  # Whitelisting functions for safe computation
  #https://stackoverflow.com/questions/18369913/safely-evaluating-arithmetic-expressions-in-r/18391779#18391779
  
  base_f <- c(
    getGroupMembers("Math"),
    getGroupMembers("Arith"),
    getGroupMembers("Compare"),
    "<-", "{", "(", "c", "&", "|", ":", 
    "if", "T", "F", "~", "`", "!", "%in%",
    "numeric", "as.numeric", "is.numeric",
    "character", "as.character", "is.character",
    "as.POSIXct",
    "substr", "toupper", "tolower", "is.na",
    "paste", "paste0"
  )
  
  tidy_f <- c(
    "filter",
    "tibble",
    "mutate",
    "select",
    "rename",
    "case_when",
    "case_match",
    "%>%",
    "group_by",
    "ymd"
  )
  
  safe_env <- new.env(parent = emptyenv())
  
  for (func in base_f) {
    if (exists(func, envir = baseenv())) {
      assign(func, get(func, envir = baseenv()), envir = safe_env)
    }
  }
  
  for (func in tidy_f) {
    if (exists(func, envir = asNamespace("dplyr"))) {
      assign(func, get(func, envir = asNamespace("dplyr")), envir = safe_env)
    }
  }
  
  assign("df", df, safe_env)
  
  # Convert the escape sequences to their actual characters
  parsed_text <- parse(text = code_text)
  
  # Safely evaluate
  result <- eval(parsed_text, safe_env)
  
  return(result)
  
}
