#' Filling missing metadata columns based on other processed RLS data
#'
#' For columns "vis", "direction", "time", and "photoquadrats", the function
#' converts "BLANK" to NA and checks processed L2 data for fill values from
#' the same survey.
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
utl_rls_fill_missing_metadata <- function(df){

  columns <- c("sample_event_id", "vis", "direction", "time", "photoquadrats")

  ids <- unique(df$sample_event_id)

  df_in <- df %>%
    mutate(vis = case_when(
      vis == "BLANK" ~ NA,
      T ~ vis
    )) %>%
    mutate(direction = case_when(
      direction == "BLANK" ~ NA,
      T ~ direction
    )) %>%
    mutate(photoquadrats = case_when(
      photoquadrats == "BLANK" ~ NA,
      T ~ photoquadrats
    ))

  if(is.character(df_in$time)){
    df_in <- df_in %>%
      mutate(time = case_when(
        time == "BLANK" ~ NA,
        T ~ time
      ))
  }

  fill_options <- marinegeo.utils::db_marinegeo_L2("reef-life-survey-data-marinegeo-v1") %>%
    filter(sample_event_id %in% ids) %>%
    select(all_of(columns)) %>%
    distinct() %>%
    mutate(vis = case_when(
      vis == "BLANK" ~ NA_character_,
      T ~ vis
    )) %>%
    mutate(direction = case_when(
      direction == "BLANK" ~ NA_character_,
      T ~ direction
    )) %>%
    mutate(photoquadrats = case_when(
      photoquadrats == "BLANK" ~ NA_character_,
      T ~ photoquadrats
    )) %>%
    mutate(time = case_when(
      time == "BLANK" ~ NA_character_,
      T ~ time
    )) %>%
    collect() %>%
    group_by(sample_event_id) %>%
    summarize(vis = first(vis),
              direction = first(direction),
              photoquadrats = first(photoquadrats),
              time = first(time))


  if(is.numeric(df_in$vis)){
    df_in <- df_in %>%
      mutate(vis = as.character(vis))
  }

  if(!is.character(time)){
    df_in <- df_in %>%
      mutate(time = as.character(time))
  }

  df_out <- df_in %>%
    rows_patch(fill_options, by = "sample_event_id")

  return(df_out)

}
