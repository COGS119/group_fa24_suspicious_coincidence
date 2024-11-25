### helper functions ###

##converting test array choices
convert_array <- function(json_txt, column_name) {
  column_name = rlang::as_name(column_name)
  json_txt %>% 
    gather_array() %>% 
    unnest(..JSON) %>%
    rename(index=array.index,!!column_name:=..JSON) %>% 
    as_tibble() %>% 
    #pivot_wider(names_prefix = paste0(column_name,"_"), names_from=index,values_from=stims) %>%
    select(-document.id)
}
