
model_output <- function(model) {
  
  summary(model)$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename(
      Coefficient = rowname
    )
  
}  


add_double_colon <- function(x){
  stringr::str_replace_all(string = x, pattern = " ", replacement = ":")
}

check_multi_plus <- function(interaction_input){
  
  check_multi_plus_tib <- 
    tibble::tibble(
      int_original = interaction_input %>% paste(), 
      int_lead = dplyr::lead(interaction_input) %>% paste(), 
      is_plus = dplyr::if_else(interaction_input == "+" & int_lead == "+", 1, 0)
    )
  
  any(check_multi_plus_tib$is_plus == 1)
  
}

process_int_string <- function(interaction_input){
  
  int_string = paste0(interaction_input, collapse = " ")
  
  int_vector = strsplit(int_string, " [+] ")[[1]] %>% 
    stringr::str_remove_all(., pattern = "[+][ +]")
  
  purrr::map(.x = int_vector, .f = add_double_colon) %>% 
    paste0(., collapse = " + ")
  
}