
model_output <- function(model) {
  
  summary(model)$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename(
      Coefficient = rowname
    )
  
}  