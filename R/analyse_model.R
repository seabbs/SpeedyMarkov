analyse_model <- function(results = NULL) {
  
  ## Work out incremental costs
  
  ## Work out mean costs
  mean_costs <- results %>% 
    dplyr::group_by(intervention) %>% 
    dplyr::summarise(
      mean_costs = mean(total_costs),
      mean_qalys = mean(total_qalys)
    ) %>% 
    dplyr::ungroup()
}