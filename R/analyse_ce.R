#' Analyse the Cost Effectiveness of Interventions
#'
#'
#'@description This function produces cost effectiveness summary measures using the output of `markov_simulation_pipeline`
#' or similar data structures. At least two intervetions must be present.
#' @param markov_simulations A dataframe of markov samples and simulations as produced by `markov_simulation_pipeline`. At least two 
#' interventions must be present.
#' @param baseline Numeric, the intervetion to consider as the baseline for pairwise comparisons.
#' @param willingness_to_pay_thresold Numeric, defaulting to 20,000. This is the thresold at which an intervention
#' may be considered cost effective in the UK.
#' @param type A character string specifying the approach to use to simulate the model. Currently implemented
#' approaches are "base" with "base" as the default.
#' @return A list of dataframes including: Cost effectiveness measures for each sample, and summarised cost effectiveness 
#' measures across samples.
#' @export
#' @importFrom dplyr group_by mutate ungroup summarise n
#' @importFrom stats sd
#' @examples
#' 
#' sims <-  markov_simulation_pipeline(example_two_state_markov(),
#'                                     duration = 10, samples = 10)
#'   
#' analyse_ce(sims)
#'   
analyse_ce <- function(markov_simulations = NULL, 
                       baseline = 1,
                       willingness_to_pay_thresold = 20000,
                       type = "base") { 

  ## NULL out variables to deal with package notes
  total_costs <- NULL; total_qalys <- NULL; incremental_qalys <- NULL; incremental_costs <- NULL;
  intervention <- NULL; incremental_net_benefit <- NULL; mean_costs <- NULL; mean_qalys <- NULL;
  total_costs <- NULL; total_costs <- NULL;
  
  ## Incremental costs and qalys
  incremental_sims <- markov_simulations %>% 
    dplyr::group_by(sample) %>% 
    dplyr::mutate(incremental_costs = total_costs - total_costs[baseline],
                  incremental_qalys = total_qalys - total_qalys[baseline],
                  incremental_net_benefit =
                    willingness_to_pay_thresold * incremental_qalys - incremental_costs) %>% 
    dplyr::ungroup()
              
   
  ## Summarise costs - this could be shorter and/or may be incorrect.
  summarised_sims <-  incremental_sims %>% 
    dplyr::group_by(intervention) %>% 
    dplyr::summarise(
      mean_costs = mean(total_costs),
      sd_costs = stats::sd(total_costs),
      mean_qalys = mean(total_qalys),
      sd_qlays = stats::sd(total_qalys),
      mean_incremental_qalys = mean(incremental_qalys),
      sd_incremental_qlays = stats::sd(incremental_qalys),
      mean_incremental_costs = mean(incremental_costs),
      sd_incremental_costs = stats::sd(incremental_costs),
      mean_incremental_net_benefit = mean(incremental_net_benefit),
      sd_incremental_net_benefit = stats::sd(incremental_net_benefit),
      probability_cost_effective = sum(incremental_net_benefit > 0) / dplyr::n()
    ) %>% 
    dplyr::mutate(icer = mean_costs / mean_qalys) %>% 
    dplyr::ungroup()

  
  output <- list(incremental_sims, summarised_sims)
  
  names(output) <- c("simulations_with_ce", "summarised_ce")
  
  return(output)
}
