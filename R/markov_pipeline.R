#' Title
#'
#' @param model 
#'
#' @return
#' @export
#' @importFrom furrr future_map_dfr
#' @importFrom dplyr bind_cols
#' @examples
#' 
#' ## Code
#' markov_pipeline
markov_pipeline <- function(model = NULL) {
  
  # Generate samples --------------------------------------------------------
  
  samples <- furrr::future_map_dfr(1:no_samples, ~ SpeedyMarkov::sample_markov(transitions = transitions_list,
                                                                 state_costs = state_costs,
                                                                 intervention_costs = intervention_costs,
                                                                 cohorts = cohorts, qalys = qalys),
                                   .id = "sample", .progress = TRUE)
  
  results <- furrr::future_map_dfr(1:nrow(samples), 
                                   ~ SpeedyMarkov::simulate_markov(transition = samples$transition[[.]],
                                                cohort = samples$cohort[[.]],
                                                state_cost = samples$state_cost[[.]], 
                                                intervention_cost = samples$intervention_cost[[.]], 
                                                qalys = samples$qalys[[.]], 
                                                duration = duration,
                                                discount = discount),
                                   .progress = TRUE)
  
  combined <- dplyr::bind_cols(samples, results)
  
  # Analyse model -----------------------------------------------------------
  
  sum <- SpeedyMarkov::analyse_markov(combined)
  


# Define pipeline output --------------------------------------------------

  
  output <- list(results, sum)
  names(output) <- c("simulations", "cost_effectiveness")
  return(output)
}

