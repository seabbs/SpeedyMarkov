#' Markov Sampling and Simulation Pipeline
#'
#' @param samples Numeric, defaults to 1. The number of markov model samples to use. 
#' @param type  A character string specifying the approach to use in the modelling pipeline. Currently implemented
#' approaches are "base" with "base" as the default.
#' @return A list containing the model samples and simulations.
#' @export
#' @importFrom furrr future_map
#' @importFrom data.table rbindlist
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @inheritParams simulate_markov
#' @inheritParams sample_markov
#' @seealso sample_markov simulate_markov
#' @examples
#' 
#' 
#' markov_simulation_pipeline(example_two_state_markov(), duration = 10, samples = 2)
#'   
markov_simulation_pipeline <- function(markov_model = NULL, duration = NULL,
                                       discount = 1.035, samples = 1, type = "base") {
  
  # Generate samples --------------------------------------------------------
  
  model_samples <- furrr::future_map(1:samples, ~ SpeedyMarkov::sample_markov(markov_model, type = type),
                                     .progress = TRUE)
  
  ## Parallel data frame binding for samples from data.table
  model_samples <- data.table::rbindlist(model_samples, idcol = "sample") 
  
  results <- furrr::future_map(1:nrow(model_samples), 
                               ~ SpeedyMarkov::simulate_markov(
                                 markov_sample = model_samples[., ], 
                                 duration = duration,
                                 discount = discount, 
                                 type = type) %>% 
                                 tibble::as_tibble(),
                               .progress = TRUE)
  
  ## Parallel data frame binding for results from data.table
  results <- data.table::rbindlist(results)
  
  ## Combine samples and simulation results
  combined <- dplyr::bind_cols(model_samples, results)
  
  ## Convert to tibble for ease of interaction
  combined <- as_tibble(combined)
  
  return(combined)
}