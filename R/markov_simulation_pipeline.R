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
#' @importFrom purrr transpose map
#' @inheritParams simulate_markov
#' @inheritParams sample_markov
#' @seealso sample_markov simulate_markov
#' @examples
#' 
#' 
#' markov_simulation_pipeline(example_two_state_markov(), duration = 10, samples = 2)
#'   
markov_simulation_pipeline <- function(markov_model = NULL, duration = NULL,
                                       discount = 1.035, samples = 1, 
                                       type = "base", debug = FALSE) {
  
  # Generate samples --------------------------------------------------------
  
  model_samples <- SpeedyMarkov::sample_markov(markov_model,
                                               type = type,
                                               debug = debug,
                                               samples = samples)
  

# Allocate simulation matrix ----------------------------------------------

  ## This optional step gives a small speed boost by making preallocation occur once
  ## rather than for every model simulation
  ## Pull out a template transition matrix
  template_transition <- model_samples$transition[[1]]
  ## Set up simulation preallocation
  sim_storage <- matrix(NA, nrow = duration, ncol = nrow(template_transition))
  colnames(sim_storage) <- colnames(template_transition )
   
  

# Simulate model from samples ---------------------------------------------

   
  ## Map samples to a list for efficiency
  samples_list <- purrr::transpose(model_samples)
  
  ## Simulate over samples and interventions
  results <- furrr::future_map(samples_list, 
                               ~ SpeedyMarkov::simulate_markov(
                                 markov_sample = ., 
                                 duration = duration,
                                 discount = discount, 
                                 type = type,
                                 sim = sim_storage,
                                 input_is_list = TRUE,
                                 debug = debug),
                               .progress = TRUE)
  
  ## Parallel data frame binding for results from data.table
  results <- data.table::rbindlist(results)
  
  ## Combine samples and simulation results
  combined <- dplyr::bind_cols(model_samples, results)
  
  ## Convert to tibble for ease of interaction
  combined <- tibble::as_tibble(combined)
  
  return(combined)
}