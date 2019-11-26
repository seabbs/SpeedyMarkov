#' Markov Sampling and Simulation Pipeline
#'
#' @param samples Numeric, defaults to 1. The number of markov model samples to use. 
#' @param sample_type A character string specifying the approach to use to sample the model. 
#' Options and defaults inherited from `sample_markov`.
#' @param sim_type A character string specifying the approach to use to simulate the model. 
#' Options and defaults inherited from `simulate_markov`.
#' @param discount Numeric, the discount that should be applied to the costs and QALYs. Defaults to 1.035.
#' @param map_fn An R function used to iterate over the model samples and run simulations. Must accept a functon 
#' as an argument. Defaults to using `purrr::map` if not supplied.
#' @return A list containing the model samples and simulations.
#' @export
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
                                       sample_type = "base",
                                       sim_type = "base",
                                       map_fn = NULL, 
                                       debug = FALSE) {
  
  # Generate samples --------------------------------------------------------
  
  model_samples <- SpeedyMarkov::sample_markov(markov_model,
                                               type = sample_type,
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
   


# Calculate discounting ---------------------------------------------------

  discounting <-  SpeedyMarkov::calc_discounting(discount, duration)  

# Simulate model from samples ---------------------------------------------

   
  ## Map samples to a list for efficiency
  samples_list <- purrr::transpose(model_samples)
  
  ## Default to purrr map function if not supplied
  if (is.null(map_fn)) {
    map_fun <- purrr::map
  }
  
  ## Simulate over samples and interventions
  results <- map_fun(samples_list, function(sample) {
    SpeedyMarkov::simulate_markov(
      markov_sample = sample, 
      duration = duration,
      discounting = discounting, 
      type = sim_type,
      sim = sim_storage,
      input_is_list = TRUE,
      debug = debug)
  })
  
  ## Parallel data frame binding for results from data.table
  results <- data.table::rbindlist(results)
  
  ## Combine samples and simulation results
  combined <- dplyr::bind_cols(model_samples, results)
  
  ## Convert to tibble for ease of interaction
  combined <- tibble::as_tibble(combined)
  
  return(combined)
}