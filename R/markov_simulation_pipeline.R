#' Markov Sampling and Simulation Pipeline
#'
#' @description This functions wraps multiple modular functions and allows an end-to-end cost effectiveness to 
#' be run,  excluding the final analysis of the findings. It may also be used in batch mode to run analyses in 
#' parallel.
#' @param samples Numeric, defaults to 1. The number of markov model samples to use. 
#' @param sample_type A character string specifying the approach to use to sample the model. 
#' Options and defaults inherited from `sample_markov`.
#' @param sim_type A character string specifying the approach to use to simulate the model. 
#' Options and defaults inherited from `simulate_markov`.
#' @param discount Numeric, the discount that should be applied to the costs and QALYs. Defaults to 1.035.
#' @param batches Numeric, defaults to 1. The number of batches to run simulation/sampling in. When set to 
#' values greater than 1 a `batch_fn` must also be supplied. It is likely that the most efficient option will
#' be to use a batch number that corresponds to the number of cores being utilised.
#' @param batch_fn Function, defaults to `NULL`. This is the function to be used to parallise across batches. Potential options 
#' include `parallel::mclapply` (not Windows) or `furrr::future_map` (requires the use of `future::plan` outside the function). When
#' not given the function will default to using no batching.
#' @param ... Additional options to pass to `batch_fn`. For example this may be the `mc.cores` argument of `parallel::mclapply`.
#' @return A list containing the model samples and simulations.
#' @export
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom purrr transpose map
#' @inheritParams simulate_markov
#' @inheritParams sample_markov
#' @seealso sample_markov simulate_markov
#' @examples
#' 
#' 
#' markov_simulation_pipeline(example_two_state_markov(), duration = 10, samples = 2)
markov_simulation_pipeline <- function(markov_model = NULL, duration = NULL,
                                       discount = 1.035, samples = 1, 
                                       sample_type = "rcpp",
                                       sim_type = "armadillo_all",
                                       batches = 1, 
                                       batch_fn = NULL,
                                       debug = FALSE,
                                       ...) {
  
  # Define pipeline ---------------------------------------------------------
  
  markov_simulation_pipeline_inner <- function(samples = NULL) {
    
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
    
    ## Simulate over samples and interventions
    results <- purrr::map(samples_list, function(sample) {
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
  
    return(combined)
  }

# Check batch instructions ------------------------------------------------

  if (is.null(batch_fn) & batches > 1) {
    message("No batch function has been supplied so falling back to not using batching (batches = 1)")
    batches <- 1
  }
  
  if (batches > samples) {
    stop("The number of batches should not be greater than the number of samples")
  }
  
# Simulate/Sample single/batch ----------------------------------------

if (batches == 1)  {
  ##Single batch (i.e no parallisation)
  combined <-  markov_simulation_pipeline_inner(samples = samples)
}else{
  
  ## Find samples that won't divide evenly into batches
  div_diff_samples <- samples %% batches
  
  ## Divide divisible samples into batch sizes
  diff_samples <- (samples - div_diff_samples) / batches
  
  ## Make batch sample vector
  batch_samples <- rep(diff_samples, batches)
  
  ## Add non-divisible batches to final batch
  batch_samples[batches] <- batch_samples[batches] + div_diff_samples
  
  ## Run batch simulations
  combined <- batch_fn(batch_samples, function(batch_sample){
    markov_simulation_pipeline_inner(samples = batch_sample)
  }, ...)
  
  ## Combine batch simulations
  combined <- data.table::rbindlist(combined)
}
  
  ## Convert to tibble for ease of interaction once returned
  combined <- tibble::as_tibble(combined)
  
  return(combined)
}