#' Markov Sampling, Simulation, and Analysis Pipeline
#'
#' @param samples Numeric, defaults to 1. The number of markov model samples to use. 
#' @param type  A character string specifying the approach to use in the modelling pipeline. Currently implemented
#' approaches are "base" with "base" as the default.
#' @return
#' @export
#' @importFrom furrr future_map_dfr
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @inheritParams simulate_markov
#' @inheritParams sample_markov
#' @examples
#' 
#' 
#' markov_pipeline(example_two_state_markov(), duration = 10, samples = 2)
#'   
markov_pipeline <- function(markov_model = NULL, duration = NULL,
                            discount = 1.035, samples = 1, type = "base") {
  
  # Generate samples --------------------------------------------------------
  
  model_samples <- furrr::future_map_dfr(1:samples, ~ SpeedyMarkov::sample_markov(markov_model, type = type),
                                   .id = "sample", .progress = TRUE)
  
  results <- furrr::future_map_dfr(1:nrow(model_samples), 
                                   ~ SpeedyMarkov::simulate_markov(
                                     markov_sample = model_samples[., ], 
                                     duration = duration,
                                     discount = discount, 
                                     type = type) %>% 
                                     tibble::as_tibble(),
                                   .progress = TRUE)
  
  combined <- dplyr::bind_cols(samples, results)
  
  # Analyse model -----------------------------------------------------------
  
  #sum <- SpeedyMarkov::analyse_markov(combined)
  
# Define pipeline output --------------------------------------------------

  
  output <- list(results, sum)
  names(output) <- c("simulations", "cost_effectiveness")
  return(output)
}

