#' Markov Sampling, Simulation, and Cost Effectiveness Analysis Pipeline
#'
#' @return A list containing the model samples and simulations and cost effectiveness summary measures.
#' @export
#' @inheritParams markov_simulation_pipeline
#' @inheritParams analyse_ce
#' @seealso markov_simulation_pipeline analyse_ce
#' @examples
#' 
#' markov_ce_pipeline(example_two_state_markov(), duration = 10, samples = 5)
#'   
markov_ce_pipeline <- function(markov_model = NULL, duration = NULL,
                            discount = 1.035, samples = 1, baseline = 1,
                            willingness_to_pay_thresold = 20000,
                            sample_type = "base", sim_type = "base", debug = FALSE, 
                            batches = 1, batch_fn = NULL, ...) {
  
  

  # Sample and simulation markov --------------------------------------------
  simulations <- SpeedyMarkov::markov_simulation_pipeline(markov_model = markov_model, 
                                            duration = duration,
                                            discount = discount, 
                                            samples = samples,
                                            sample_type = sample_type,
                                            sim_type = sim_type,
                                            batches = batches, 
                                            batch_fn =  batch_fn,
                                            debug = debug, ...)
  
  # Analyse model -----------------------------------------------------------
  
  sum <- SpeedyMarkov::analyse_ce(simulations, baseline = baseline,
                                  willingness_to_pay_thresold = willingness_to_pay_thresold)
  
  return(sum)
}




