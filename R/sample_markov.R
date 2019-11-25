#' Sample a Markov Model
#'
#'
#'@description This model agnostic function samples a markov model specification. It wraps multiple approaches
#'that may offer various advantages and disadvantages.
#' 
#' @param markov_model A list of functions that define a markov model across multiple interventions. See `example_two_state_markov` 
#' for the correct format.
#' @param type A character string specifying the approach to use to simulate the model. Currently implemented
#' approaches are "base" with "base" as the default.
#' @param debug Logical, defaults to \code{FALSE}. Turns on all debug checks - this may impact runtimes.
#' @export
#' @inherit sample_markov_base return
#' @examples
#' 
#' sample <- sample_markov(example_two_state_markov())
#'   
#' sample
sample_markov <- function(markov_model = NULL, 
                          type = "base", 
                          debug = FALSE, 
                          samples = 1) { 
  
  if (debug) {
    if (!is.list(markov_model)) {
      stop("The markov model must be supplied as a list.
         See SpeedyMarkov::example_two_state_markov for details of the required data format.")
    }
  }

  
  if (type == "base") {
    out <- sample_markov_base(
      transitions = markov_model[["transitions_list"]],
      cohort = markov_model[["cohorts"]],
      state_cost = markov_model[["state_costs"]], 
      intervention_cost = markov_model[["intervention_costs"]], 
      qalys = markov_model[["qalys"]],
      samples = samples
    )
  }
  
  return(out)
}

#' Sample a Markov Model Sample using Base R
#'
#'
#' @description This model agnostic function samples a markov model specification using a base R implementation.
#'  See `example_two_state_markov` for an example of the required input. Alternatively use `sample_markov(type = "base")` passing the 
#'  model specification function.
#' 
#' @param transitions A function that generates a list of transition matrices, 
#' see `example_two_state_markov` for an example of setting this up.
#' @param cohorts A function that generates a list containing the initial state for each intervention, 
#' see `example_two_state_markov` for an example of setting this up.
#' @param state_costs A function that generates a list of state costs for each intervention, 
#' see `example_two_state_markov` for an example of setting this up.
#' @param intervention_costs A function that generates a vector of intervention costs, see `example_two_state_markov` for an example of setting this up.
#' @param qalys A function that generates a list of QALYs for each intervention, see `example_two_state_markov` for an example of setting this up.
#' @param samples Numeric, defaults to 1. The number of samples to take from the Markov model
#' @return A data.frame of samples of a model encoded in the `SpeedyMarkov` format (see `example_two_state_markov` for details).
#' @export
#' @importFrom purrr map transpose
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @examples
#'   
#'   
#'  markov_model <- example_two_state_markov()
#'   
#'  sample_markov_base(
#'      transitions = markov_model$transitions_list,
#'      cohorts = markov_model$cohorts,
#'      state_costs = markov_model$state_costs, 
#'      intervention_costs = markov_model$intervention_costs, 
#'      qalys = markov_model$qalys
#'  ) 
#'   
sample_markov_base <- function(transitions = NULL, state_costs = NULL,
                               intervention_costs = NULL, cohorts = NULL, 
                               qalys = NULL, samples = 1) {
  
  #sample baseline transition matrix
  baseline <- transitions[[1]](samples = samples)
  
  #sample all interventions depending on baseline
  interventions <- purrr::map(2:length(transitions), ~ transitions[[.]](baseline))
  
  #update transitions as a single sample
  transitions[[1]] <- baseline
  transitions[-1] <- interventions

  
  ## Organise as data.table - this section could easily be optimised
  samples <- data.table::data.table(sample = 1:samples,
                           intervention = list(names(transitions)),
                           transition = purrr::transpose(transitions), 
                           state_cost = state_costs(samples = samples), 
                           intervention_cost = intervention_costs(samples = samples),
                           cohort = cohorts(samples = samples),
                           qalys = qalys(samples = samples))
  
  
  ## unnest data for readability/usability etc. 
  samples <-  samples[, .(intervention = unlist(intervention),
                          transition = purrr::flatten(transition),
                          state_cost = purrr::flatten(state_cost),
                          intervention_cost = unlist(intervention_cost),
                          cohort = purrr::flatten(cohort),
                          qalys = purrr::flatten(qalys)),
                      by = sample]
  
  samples <- tibble::as_tibble(samples)
  
  return(samples)
}



