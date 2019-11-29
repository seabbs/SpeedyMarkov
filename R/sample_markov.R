#' Sample a Markov Model
#'
#'
#'@description This model agnostic function samples a markov model specification. It wraps multiple approaches
#'that may offer various advantages and disadvantages.
#' 
#' @param markov_model A list of functions that define a markov model across multiple interventions. See `example_two_state_markov` 
#' for the correct format.
#' @param debug Logical, defaults to \code{FALSE}. Turns on all debug checks - this may impact runtimes.
#' @export
#' @inheritParams sample_markov_base
#' @inherit sample_markov_base return
#' @examples
#' 
#' sample <- sample_markov(example_two_state_markov())
#'   
#' sample
sample_markov <- function(markov_model = NULL, 
                          type = "rcpp", 
                          debug = FALSE, 
                          samples = 1) { 
  
  if (debug) {
    if (!is.list(markov_model)) {
      stop("The markov model must be supplied as a list.
         See SpeedyMarkov::example_two_state_markov for details of the required data format.")
    }
  }


  out <- sample_markov_base(
    transitions = markov_model[["transitions_list"]],
    cohorts = markov_model[["cohorts"]],
    state_costs = markov_model[["state_costs"]], 
    intervention_costs = markov_model[["intervention_costs"]], 
    qalys = markov_model[["qalys"]],
    samples = samples,
    type = type
  )

  
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
#' @param type A character string specifying the approach to use to sample the model. Currently implemented
#' approaches are "base" and "rcpp" with "rcpp" as the default.
#' @return A data.frame of samples of a model encoded in the `SpeedyMarkov` format (see `example_two_state_markov` for details).
#' @export
#' @importFrom purrr map transpose
#' @importFrom tibble tibble
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
                               qalys = NULL, samples = 1, type = "rcpp") {
  
  #sample baseline transition matrix
  baseline <- transitions[[1]](samples = samples, type = type)
  
  #sample all interventions depending on baseline
  interventions <- purrr::map(2:length(transitions), ~ transitions[[.]](baseline, type = type))
  
  #update transitions as a single sample
  transitions[[1]] <- baseline
  transitions[-1] <- interventions
  
  ## Organise as dataframe
  ## Unlist interventions
  samples_df <- tibble::tibble(
                             sample = unlist(purrr::map(1:samples, 
                                                        ~ rep(., length(transitions)))),
                             intervention = rep(names(transitions), samples),
                             transition = purrr::flatten(purrr::transpose(transitions)), 
                             state_cost = purrr::flatten(state_costs(samples = samples, type = type)), 
                             intervention_cost = unlist(intervention_costs(samples = samples, type = type)),
                             cohort = purrr::flatten(cohorts(samples = samples, type = type)),
                             qalys = purrr::flatten(qalys(samples = samples, type = type)))
  

  return(samples_df)
}



