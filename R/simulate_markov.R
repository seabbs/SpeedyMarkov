
#' Simulate a Markov Model Sample
#'
#'
#'@description This model agnostic function runs a single markov model for the specified duration. It wraps multiple approaches
#'that may offer various advantages and disadvantages.
#' 
#' @param markov_sample A single row dataframe or a list with no default. See `sample_markov` 
#' for the correct data format.
#' @param type A character string specifying the approach to use to simulate the model. Currently implemented
#' approaches are "base", "armadillo_inner" and "armadillo_all with "armadillo_inner" as the default.
#' "armadillo_all" is likely to be generally faster but has a slightly reduced feature set.
#' @param debug Logical, defaults to \code{FALSE}. Turns on all debug checks - this may impact runtimes.
#' @param input_is_list Logical defaults to NULL. What type of input is `markov_sample`? A list or a dataframe.
#' If not given then the input type will be checked and converted to a list as required. 
#' @importFrom purrr transpose
#' @export
#' @inherit simulate_markov_base
#' @examples
#' 
#' sample <- sample_markov(example_two_state_markov())
#'   
#' simulate_markov(sample[1, ], duration = 10, discounting = SpeedyMarkov::calc_discounting(1.035, 10))
#'   
simulate_markov <- function(markov_sample = NULL, 
                            duration = NULL,
                            discounting = NULL, 
                            type = "armadillo_all",
                            debug = FALSE,
                            input_is_list = NULL,
                            sim = NULL) { 
  
  if(debug) {
    if (!is.list(markov_sample)) {
      stop("The markov sample must be supplied as a list or dataframe (1 row).
         See SpeedyMarkov::sample_markov for details of the required data format.")
    }
    
    if (tibble::is.tibble(markov_sample) && nrow(markov_sample) > 1) {
      stop("Only 1 sample may be simulated at a time.")
      
    }
  
    if (type == "armadillo_all" & is.null(sim)) {
      stop("This implementation requires the sim matrix to be prespecified. 
            See the docs for details.")
    }
  }
  
  ## If the input type is not given check for a dataframe and convert as required
  if (is.null(input_is_list) | isFALSE(input_is_list)) {
    if (is.data.frame(markov_sample)) {
      # Flip input format into a nested list
      markov_sample <- purrr::transpose(markov_sample)
      # Extract the first object from the list
      markov_sample <- markov_sample[[1]]
    }
  }
  
  ## Assign transition
  transition <- markov_sample[["transition"]]

  ## Preallocate
  if (is.null(sim)) {
    sim <- matrix(NA, nrow = duration, ncol = nrow(transition))
  }
  
  if (type == "base") {
    out <- SpeedyMarkov::simulate_markov_base(      
      transition = transition,
      cohort = markov_sample[["cohort"]],
      state_cost = markov_sample[["state_cost"]],
      intervention_cost = markov_sample[["intervention_cost"]] , 
      qalys = markov_sample[["qalys"]], 
      duration = duration,
      discounting = discounting,
      sim = sim,
      markov_loop_fn = SpeedyMarkov::markov_loop
    )
  }else if (type == "armadillo_inner") {
    out <- SpeedyMarkov::simulate_markov_base(      
      transition = transition,
      cohort = markov_sample[["cohort"]],
      state_cost = markov_sample[["state_cost"]],
      intervention_cost = markov_sample[["intervention_cost"]] , 
      qalys = markov_sample[["qalys"]], 
      duration = duration,
      discounting = discounting,
      sim = sim,
      markov_loop_fn = SpeedyMarkov::ArmaMarkovLoop
    )
  }else if (type == "armadillo_all") {
    out <- SpeedyMarkov::ArmaSimulateMarkov(      
      transition = transition,
      cohort = markov_sample[["cohort"]],
      state_cost = markov_sample[["state_cost"]],
      intervention_cost = markov_sample[["intervention_cost"]] , 
      qalys = markov_sample[["qalys"]], 
      duration = duration,
      discounting = discounting,
      sim = sim
    )
  }
  
  return(out)
}

#' Simulate a Markov Model Sample using Base R
#'
#'
#' @description This model agnostic function runs a single markov model for the specified duration using a base R implementation.
#'  See `example_two_state_markov` for an example of the required input. Alternatively use `sample_markov(type = "base")` and 
#'  the output from `sample_markov`.
#' 
#' see `example_two_state_markov` for an example of setting this up.
#' @param state_cost A list of state costs for each intervention, 
#' see `example_two_state_markov` for an example of setting this up.
#' @param intervention_cost A vector of intervention costs, see `example_two_state_markov` for an example of setting this up.
#' @param qalys A list of QALYs for each intervention, see `example_two_state_markov` for an example of setting this up.
#' @param discounting Numeric vector, the discount that should be applied to the costs and QALYs for each time period. 
#' This must be the same legnth as `duration`.
#' @param sim Matrix with the same number of rows as the duration of the model and the same number of columns as the number of 
#' states in the model. Used to store model simulatons.
#' @param markov_loop_fn A function, defaults to ] \code{NULL}. The function to use to solve the inner markov loops. Built in examples
#' are `markov_loop` (using `R`) and `ArmaMarkovLoop` (using `RcppArmadillo`)
#' @return A list containing total costs and total QALYs as matrices across states
#' and the duration of the model
#' @inheritParams markov_loop
#' @export
#'
#' @examples
#'   
#'  markov_sample <- sample_markov(example_two_state_markov())
#'  
#'  simulate_markov_base(
#'      sim = matrix(NA, nrow = duration, ncol = nrow(markov_sample$transition[[1]])),
#'      transition = markov_sample$transition[[1]],
#'      cohort = markov_sample$cohort[[1]],
#'      state_cost = markov_sample$state_cost[[1]], 
#'      intervention_cost = markov_sample$intervention_cost[[1]], 
#'      qalys = markov_sample$qalys[[1]], 
#'      duration = 10,
#'      discounting = SpeedyMarkov::calc_discounting(1.035, 10),
#'      markov_loop_fn = SpeedyMarkov::markov_loop
#'  )  
#'   
#'   
simulate_markov_base <- function(transition = NULL, cohort = NULL, state_cost = NULL, 
                                 intervention_cost = NULL, qalys = NULL, duration = NULL,
                                 discounting = NULL, sim = NULL, 
                                 markov_loop_fn = NULL) {
  
  # Simulate model over the loop
  sim <-  markov_loop_fn(sim, cohort, transition, duration)
  
  ##Total costs per cycle
  total_costs_cycle <- (sim %*% state_cost) * discounting
  
  ##Total QALYs per cycle
  discounted_qalys <- (sim %*% qalys) * discounting
  total_qalys <- sum(discounted_qalys)
  
  ## Overall costs
  total_costs <- sum(total_costs_cycle) + intervention_cost
  
  out <- list(total_costs = total_costs, total_qalys = total_qalys)
  
  return(out)
}


