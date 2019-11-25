
#' Simulate a Markov Model Sample
#'
#'
#'@description This model agnostic function runs a single markov model for the specified duration. It wraps multiple approaches
#'that may offer various advantages and disadvantages.
#' 
#' @param markov_sample A single row dataframe or a list with no default. See `sample_markov` 
#' for the correct data format.
#' @param type A character string specifying the approach to use to simulate the model. Currently implemented
#' approaches are "base" with "base" as the default.
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
#' simulate_markov(sample[1, ], duration = 10)
#'   
simulate_markov <- function(markov_sample = NULL, 
                            duration = NULL,
                            discount = 1.035, 
                            type = "base",
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

  
  if (type == "base") {
    out <- simulate_markov_base(
      transition = markov_sample[["transition"]],
      cohort = markov_sample[["cohort"]],
      state_cost = markov_sample[["state_cost"]],
      intervention_cost = markov_sample[["intervention_cost"]] , 
      qalys = markov_sample[["qalys"]], 
      duration = duration,
      discount = discount,
      sim = sim)
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
#' @param transition A transition matrix, see `example_two_state_markov` for an example of setting this up.
#' @param cohort A list containing the initial state for each intervention, 
#' see `example_two_state_markov` for an example of setting this up.
#' @param state_cost A list of state costs for each intervention, 
#' see `example_two_state_markov` for an example of setting this up.
#' @param intervention_cost A vector of intervention costs, see `example_two_state_markov` for an example of setting this up.
#' @param qalys A list of QALYs for each intervention, see `example_two_state_markov` for an example of setting this up.
#' @param duration Numeric, how many long to run the model for.
#' @param discount Numeric, the discount that should be applied to the costs and QALYs. Defaults to 1.035.
#' @param sim Matrix with the same number of rows as the duration of the model and the same number of columns as the number of 
#' states in the model. Used to store model simulatons. Rather than setting up for each sample this can be preallocated initially to increase efficiency.
#' @return A list containing total costs and total QALYs as matrices across states
#' and the duration of the model
#' @export
#'
#' @examples
#'   
#'  markov_sample <- sample_markov(example_two_state_markov())
#'  
#'  simulate_markov_base(
#'      transition = markov_sample$transition[[1]],
#'      cohort = markov_sample$cohort[[1]],
#'      state_cost = markov_sample$state_cost[[1]], 
#'      intervention_cost = markov_sample$intervention_cost[[1]], 
#'      qalys = markov_sample$qalys[[1]], 
#'      duration = 10
#'  )  
#'   
#'   
simulate_markov_base <- function(transition = NULL, cohort = NULL, state_cost = NULL, 
                                 intervention_cost = NULL, qalys = NULL, duration = NULL,
                                 discount = 1.035, sim = NULL) {
  
  ## Preallocate
  if (is.null(sim)) {
    sim <- matrix(NA, nrow = duration, ncol = nrow(transition))
    colnames(sim) <- colnames(transition)
  }
  
  ## Assign initial pop
  sim[1, ] <- cohort
  
  ##Loop over the rest of the model 
  for (i in 2:duration) {
    sim[i, ] <- sim[i - 1, ] %*% transition
  }
  
  ## Discounting
  discounting <-  (1 / discount)^(0:(duration - 1))
  
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


