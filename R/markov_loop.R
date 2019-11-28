#' Inner Markov Loop in base R
#'
#' @param sim Matrix with the same number of rows as the duration of the model and the same number of columns as the number of 
#' states in the model. 
#' @param cohort A vector containing the initial state. 
#' @param transition  A transition matrix, see `example_two_state_markov` for an example of setting this up.
#' @param duration Numeric, how many long to run the model for.
#'
#' @return A matrix of the same size as the inputted sim matrix.
#' @export
#'
#' @examples
#' 
#'  
#'transition <- matrix(rnorm(4), 2, 2)
#'sim <- matrix(NA, 10, 2)
#'cohort <- c(1, 0)
#'duration <- 10
#'  
#'markov_loop(sim, cohort, transition, duration)
markov_loop <- function(sim = NULL, cohort, transition = NULL, duration = NULL) {
  ## Assign initial pop			
  sim[1, ] <- cohort
  
  ##Loop over the rest of the model 
  for (i in 2:duration) {
    sim[i, ] <- sim[i - 1, ] %*% transition
  }
  
  return(sim)
}
