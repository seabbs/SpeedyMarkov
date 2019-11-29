#' Reference Two State Markov Model
#' @description This is a two state Markov model - modelling smoking cessation - it was adapted from `reference_two_state_markov` 
#' to use the `SpeedyMarkov` framework. It essentially contains a list of functions that are then used to sample a markov model 
#' that can then be simulated and analysed. Unlike `reference_two_state_markov`  this is not a standalone analysis pipeline
#' but instead represents a model definition.
#'
#' @return A named list of functions that all require a samples argument and pass additional arguments (using ...).
#'  The list contains:
#' * transitions_list: a list of transition functions, with the first taking the number of samples as an argument
#' and the following being dependent on the a previous transition.
#' * qalys: a function that samples the qaly cost for each intervention.
#' * intervention_costs: a function that returns the costs for each intervention.
#' * state_costs: a function that returns the state costs for each intervention.
#' * cohorts: a function that returns the initial state for each intervention.
#' 
#' Please  see the code for more details on each required list item.
#' @export
#' @importFrom VGAM rdiric
#' @importFrom stats rnorm
#' @importFrom purrr map transpose
#' @author Sam Abbott
#' @examples 
#' ## Example model run
#' example_two_state_markov()
#'
example_two_state_markov <- function() {
  
  # Transitions -------------------------------------------------------------
  # 1. Specify transition matrices for each intervention
  # Baseline - Soc
  # Pass additional arguments internally
  soc_transition <- function(samples = NULL, ...) {
    # Sample transitions
    tmp <- list(VGAM::rdiric(samples, c(88, 12)),
                VGAM::rdiric(samples, c(8, 92)))
    
    # Arrange as matrices
    tmp <- SpeedyMarkov::matrix_arrange(tmp, ...)
    
    return(tmp)
  }
  
  # Intervention - Soc with website
  # Depends on Soc
  soc_with_website_transition <- function(baseline = NULL, ...) {
    
    #Sample transitions for each baseline matrix
    samples <- length(baseline)
    tmp <- VGAM::rdiric(samples,c(85,15))
    
    # Update baseline
    updated <- purrr::map(1:samples, function(sample) {
      update <- baseline[[sample]]
      update[1, ] <- tmp[sample, ]
      return(update)
      })
    
    return(updated)
  }
  
  
  ## Test
  #soc_trans_sample <- soc_transition()
  # soc_trans_sample
  
  #soc_with_website_trans_sample <- soc_with_website_transition(soc_trans_sample)
  # soc_with_website_trans_sample
  
  #Set up transition list
  transitions_list <- list(soc_transition, 
                           soc_with_website_transition)
  
  names(transitions_list) <- c("SoC", "Soc with Website")
  
  # Qualies -----------------------------------------------------------------
  # 2. Specify qaly costs per intervention (random sampling)

  qalys <- function(samples = NULL, ...) {
    qaly <- function(samples = 1, ...) {
      ## Sample
      tmp <- list(stats::rnorm(samples, mean = 0.95,sd = 0.01) / 2,
                   rep(1 / 2, samples))
      
      out <- SpeedyMarkov::vector_arrange(tmp)
  
      return(out)
    }
    
    soc <- qaly(samples = samples)
    soc_with_website <- soc
    
    out <- list(soc, soc_with_website)
    names(out) <- list("SoC", "Soc with Website")
    
    out <- purrr::transpose(out)
    
    return(out)
  }
  
  # qalys()
  

  # Costs -------------------------------------------------------------------
  # 3. Specify costs per intervention (random sampling)
  
  intervention_costs <- function(samples = NULL, ...) {
    ## Sample
    tmp <- list(rep(0, samples),
                 rep(50, samples))
    
    out <- SpeedyMarkov::vector_arrange(tmp)

    return(out)
  }
  
  # intervention_costs()
  
  state_costs <- function(samples = NULL, ...) {
    state_cost <- function(samples = 1) {
      tmp <- list(rep(0, samples),
                 rep(0, samples))
      
      out <- SpeedyMarkov::vector_arrange(tmp)
      
      return(out)
      
    }
    
    soc <- state_cost(samples = samples)
    soc_with_website <- soc
    
    out <- list(soc, soc_with_website)
    names(out) <- list("SoC", "Soc with Website")
    
    out <- purrr::transpose(out)
    
    return(out)
  }
  
  # state_costs()
  
  # Cohort ------------------------------------------------------------------
  #4. Define cohort
  
  cohorts <- function(samples = NULL, ...) {
    
    cohort <- function(samples = 1) {
      tmp <- list(rep(1, samples),
                  rep(0, samples))
      
      out <- SpeedyMarkov::vector_arrange(tmp)

      return(out)
    }
    
    soc <- cohort(samples = samples)
    soc_with_website <- soc
    
    out <- list(soc, soc_with_website)
    names(out) <- list("SoC", "Soc with Website")
    
    out <- purrr::transpose(out)
    
    return(out)
  }
  
  #cohorts()
  
  
  model <- list(
    transitions_list = transitions_list,
    qalys = qalys,
    intervention_costs = intervention_costs,
    state_costs = state_costs,
    cohorts = cohorts
  )
  
  
  class(model) <- c("SpeedyMarkov", class(model))
  
  return(model)
}
