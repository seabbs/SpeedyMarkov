#' Reference Two State Markov Model
#' @description This is a two state Markov model - modelling smoking cessation - it was adapted from `reference_two_state_markov` 
#' to use the `SpeedyMarkov` framework. It essentially contains a list of functions that are then used to sample a markov model 
#' that can then be simulated and analysed. Unlike `reference_two_state_markov`  this is not a standalone analysis pipeline.
#'
#' @return A named list containing:
#' * transitions_list: a list of transition functions, with the first taking no argument 
#' and the following being dependent on the a previous transition.
#' * qalys: a function that samples the qaly cost for each intervention.
#' * intervention_costs: a function that returns the costs for each intervention.
#' * state_costs: a function that returns the state costs for each intervention.
#' * cohorts: a function that returns the initial state for each intervention.
#' 
#' Please  see the code for more details on each required list item.
#' @export
#' @importFrom VGAM rdiric
#' @author Sam Abbott
#' @examples 
#' ## Example model run
#' example_two_state_markov()
#'
example_two_state_markov <- function() {
  
  
  # Transitions -------------------------------------------------------------
  # 1. Specify transition matrices for each intervention
  # Baseline - Soc
  soc_transition <- function() {
    tmp <- rbind(VGAM::rdiric(1, c(88,12)),
                 VGAM::rdiric(1, c(8,92)))
    
    colnames(tmp) <- c("Smoking", "Not smoking")
    rownames(tmp) <- c("Smoking", "Not smoking")
    
    return(tmp)
  }
  
  # Intervention - Soc with website
  # Depends on Soc
  soc_with_website_transition <- function(baseline = NULL) {
    baseline[1, ] <- VGAM::rdiric(1,c(85,15))
    
    return(baseline)
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

  qalys <- function() {
    qaly <- function(samples = 1) {
      smoking <- rnorm(1, mean = 0.95,sd = 0.01) / 2
      not_smoking <- 1 / 2
      
      out <- c(smoking, not_smoking)
      names(out) <- c("Smoking", "Not smoking")
      
      return(out)
      
    }
    
    soc <- qaly()
    soc_with_website <- soc
    
    out <- list(soc, soc_with_website)
    names(out) <- list("SoC", "Soc with Website")
    
    return(out)
  }
  
  # qalys()
  

  # Costs -------------------------------------------------------------------
  # 3. Specify costs per intervention (random sampling)
  
  intervention_costs <- function(samples = 1) {
    soc <- 0
    soc_with_website <- 50
    
    out <- c(soc, soc_with_website)
    names(out) <- c("SoC", "Soc with Website")
    
    return(out)
  }
  
  # intervention_costs()
  
  state_costs <- function(samples = 1) {
    state_cost <- function(samples = 1) {
      smoking <- 0
      not_smoking <- 0
      
      out <- c(smoking, not_smoking)
      names(out) <- c("Smoking", "Not smoking")
      
      return(out)
      
    }
    
    soc <- state_cost()
    soc_with_website <- soc
    
    out <- list(soc, soc_with_website)
    names(out) <- list("SoC", "Soc with Website")
    return(out)
  }
  
  # state_costs()
  
  # Cohort ------------------------------------------------------------------
  #4. Define cohort
  
  cohorts <- function() {
    
    cohort <- function() {
      smoking <- 1
      not_smoking <- 0
      
      out <- matrix(c(smoking, not_smoking), ncol = 2)
      colnames(out) <- c("Smoking", "Not smoking")
      
      return(out)
    }
    
    soc <- cohort()
    soc_with_website <- soc
    
    out <- list(soc, soc_with_website)
    names(out) <- list("SoC", "Soc with Website")
    
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
