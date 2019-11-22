



simulate_markov <- function(transition = NULL, cohort = NULL, state_cost = NULL, 
                       intervention_cost = NULL, qalys = NULL, duration = NULL,
                       discount = NULL) {
  
  ## Preallocate
  sim <- matrix(NA, nrow = duration, ncol = nrow(transition))
  colnames(sim) <- colnames(transition)
  
  ## Assign initial pop
  sim[1, ] <- cohort
  
  ##Loop over the rest of the model 
  for (i in 2:duration) {
    sim[i, ] <- sim[i - 1, ] %*% transition
  }
  
  ## Discounting
  discounting <-  (1 / 1.035)^(0:(duration - 1))
  
  ##Total costs per cycle
  total_costs_cycle <- (sim %*% state_cost) * discounting
  
  ##Total QALYs per cycle
  discounted_qalys <- (sim %*% qalys) * discounting
  total_qalys <- sum(discounted_qalys)
  
  ## Overall costs
  total_costs <- sum(total_costs_cycle) + intervention_cost
  
  out <- tibble::tibble(total_costs = total_costs, total_qalys = total_qalys)
  
  return(out)
}
