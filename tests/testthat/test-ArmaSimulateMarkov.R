context("ArmaSimulateMarkov")

test_that("Check that Armadillo Markov simulation produces the same input
          as the base R Markov simulation.", {
             ## Sample model
   markov_sample <- sample_markov(example_two_state_markov())
   
   ## Simulate using R
   sim_r <- simulate_markov_base(
      ## Specify the storage simulation matrix to maintain consistency 
      ##here (but not needed for the base implementation).
      sim =  matrix(NA, nrow = 10, ncol = nrow(markov_sample$transition[[1]])),
      transition = markov_sample$transition[[1]],
      cohort = markov_sample$cohort[[1]],
      state_cost = markov_sample$state_cost[[1]], 
      intervention_cost = markov_sample$intervention_cost[[1]], 
      qalys = markov_sample$qalys[[1]], 
      duration = 10,
      discounting = SpeedyMarkov::calc_discounting(1.035, 10),
      markov_loop_fn = SpeedyMarkov::markov_loop
   )  
   
   # RcppArmadillo implementation
   sim_rcppArma <- ArmaSimulateMarkov(
      sim =  matrix(NA, nrow = 10, ncol = nrow(markov_sample$transition[[1]])),
      transition = markov_sample$transition[[1]],
      cohort = markov_sample$cohort[[1]],
      state_cost = markov_sample$state_cost[[1]], 
      intervention_cost = markov_sample$intervention_cost[[1]], 
      qalys = markov_sample$qalys[[1]], 
      duration = 10,
      discounting = SpeedyMarkov::calc_discounting(1.035, 10)
   )  
   
   # Check results are within tolerances
   expect_equal(sim_r, sim_rcppArma)
})
