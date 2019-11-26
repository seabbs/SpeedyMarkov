context("ArmaMarkovLoop")

test_that("Check that Armadillo Markov loop produces the same input
          as the base R Markov loop.", {
   transition <- matrix(rnorm(4), 2, 2)
   sim <- matrix(NA, 100, 2)
   cohort <- c(1, 0)
   duration <- 100
   
   # Reference R implementation
   sim_r <- markov_loop(sim, cohort, transition, duration)
   
   # RcppArmadillo implementation
   sim_rcppArma <- ArmaMarkovLoop(sim, cohort, transition, duration)
   
   # Check results are within tolerances
   expect_equal(sim_r, sim_rcppArma)
})
