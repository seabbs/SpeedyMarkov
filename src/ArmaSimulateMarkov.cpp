// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

//' @title An inner Markov loop implemented using RcppArmadillo
//' @inherit markov_loop
//' @export
//' @useDynLib SpeedyMarkov, .registration=TRUE
//' @examples
//' 
//' transition <- matrix(rnorm(4), 2, 2)
//' sim <- matrix(NA, 100, 2)
//' cohort <- c(1, 0)
//' duration <- 100
//' 
//' # Reference R implementation
//' sim_r <- markov_loop(sim, cohort, transition, duration)
//' 
//' # RcppArmadillo implementation
//' sim_rcppArma <- ArmaMarkovLoop(sim, cohort, transition, duration)
//' 
//' # Check results are within tolerances
//' all.equal(sim_r, sim_rcppArma)
//' 
//' # Benchmark
//' library(microbenchmark)
//' microbenchmark(markov_loop(sim, cohort, transition, duration), 
//'                ArmaMarkovLoop(sim, cohort, transition, duration), 
//'                times = 1000)
// [[Rcpp::export]]
arma::mat ArmaMarkovLoop(arma::mat sim, arma::rowvec cohort, arma::mat transition, int duration){
  
  sim.row(0) = cohort;
  
  for(int i = 1; i < duration; i++){
    sim.row(i) = sim.row(i-1) * transition;
  }
  
  return sim;
}


//' @title Simulate a Markov Model Sample using RcppArmadillo
//' 
//' @description This model agnostic function runs a single markov model for the specified duration using a Armadillo implementation.
//'  See `example_two_state_markov` for an example of the required input. Alternatively use `sample_markov(type = "base")`
//'   and the output from `sample_markov`. Unlike `simulate_markov_base` this implementation requires the prespecification of the
//'   simulation storage matrix.
//' @inherit simulate_markov_base
//' @export
//' @useDynLib SpeedyMarkov, .registration=TRUE
//' @examples
//' 
//' ## Sample model
//' markov_sample <- sample_markov(example_two_state_markov())
//' 
//' ## Simulate using R
//' sim_r <- simulate_markov_base(
//'         ## Specify the storage simulation matrix to maintain consistency here (but not needed for the base implementation).
//'         sim =  matrix(NA, nrow = 10, ncol = nrow(markov_sample$transition[[1]])),
//'         transition = markov_sample$transition[[1]],
//'         cohort = markov_sample$cohort[[1]],
//'         state_cost = markov_sample$state_cost[[1]], 
//'         intervention_cost = markov_sample$intervention_cost[[1]], 
//'         qalys = markov_sample$qalys[[1]], 
//'         duration = 10,
//'         discounting = SpeedyMarkov::calc_discounting(1.035, 10),
//'         markov_loop_fn = SpeedyMarkov::markov_loop
//')  
//' 
//' # RcppArmadillo implementation
//' sim_rcppArma <- ArmaSimulateMarkov(
//'         sim =  matrix(NA, nrow = 10, ncol = nrow(markov_sample$transition[[1]])),
//'         transition = markov_sample$transition[[1]],
//'         cohort = markov_sample$cohort[[1]],
//'         state_cost = markov_sample$state_cost[[1]], 
//'         intervention_cost = markov_sample$intervention_cost[[1]], 
//'         qalys = markov_sample$qalys[[1]], 
//'         duration = 10,
//'         discounting = SpeedyMarkov::calc_discounting(1.035, 10)
//')  
//' 
//' # Check results are within tolerances
//' all.equal(sim_r, sim_rcppArma)
//' 
//' # Benchmark
//' library(microbenchmark)
//' microbenchmark(simulate_markov_base(
//'         sim =  matrix(NA, nrow = 100, ncol = nrow(markov_sample$transition[[1]])),
//'         transition = markov_sample$transition[[1]],
//'         cohort = markov_sample$cohort[[1]],
//'         state_cost = markov_sample$state_cost[[1]], 
//'         intervention_cost = markov_sample$intervention_cost[[1]], 
//'         qalys = markov_sample$qalys[[1]], 
//'         duration = 100,
//'         discounting = SpeedyMarkov::calc_discounting(1.035, 100),
//'         markov_loop_fn = SpeedyMarkov::markov_loop),
//'         ArmaSimulateMarkov(
//'         sim =  matrix(NA, nrow = 100, ncol = nrow(markov_sample$transition[[1]])),
//'         transition = markov_sample$transition[[1]],
//'         cohort = markov_sample$cohort[[1]],
//'         state_cost = markov_sample$state_cost[[1]], 
//'         intervention_cost = markov_sample$intervention_cost[[1]], 
//'         qalys = markov_sample$qalys[[1]], 
//'         duration = 100,
//'         discounting = SpeedyMarkov::calc_discounting(1.035, 100)),
//'         times = 1000)
// [[Rcpp::export]]
Rcpp::List ArmaSimulateMarkov(arma::mat sim, arma::rowvec cohort,
                              arma::mat transition, int duration, arma::colvec state_cost,
                              arma::vec discounting, arma::colvec qalys, double intervention_cost){
  
  

  //Run Markov loop using previously defined function
  sim = ArmaMarkovLoop(sim, cohort, transition, duration);
  
  //Total costs per cycle (with discounting)
  arma::vec total_costs_cycle(duration);
  total_costs_cycle = (sim * state_cost) % discounting;
  
  //Total QALYs per cycle (with discounting)
  arma::vec discounted_qalys(duration);
  discounted_qalys = (sim * qalys) % discounting;
  double total_qalys = arma::sum(discounted_qalys);
  
  // Overall costs
  double  total_costs = arma::sum(total_costs_cycle) + intervention_cost;
  
  // Retrun total costs and qalys
  return Rcpp::List::create(Rcpp::Named("total_costs") = total_costs,
                            Rcpp::Named("total_qalys") = total_qalys);
}

