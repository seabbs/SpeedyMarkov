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
SEXP ArmaMarkovLoop(arma::mat sim, arma::rowvec cohort, arma::mat transition, int duration){
  
  sim.row(0) = cohort;
  
  for(int i = 1; i < duration; i++){
    sim.row(i) = sim.row(i-1) * transition;
  }
  
  return Rcpp::wrap(sim);
}
