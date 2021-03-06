% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{ArmaSimulateMarkov}
\alias{ArmaSimulateMarkov}
\title{Simulate a Markov Model Sample using RcppArmadillo}
\usage{
ArmaSimulateMarkov(
  sim,
  cohort,
  transition,
  duration,
  state_cost,
  discounting,
  qalys,
  intervention_cost
)
}
\arguments{
\item{sim}{Matrix with the same number of rows as the duration of the model and the same number of columns as the number of
states in the model. Used to store model simulations.}

\item{cohort}{A vector containing the initial state.}

\item{transition}{A transition matrix, see \code{example_two_state_markov} for an example of setting this up.}

\item{duration}{Numeric, how many long to run the model for.}

\item{state_cost}{A list of state costs for each intervention,
see \code{example_two_state_markov} for an example of setting this up.}

\item{discounting}{Numeric vector, the discount that should be applied to the costs and QALYs for each time period.
This must be the same length as \code{duration}.}

\item{qalys}{A list of QALYs for each intervention, see \code{example_two_state_markov} for an example of setting this up.}

\item{intervention_cost}{A vector of intervention costs, see \code{example_two_state_markov} for an example of setting this up.}
}
\value{
A list containing total costs and total QALYs as matrices across states
and the duration of the model
}
\description{
This model agnostic function runs a single markov model for the specified duration using a Armadillo implementation.
See \code{example_two_state_markov} for an example of the required input. Alternatively use \code{sample_markov(type = "base")}
and the output from \code{sample_markov}.
}
\examples{

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
all.equal(sim_r, sim_rcppArma)

# Benchmark
library(microbenchmark)
microbenchmark(simulate_markov_base(
        sim =  matrix(NA, nrow = 100, ncol = nrow(markov_sample$transition[[1]])),
        transition = markov_sample$transition[[1]],
        cohort = markov_sample$cohort[[1]],
        state_cost = markov_sample$state_cost[[1]], 
        intervention_cost = markov_sample$intervention_cost[[1]], 
        qalys = markov_sample$qalys[[1]], 
        duration = 100,
        discounting = SpeedyMarkov::calc_discounting(1.035, 100),
        markov_loop_fn = SpeedyMarkov::markov_loop),
        ArmaSimulateMarkov(
        sim =  matrix(NA, nrow = 100, ncol = nrow(markov_sample$transition[[1]])),
        transition = markov_sample$transition[[1]],
        cohort = markov_sample$cohort[[1]],
        state_cost = markov_sample$state_cost[[1]], 
        intervention_cost = markov_sample$intervention_cost[[1]], 
        qalys = markov_sample$qalys[[1]], 
        duration = 100,
        discounting = SpeedyMarkov::calc_discounting(1.035, 100)),
        times = 1000)
}
