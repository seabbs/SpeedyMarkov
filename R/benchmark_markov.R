# Set up grid to be used across models
# Not in Rstudio future currently defaults to multisession rather than multicore which may be slower. 
#' Benchmarks Markov Model Simulation Approaches
#'
#' @description This function runs a benchmark across multiple approaches to Markov model
#' simulation. This includes multiple approaches using `Rcpp` and different strategies for parallisation.
#' Unless otherwise stated the default settings for `SpeedyMarkov` are used (which are to maximise the use 
#' of `Rcpp`). **Note: `mclapply` uses mutlicore parallisation which is only available on Mac and Linux. `future`
#' and `furrr` default to multicore parallisation but this will again not be used on Windows or in an interactive 
#' Rstudio session. This may impact benchmark performance.**). 
#' 
#' This function relies on suggested packages and so requires the installation of the following packages: 
#'   * microbenchmark
#'   * furrr
#' @param reference Function, the reference model to compare the `SpeedyMarkov` approaches to. An example
#'  of a reference model is `reference_two_state_markov`.
#' @param times Numeric, the number of times to repeat the benchmark. Defaults to once. Repeating the benchmark
#' can help control for stochastic effects. 
#'
#' @inheritParams markov_ce_pipeline
#' @return Returns a `microbenchmark` data.frame of benchmarking results
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # Run a benchmark across approaches for a small number of samples 
#' results <- benchmark_markov(markov_model = example_two_state_markov,
#'                             reference = reference_two_state_markov,
#'                             duration = 100, samples = 1000,
#'                             times = 10)
#'                                 
#'results
#'
#'
#'ggplot2::autoplot(results)
#'} 
benchmark_markov <- function(markov_model = NULL, reference = NULL, 
                             duration = NULL, samples = NULL,
                             times = 1) {
  ## Start up parallel sessions for future
  future::plan(future::multiprocess, workers = 4);
  
  benchmark <- microbenchmark::microbenchmark(
    "Reference" = {
      reference(cycles = duration, samples = samples)
    },
    "R" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "base",
                         sample_type = "base")
    },
    "Rcpp inner loop" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "armadillo_inner",
                         sample_type = "base")
    },
    "Rcpp simulation" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "armadillo_all",
                         sample_type = "base")
    },
    "Rcpp simulation + sampling" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "armadillo_all",
                         sample_type = "rcpp")
    },
    "Rcpp simulation + sampling - mclapply (2 cores)" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "armadillo_all",
                         sample_type = "rcpp",
                         batches = 2,
                         batch_fn = parallel::mclapply,
                         mc.cores = 2)
    },
    "Rcpp simulation + sampling - mclapply (4 cores)" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "armadillo_all",
                         sample_type = "rcpp",
                         batches = 4,
                         batch_fn = parallel::mclapply,
                         mc.cores = 4
      )
    },
    "Rcpp simulation - furrr::future_map (4 cores)" = {
      markov_ce_pipeline(markov_model(),
                         duration = duration, 
                         samples = samples,
                         sim_type = "armadillo_all",
                         sample_type = "rcpp",
                         batches = 4,
                         batch_fn = furrr::future_map)
    },
    times = times)
  
  return(benchmark)
}