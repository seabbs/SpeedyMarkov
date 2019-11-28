
# Designed to be run using Rscripts from the package directory
# Packages ----------------------------------------------------------------

library(SpeedyMarkov)
library(microbenchmark)
library(parallel)
library(furrr)

# Two state model ---------------------------------------------------------

# Benchmarks for two state with 100 duration
two_state_bench_dur_100 <- benchmark_markov(markov_model = example_two_state_markov,
                                    reference = reference_two_state_markov,
                                    duration = 100, samples = 100000,
                                    times = 10)

saveRDS(two_state_bench_dur_100, "inst/benchmarks/two_state_duration_100.rds")

# Benchmarks for two state with 200 duration
two_state_bench_dur_200 <- benchmark_markov(markov_model = example_two_state_markov,
                                            reference = reference_two_state_markov,
                                            duration = 200, samples = 100000,
                                            times = 10)

saveRDS(two_state_bench_dur_200, "inst/benchmarks/two_state_duration_1000.rds")