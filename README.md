
# Speed up Discrete Markov Model Cost Effectiveness Simulations <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![badge](https://img.shields.io/badge/Launch-SpeedyMarkov-blue.svg)](https://mybinder.org/v2/gh/seabbs/SpeedyMarkov/master?urlpath=rstudio)
[![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/SpeedyMarkov)](https://CRAN.R-project.org/package=SpeedyMarkov)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg?style=flat)](https://github.com/seabbs/SpeedyMarkov)
[![DOI](https://zenodo.org/badge/219757940.svg)](https://zenodo.org/badge/latestdoi/219757940)

**Work in progress**

This package aims to:

  - Compare a functional markov modelling approach to a reference
    approach for several example models.
  - Explore approaches to speeding up Markov modelling in a principled
    fashion making use of C++ when required.
  - Detail the benefits of parallisation and provide a code structure in
    which parallisation is easy to make use of.
  - Provide a toolkit for use in discrete Markov modelling.
  - Provide optimised code that may be ported into other applications
    and workflows.

The work in this package was started at the Health Economic 2019
hackathon hosted at Imperial. Much of this work is based on that
developed by the
[hermes6](https://github.com/HealthEconomicsHackathon/hermes6) team. The
original reference approach was developed by [Howard
Thom](https://orcid.org/0000-0001-8576-5552).

## Installation

Install the CRAN version (when released):

``` r
install.packages("SpeedyMarkov")
```

Alternatively install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("seabbs/SpeedyMarkov")
```

## Documentation

[![Documentation](https://img.shields.io/badge/Documentation-release-lightgrey.svg?style=flat)](https://www.samabbott.co.uk/SpeedyMarkov/)
[![Development
documentation](https://img.shields.io/badge/Documentation-development-lightblue.svg?style=flat)](https://www.samabbott.co.uk/SpeedyMarkov/dev)
[![Getting
started](https://img.shields.io/badge/Documentation-getting%20started-yellow.svg?style=flat)](https://www.samabbott.co.uk/SpeedyMarkov/articles/intro.html)
[![Functions](https://img.shields.io/badge/Documentation-functions-orange.svg?style=flat)](https://www.samabbott.co.uk/SpeedyMarkov/reference/index.html)

## Testing

[![Travis-CI Build
Status](https://travis-ci.org/seabbs/ceplotr.svg?branch=master)](https://travis-ci.org/seabbs/SpeedyMarkov)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/seabbs/SpeedyMarkov?branch=master&svg=true)](https://ci.appveyor.com/project/seabbs/SpeedyMarkov)
[![Coverage
Status](https://img.shields.io/codecov/c/github/seabbs/SpeedyMarkov/master.svg)](https://codecov.io/github/seabbs/SpeedyMarkov?branch=master)

## Quick start

The first step is to specify a Markov model in the format specificed by
`SpeedyMarkov`. An example framework is `example_two_state_markov` which
is a two state Markov model that compares an intervention to a baseline.
See `?example_two_state_markov` for more details.

``` r
SpeedyMarkov::example_two_state_markov()
#> $transitions_list
#> $transitions_list$SoC
#> function(samples = NULL) {
#>     # Sample transitions
#>     tmp <- list(VGAM::rdiric(samples, c(88, 12)),
#>                  VGAM::rdiric(samples, c(8, 92)))
#>     
#>     # Arrange as matrices
#>     tmp <- SpeedyMarkov::matrix_arrange(tmp)
#>     
#>     return(tmp)
#>   }
#> <bytecode: 0x56471c06dac0>
#> <environment: 0x56471c0ba438>
#> 
#> $transitions_list$`Soc with Website`
#> function(baseline = NULL) {
#>     
#>     #Sample transitions for each baseline matrix
#>     samples <- length(baseline)
#>     tmp <- VGAM::rdiric(samples,c(85,15))
#>     
#>     # Update baseline
#>     updated <- purrr::map(1:samples, function(sample) {
#>       update <- baseline[[sample]]
#>       update[1, ] <- tmp[sample, ]
#>       return(update)
#>       })
#>     
#>     return(updated)
#>   }
#> <bytecode: 0x56471c075ac0>
#> <environment: 0x56471c0ba438>
#> 
#> 
#> $qalys
#> function(samples = NULL) {
#>     qaly <- function(samples = 1) {
#>       ## Sample
#>       tmp <- list(stats::rnorm(samples, mean = 0.95,sd = 0.01) / 2,
#>                    rep(1 / 2, samples))
#>       
#>       out <- SpeedyMarkov::vector_arrange(tmp)
#>   
#>       return(out)
#>     }
#>     
#>     soc <- qaly(samples = samples)
#>     soc_with_website <- soc
#>     
#>     out <- list(soc, soc_with_website)
#>     names(out) <- list("SoC", "Soc with Website")
#>     
#>     out <- purrr::transpose(out)
#>     
#>     return(out)
#>   }
#> <bytecode: 0x56471c0806a0>
#> <environment: 0x56471c0ba438>
#> 
#> $intervention_costs
#> function(samples = NULL) {
#>     ## Sample
#>     tmp <- list(rep(0, samples),
#>                  rep(50, samples))
#>     
#>     out <- SpeedyMarkov::vector_arrange(tmp)
#> 
#>     return(out)
#>   }
#> <bytecode: 0x56471c093d98>
#> <environment: 0x56471c0ba438>
#> 
#> $state_costs
#> function(samples = NULL) {
#>     state_cost <- function(samples = 1) {
#>       tmp <- list(rep(0, samples),
#>                  rep(0, samples))
#>       
#>       out <- SpeedyMarkov::vector_arrange(tmp)
#>       
#>       return(out)
#>       
#>     }
#>     
#>     soc <- state_cost(samples = samples)
#>     soc_with_website <- soc
#>     
#>     out <- list(soc, soc_with_website)
#>     names(out) <- list("SoC", "Soc with Website")
#>     
#>     out <- purrr::transpose(out)
#>     
#>     return(out)
#>   }
#> <bytecode: 0x56471c0978c8>
#> <environment: 0x56471c0ba438>
#> 
#> $cohorts
#> function(samples = NULL) {
#>     
#>     cohort <- function(samples = 1) {
#>       tmp <- list(rep(1, samples),
#>                   rep(0, samples))
#>       
#>       out <- SpeedyMarkov::vector_arrange(tmp)
#> 
#>       return(out)
#>     }
#>     
#>     soc <- cohort(samples = samples)
#>     soc_with_website <- soc
#>     
#>     out <- list(soc, soc_with_website)
#>     names(out) <- list("SoC", "Soc with Website")
#>     
#>     out <- purrr::transpose(out)
#>     
#>     return(out)
#>   }
#> <bytecode: 0x56471c0ab190>
#> <environment: 0x56471c0ba438>
#> 
#> attr(,"class")
#> [1] "SpeedyMarkov" "list"
```

Once a model has been specified a cost effectiveness analysis can run
using the following function
call.

``` r
SpeedyMarkov::markov_ce_pipeline(SpeedyMarkov::example_two_state_markov(), 
                                 duration = 100, samples = 10, discount = 1.035, 
                                 baseline = 1, willingness_to_pay_thresold = 20000)
#> $simulations_with_ce
#> # A tibble: 20 x 12
#>    sample intervention transition state_cost intervention_co… cohort qalys
#>     <int> <chr>        <list>     <list>                <dbl> <list> <lis>
#>  1      1 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#>  2      1 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#>  3      2 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#>  4      2 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#>  5      3 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#>  6      3 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#>  7      4 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#>  8      4 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#>  9      5 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#> 10      5 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#> 11      6 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#> 12      6 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#> 13      7 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#> 14      7 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#> 15      8 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#> 16      8 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#> 17      9 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#> 18      9 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#> 19     10 SoC          <dbl[,2] … <dbl [2]>                 0 <dbl … <dbl…
#> 20     10 Soc with We… <dbl[,2] … <dbl [2]>                50 <dbl … <dbl…
#> # … with 5 more variables: total_costs <dbl>, total_qalys <dbl>,
#> #   incremental_costs <dbl>, incremental_qalys <dbl>,
#> #   incremental_net_benefit <dbl>
#> 
#> $summarised_ce
#> # A tibble: 2 x 13
#>   intervention mean_costs sd_costs mean_qalys sd_qlays mean_incrementa…
#>   <chr>             <dbl>    <dbl>      <dbl>    <dbl>            <dbl>
#> 1 SoC                   0        0       13.9   0.112            0     
#> 2 Soc with We…         50        0       14.0   0.0982           0.0575
#> # … with 7 more variables: sd_incremental_qlays <dbl>,
#> #   mean_incremental_costs <dbl>, sd_incremental_costs <dbl>,
#> #   mean_incremental_net_benefit <dbl>, sd_incremental_net_benefit <dbl>,
#> #   probability_cost_effective <dbl>, icer <dbl>
```

See
[Functions](https://www.samabbott.co.uk/SpeedyMarkov/reference/index.html)
for more details of the functions included in the package
(`markov_ce_pipeline` for example wraps multiple modular - user
customisable - functions). The package vignettes also provide more
detail.

## Contributing

File an issue [here](https://github.com/seabbs/SpeedyMarkov/issues) if
there is a feature that you think is missing from the package, or better
yet submit a pull request.

Please note that the `SpeedyMarkov` project is released with a
[Contributor Code of
Conduct](https://github.com/seabbs/SpeedyMarkov/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citing

If using `SpeedyMarkov` please consider citing the package in the
relevant work. Citation information can be generated in R using the
following (after installing the package),

``` r
citation("SpeedyMarkov")
#> 
#> To cite SpeedyMarkov in publications use:
#> 
#>   Sam Abbott (2019). SpeedyMarkov - -
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {SpeedyMarkov},
#>     author = {Sam Abbott and Howard Thom},
#>     journal = {-},
#>     year = {2019},
#>   }
```

## Docker

This package has been developed in docker based on the
`rocker/tidyverse` image, to access the development environment enter
the following at the command line (with an active docker daemon
running),

``` bash
docker pull seabbs/SpeedyMarkov
docker run -d -p 8787:8787 -e USER=SpeedyMarkov -e PASSWORD=SpeedyMarkov --name SpeedyMarkov seabbs/speedymarkov
```

The rstudio client can be accessed on port `8787` at `localhost` (or
your machines ip). The default username is ceplotr and the default
password is SpeedyMarkov. Alternatively, access the development
environment via
[binder](https://mybinder.org/v2/gh/seabbs/SpeedyMarkov/master?urlpath=rstudio).
