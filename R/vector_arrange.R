
#' Arrange Vectorised Vector Samples
#'
#' @description A convenience function used to arrange vectorised samples into the correct
#' vector format for several functions used to specify Markov model. See `example_two_state_markov`
#' for an example use case.
#' @param samples A list of vectorised samples
#'
#' @return A list of vectors with each vector representing a single sample.
#' @importFrom purrr map
#' @export
#'
#' @examples
#' 
#' vector_samples <- list(rnorm(10, 1, 2), rnorm(10, 10, 2))
#' 
#' vector_arrange(vector_samples)
vector_arrange <- function(samples) {
  ## Sample
  tmp <- do.call(cbind, samples)
  
  ## Split into vectors and name
  out <- split(tmp, 1:nrow(tmp))
  names(out) <- NULL
  
  return(out)
}
