#' Arrange Vectorised Matrix Samples
#'
#' @description A convenience function used to arrange vectorised matrix samples into the correct
#' matrix format for several functions used to specify Markov model. See `example_two_state_markov`
#' for an example use case. Both a R and C++ implementation are available.
#' @param type A character string specifying the approach to use. Currently implemented
#' approaches are "base", and "rcpp" with "rcpp" as the default.
#' @export
#' @inherit matrix_arrange_inner
#' @examples
#' 
#' matrix_samples <- list(VGAM::rdiric(1:5, c(88, 12)),
#'                        VGAM::rdiric(1:5, c(8, 92)))
#' 
#' # Default usage
#' matrix_arrange(matrix_samples)
#' # R implementation
#' matrix_arrange(matrix_samples, type = "base")
#' # Rcpp implementation
#' matrix_arrange(matrix_samples, type = "rcpp")
#' 
matrix_arrange <- function(samples, type = "rcpp") {
  
#Send findings to inner functions
  if (type == "base") {
    tmp <- SpeedyMarkov::matrix_arrange_inner(samples)
  }else if (type == "rcpp"){
    tmp <- SpeedyMarkov::MatrixArrange(samples)
  }
  return(tmp)
}



#' Arrange Vectorised Matrix Samples using R
#'
#' @description A convenience function used to arrange vectorised matrix samples into the correct
#' matrix format for several functions used to specify Markov model. See `example_two_state_markov`
#' for an example use case. Implemented using R.
#' @param samples A list of vectorised matrix samples
#'
#' @return A list of matrices with each matrix representing a single sample.
#' @importFrom purrr map
#' @export
#'
#' @examples
#' 
#' matrix_samples <- list(VGAM::rdiric(1:5, c(88, 12)),
#'                        VGAM::rdiric(1:5, c(8, 92)))
#' 
#' matrix_arrange_inner(matrix_samples)
matrix_arrange_inner <- function(samples) {
  
  # Map transitions into matrices
  dim <- length(samples)
  out <- matrix(NA, dim, dim)
  
  tmp <- purrr::map(1:nrow(samples[[1]]), ~  {
    
    for (i in 1:dim) {
      out[i, ] <- samples[[i]][., ]
    }
    return(out)
  })
  
  return(tmp)
}
