#' Arrange Vectorised Matrix Samples
#'
#' @description A convenience function used to arrange vectorised matrix samples into the correct
#' matrix format for several functions used to specify Markov model. See `example_two_state_markov`
#' for an example use case.
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
#' matrix_arrange(matrix_samples)
matrix_arrange <- function(samples) {
  
  # Map transitions into matrices
  dim <- length(samples)
  out <- matrix(NA, dim, dim)
  
  tmp <- purrr::map(1:nrow(samples[[1]]),~  {
    
    for (i in 1:dim) {
      out[i, ] <- samples[[i]][., ]
    }
    return(out)
  })
  
  return(tmp)
}
