#' Calculate Discounting Over the Model Run Time.
#'
#' @param discount Numeric, the discount that should be applied to the costs and QALYs. Defaults to 1.035.
#'
#' @inheritParams markov_loop
#' @return A vector of length `duration`.
#' @export
#'
#' @examples
#' 
#' calc_discounting(1.035, 10)
calc_discounting <- function(discount = 1.035, duration = NULL) {
  discounting <-  (1 / discount)^(0:(duration - 1))  
  
  return(discounting)
}