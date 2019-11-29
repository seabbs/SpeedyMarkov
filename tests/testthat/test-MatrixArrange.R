context("MatrixArrange")

test_that("Check that the Rcpp MatrixArrange produces the same input
          as the base R matrix_arrange_inner.", {
matrix_samples <- list(VGAM::rdiric(1:5, c(88, 12)),
                       VGAM::rdiric(1:5, c(8, 92)))
 
 # R implementation
 samples_r <- matrix_arrange_inner(matrix_samples)
 
 
 # Rcpp implementation
 samples_rcpp <- MatrixArrange(matrix_samples)
 
 expect_equal(samples_r, samples_rcpp)
})
