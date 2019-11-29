
#include <Rcpp.h>

//' @title Arrange Vectorised Matrix Samples using Rcpp
//' 
//' @description A convenience function used to arrange vectorised matrix samples into the correct
//' matrix format for several functions used to specify Markov model. See `example_two_state_markov`
//' for an example use case. Implemented using Rcpp.
//' @inherit matrix_arrange_inner
//' @export
//' @useDynLib SpeedyMarkov, .registration=TRUE
//' @examples
//' 
//' 
//' matrix_samples <- list(VGAM::rdiric(1:5, c(88, 12)),
//'                       VGAM::rdiric(1:5, c(8, 92)))
//' 
//' # R implementation
//' samples_r <- matrix_arrange_inner(matrix_samples)
//' 
//' 
//' # Rcpp implementation
//' samples_rcpp <- MatrixArrange(matrix_samples)
//' 
//' all.equal(samples_r, samples_rcpp)
//' 
//' # Benchmark
//' library(microbenchmark)
//' microbenchmark(matrix_arrange_inner(matrix_samples), 
//'                MatrixArrange(matrix_samples), 
//'                times = 1000)
// [[Rcpp::export]]
Rcpp::List MatrixArrange(Rcpp::List samples){
  
  // Get the matrix dimensions
  int dim = samples.size();
  // Get a sample matrix
  Rcpp::NumericMatrix sample_matrix = Rcpp::as<Rcpp::NumericMatrix>(samples[0]);
  // Get the number of samples
  int sample_no = sample_matrix.nrow();
  
  // Create storage objects
  Rcpp::List samples_arranged(sample_no);
  Rcpp::NumericMatrix sample_mat(sample_no, dim);
  
    for (int i = 0; i < sample_no; i++){

      //Refresh output matrix
      Rcpp::NumericMatrix out(dim, dim);
      
      for(int j = 0; j < dim; j++){
        //Assign list entry to tmp matrix
        sample_mat = Rcpp::as<Rcpp::NumericMatrix>(samples[j]);
        //Assign to output matrix
        out.row(j) = sample_mat.row(i);
      }
      
      //Assign output matrix to list
      samples_arranged(i) = out;
    }
  
  return samples_arranged;
}


