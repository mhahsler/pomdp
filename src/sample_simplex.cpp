#include <Rcpp.h>
#include <numeric>
#include "math.h"
#include "model.h"

//#define DEBUG

using namespace Rcpp;

// uniformly sample from a simplex.
// https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
// Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986.
/***R
(samp <- pomdp:::sample_simplex_cpp(10, c("a", "b", "c"), c(NA, NA, NA)))
rowSums(samp)
(samp <- pomdp:::sample_simplex_cpp(10, c("a", "b", "c", "d"), c(NA, .3, .2, NA)))
rowSums(samp)
(samp <- pomdp:::sample_simplex_cpp(10, c("a", "b", "c", "d"), c(.2, .3, .2, NA)))
rowSums(samp)
*/

// Rcpp does not have which
Rcpp::IntegerVector which(Rcpp::LogicalVector x) {
  Rcpp::IntegerVector v = Rcpp::seq(0, x.size()-1);
  return v[x];
}


// [[Rcpp::export]]
NumericMatrix sample_simplex_cpp(int n, CharacterVector states, NumericVector projection) {
  if (states.size() != projection.size())
    stop("number of states and projection vector do not aggree!");
  
  IntegerVector sample_states = which(is_na(projection));
  int d = sample_states.size();
  double projection_sum = 1 - sum(na_omit(projection));
  
  if (projection_sum > 1)
    stop("projection vector does not sum up to <= 1!");
  
  NumericMatrix sample(n, states.size());
  
  // do the non-projected version faster
  if (d == states.size()) {
    for (int i = 0; i < n; ++i) {
      NumericVector r = runif(d + 1);
      r[0] = 0; r[d]  = 1;
      r.sort();
      sample(i, _ ) = diff(r);
    }
  }else{
    
    // set fixed dimensions
    for (int j = 0; j < states.size(); ++j) {
      if(!is_na(projection)[j]) 
        sample( _ , j) = NumericVector(n, projection(j));
    }
    
    // sample rest
    for (int i = 0; i < n; ++i) {
      NumericVector r = runif(d + 1);
      r[0] = 0; r[d]  = 1;
      r.sort();
      r = diff(r) * projection_sum;
      for (int j = 0; j < r.size(); ++j)
        sample(i, sample_states[j]) = r[j];
    }
  }
  
  colnames(sample) = states;
  return sample;
}
