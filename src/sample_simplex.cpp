#include <Rcpp.h>
#include <numeric>
#include "math.h"
#include "model.h"

//#define DEBUG

using namespace Rcpp;

// uniformly sample from a simplex.
// https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
// Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986.

// [[Rcpp::export]]
NumericMatrix sample_simplex_cpp(int n, int d) {
  NumericMatrix sample(n, d);
  
  for (int i = 0; i < n; ++i) {
    NumericVector r = runif(d + 1);
    r[0] = 0; r[d]  = 1;
    r.sort();
    sample(i, _ ) = diff(r);
  }
  
  return sample;
}
