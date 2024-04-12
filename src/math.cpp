#include <Rcpp.h>
#include <numeric>

#include "math.h"

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector round_stochastic_cpp(const NumericVector& x, int digits) {
  NumericVector xr = round(x, digits);
  double s = sum(xr);
  if (s != 1.0)
    xr[which_max(xr)] += 1 - s;
  
  return xr;
}

// [[Rcpp::export]]
NumericVector veccrossprod(const NumericMatrix& A, const NumericVector& b) {
  if (A.nrow() != b.length())
    stop("matrix and vector do not conform for veccrossprod.");
  
  NumericVector out(A.ncol());
  for (R_xlen_t i = 0; i < A.ncol(); ++i) {
    NumericMatrix::ConstColumn A_col = A( _ , i);
    out[i] = std::inner_product(A_col.begin(), A_col.end(), b.begin(), 0.);              
  }
  
  return out;
}

// [[Rcpp::export]]
NumericVector vecprod(const NumericMatrix& A, const NumericVector& b) {
  if (A.ncol() != b.length())
    stop("matrix and vector do not conform for vecprod.");
  
  NumericVector out(A.nrow());
  for (R_xlen_t i = 0; i < A.nrow(); ++i) {
    NumericMatrix::ConstRow A_row = A(i, _ );
    out[i] = std::inner_product(A_row.begin(), A_row.end(), b.begin(), 0.);              
  }
  
  return out;
}

// internal helper
bool contains(IntegerVector X, int z) { 
  return std::find(X.begin(), X.end(), z) != X.end(); 
}