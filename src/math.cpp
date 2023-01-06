#include <Rcpp.h>
#include <numeric>

#include "math.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector veccrossprod(const NumericMatrix& A, const NumericVector& b) {
  if (A.nrow() != b.length())
    stop("matrix and vector do not conform for veccrossprod.");
  
  NumericVector out(A.ncol());
  //NumericVector A_col;
  for (int i = 0; i < A.ncol(); ++i) {
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
  // NumericVector A_row;
  for (int i = 0; i < A.nrow(); ++i) {
    NumericMatrix::ConstRow A_row = A(i, _ );
    out[i] = std::inner_product(A_row.begin(), A_row.end(), b.begin(), 0.);              
  }
  
  return out;
}