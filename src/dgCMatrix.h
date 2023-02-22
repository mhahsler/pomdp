#ifndef DGCMATRIX_H_
#define DGCMATRIX_H_

// Sources: 
//  * https://gallery.rcpp.org/articles/sparse-matrix-class/
//  * https://codereview.stackexchange.com/questions/259594/rcpp-sparse-csc-matrix-class
//  * https://github.com/zdebruine/RcppSparse

#include <Rcpp.h>

namespace Rcpp {

class dgCMatrix {
public:
  IntegerVector i, p, Dim;
  NumericVector x;
  List Dimnames;
  
  // constructor
  dgCMatrix(S4 mat) {
    i = mat.slot("i");
    p = mat.slot("p");
    x = mat.slot("x");
    Dim = mat.slot("Dim");
    Dimnames = mat.slot("Dimnames");
  };
  
  int nrow() { return Dim[0]; };
  int ncol() { return Dim[1]; };
  int rows() { return Dim[0]; };
  int cols() { return Dim[1]; };
  
  double at(int row, int col) const {
    for (int j = p[col]; j < p[col + 1]; ++j) {
      if (i[j] == row) return x[j];
      else if (i[j] > row) break;
    }
    return 0.0;
  }
  double operator()(int row, int col) { return at(row, col); };
  
  NumericVector col(int col) const {
    NumericVector c(Dim[0], 0.0);
    for (int j = p[col]; j < p[col + 1]; ++j)
      c[i[j]] = x[j];
    return c;
  }
  
  NumericVector row(int row) const {
    NumericVector r(Dim[1], 0.0);
    for (int col = 0; col < Dim[1]; ++col) {
      for (int j = p[col]; j < p[col + 1]; ++j) {
        if (i[j] == row) r[col] = x[j];
        else if (i[j] > row) break;
      }
    }
    return r;
  }

  NumericMatrix dense() const {
    NumericMatrix res(Dim[0], Dim[1]);
    for (int j = 0; j < Dim[0]; ++j) {
      res.row(j) = row(j);
    }
    return res;
  }
    
};

// template <> dgCMatrix as(SEXP mat) { return dgCMatrix(mat); }
// 
// template <> SEXP wrap(const dgCMatrix& sm) {
//   S4 s(std::string("dgCMatrix"));
//   s.slot("i") = sm.i;
//   s.slot("p") = sm.p;
//   s.slot("x") = sm.x;
//   s.slot("Dim") = sm.Dim;
//   s.slot("Dimnames") = sm.Dimnames;
//   return s;
//}

}

#endif