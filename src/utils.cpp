#define ARMA_NO_DEBUG // For the final version

#include "helpers.h"

using namespace arma;

// [[Rcpp::export]]
arma::umat
choose_two(const arma::uvec& x) {
  uword n = x.n_elem;
  umat m(n*(n - 1)/2, 2);
  for (uword i = 0, k = 0; i < n - 1; ++i) {
    for (uword j = i + 1; j < n; ++j, ++k) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }
  return m;
}

// Squared loss between given and desired overlap
// [[Rcpp::export]]
double
discdisc(double d,
         double r1,
         double r2,
         double overlap) {
  double r1sq = r1*r1;
  double r2sq = r2*r2;
  double dsq  = d*d;

  double D = r1sq*std::acos((dsq + r1sq - r2sq)/(2*d*r1)) +
    r2sq*std::acos((dsq + r2sq - r1sq)/(2*d*r2)) -
    0.5*std::sqrt((r1 + r2 - d)*(d + r1 - r2)*(d - r1 + r2)*(d + r1 + r2));

  return std::pow(D - overlap, 2);
}

// [[Rcpp::export]]
arma::umat
bit_index_cpp(arma::uword n) {
  return bit_index(n);
}
