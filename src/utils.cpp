#define ARMA_NO_DEBUG // For the final version

#include "helpers.h"

// [[Rcpp::export]]
arma::umat choose_two(const arma::uvec& x) {
  arma::uword n = x.n_elem;
  arma::umat m(n*(n - 1)/2, 2);
  for (arma::uword i = 0, k = 0; i < n - 1; ++i) {
    for (arma::uword j = i + 1; j < n; ++j, ++k) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }
  return m;
}

// Squared loss between given and desired overlap
// [[Rcpp::export]]
double discdisc(double d, double r1, double r2, double overlap) {
  double r1sq = std::pow(r1, 2);
  double r2sq = std::pow(r2, 2);
  double dsq  = std::pow(d, 2);

  double D = r1sq*std::acos((dsq + r1sq - r2sq)/(2*d*r1)) +
    r2sq*std::acos((dsq + r2sq - r1sq)/(2*d*r2)) -
    0.5*std::sqrt((r1 + r2 - d)*(d + r1 - r2)*(d - r1 + r2)*(d + r1 + r2));

  return std::pow(D - overlap, 2);
}

// [[Rcpp::export]]
double stress(const arma::vec& areas, const arma::vec& fit) {
  double sst   = arma::accu(arma::square(fit));
  double slope = arma::accu(areas%fit)/arma::accu(arma::square(areas));
  double sse   = arma::accu(arma::square(fit - areas*slope));
  return sse/sst;
}

// [[Rcpp::export]]
arma::umat bit_index_cpp(arma::uword n) {
  return bit_index(n);
}
