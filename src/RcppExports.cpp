// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// bit_indexr
LogicalMatrix bit_indexr(arma::uword n);
RcppExport SEXP eulerr_bit_indexr(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(bit_indexr(n));
    return rcpp_result_gen;
END_RCPP
}
// return_intersections
arma::vec return_intersections(arma::vec par);
RcppExport SEXP eulerr_return_intersections(SEXP parSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type par(parSEXP);
    rcpp_result_gen = Rcpp::wrap(return_intersections(par));
    return rcpp_result_gen;
END_RCPP
}
// venneuler_stress
double venneuler_stress(arma::vec& areas, arma::vec& fit);
RcppExport SEXP eulerr_venneuler_stress(SEXP areasSEXP, SEXP fitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type areas(areasSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type fit(fitSEXP);
    rcpp_result_gen = Rcpp::wrap(venneuler_stress(areas, fit));
    return rcpp_result_gen;
END_RCPP
}
// loss_final
double loss_final(arma::vec par, arma::vec areas);
RcppExport SEXP eulerr_loss_final(SEXP parSEXP, SEXP areasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type par(parSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type areas(areasSEXP);
    rcpp_result_gen = Rcpp::wrap(loss_final(par, areas));
    return rcpp_result_gen;
END_RCPP
}
// choose_two
IntegerMatrix choose_two(IntegerVector x);
RcppExport SEXP eulerr_choose_two(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(choose_two(x));
    return rcpp_result_gen;
END_RCPP
}
// discdisc
NumericVector discdisc(NumericVector r1, NumericVector r2, NumericVector d);
RcppExport SEXP eulerr_discdisc(SEXP r1SEXP, SEXP r2SEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(discdisc(r1, r2, d));
    return rcpp_result_gen;
END_RCPP
}
// find_surrounding_sets
LogicalMatrix find_surrounding_sets(NumericVector xs, NumericVector ys, NumericVector x, NumericVector y, NumericVector r);
RcppExport SEXP eulerr_find_surrounding_sets(SEXP xsSEXP, SEXP ysSEXP, SEXP xSEXP, SEXP ySEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ys(ysSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(find_surrounding_sets(xs, ys, x, y, r));
    return rcpp_result_gen;
END_RCPP
}
// max_colmins
arma::uword max_colmins(arma::mat x);
RcppExport SEXP eulerr_max_colmins(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(max_colmins(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"eulerr_bit_indexr", (DL_FUNC) &eulerr_bit_indexr, 1},
    {"eulerr_return_intersections", (DL_FUNC) &eulerr_return_intersections, 1},
    {"eulerr_venneuler_stress", (DL_FUNC) &eulerr_venneuler_stress, 2},
    {"eulerr_loss_final", (DL_FUNC) &eulerr_loss_final, 2},
    {"eulerr_choose_two", (DL_FUNC) &eulerr_choose_two, 1},
    {"eulerr_discdisc", (DL_FUNC) &eulerr_discdisc, 3},
    {"eulerr_find_surrounding_sets", (DL_FUNC) &eulerr_find_surrounding_sets, 5},
    {"eulerr_max_colmins", (DL_FUNC) &eulerr_max_colmins, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_eulerr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
