// This code has been modified from the Nelder-Mead algorithm written by
// C. T. Kelley, which is avilable at http://www4.ncsu.edu/~ctk/darts/nelder.m

#ifndef eulerr_neldermead_h_
#define eulerr_neldermead_h_

#include <RcppArmadillo.h>

template <class Fun, class...Pars>
arma::vec nelderMead(arma::vec&& x0,
                     Fun&& f,
                     Pars&&... args) {
  arma::uword maximize = 1;
  arma::uword maxit = 2000;
  double tol = 1e-8;
  double minmax = maximize ? -1.0 : 1.0;

  double rho = 1;
  double chi = 2;
  double gamma = 0.5;
  double sigma = 0.5;

  arma::uword n = x0.n_rows;
  arma::uword m = n + 1;

  arma::mat x(n, n, arma::fill::eye);
  x.insert_cols(0, x0);

  arma::vec hj(n);

  // Set the minimum stepsize to half the distance to the nearest ellipse
  hj.fill(f(x0, args...)/2);

  for (arma::uword i = 1; i < m; ++i) {
    x.col(i) = x0 + hj%x.col(i);
  }

  arma::uword oshrink = 1;
  arma::uword restartmax = 3;
  arma::uword restarts = 0;

  arma::uword orth = 0;

  arma::vec fv(m);

  for (arma::uword j = 0; j < m; ++j) {
    fv(j) = minmax*f(x.col(j), args...);
  }
  arma::uvec is = arma::sort_index(fv);
  fv = fv(is);
  x = x.cols(is);

  double dist = fv(n) - fv(0);

  arma::vec diam = arma::zeros<arma::vec>(n);

  arma::mat v(n, n);
  arma::vec delf(n);
  for (arma::uword j = 1; j < m; ++j) {
    v.col(j - 1) = -x.col(0) + x.col(j);
    delf(j - 1) = fv(j) - fv(0);
    diam(j - 1) = arma::norm(v.col(j - 1));
  }

  arma::vec sgrad(3);
  bool solved = arma::solve(sgrad, v.t(), delf, arma::solve_opts::no_approx);

  if (!solved) {
    x.col(0).fill(arma::datum::nan);
    return x.col(0);
  }

  double alpha = 1e-4*arma::max(diam)/arma::norm(sgrad);

  // Main Nelder-Mead loop
  arma::uword itc = 0;

  while(itc < maxit && dist > tol && restarts < restartmax) {
    double fbc = arma::accu(fv)/(n + 1);
    arma::vec xbc = arma::sum(x.t()).t()/(n + 1);

    arma::vec sgrad(3);
    bool solved = arma::solve(sgrad, v.t(), delf, arma::solve_opts::no_approx);

    if (!solved) {
      x.col(0).fill(arma::datum::nan);
      return x.col(0);
    }

    arma::uword happy = 0;

    // reflect
    arma::mat y = x.head_cols(n);
    arma::vec xbart = arma::sum(y, 1)/n; //centroid of better vertices
    arma::vec xbar = xbart;
    arma::vec xr = (1 + rho)*xbar - rho*x.col(n);
    double fr = minmax*f(xr, args...);
    double fn;
    arma::vec xn;
    if (fr >= fv(0) && fr < fv(n - 1)) {
      happy = 1;
      xn = xr;
      fn = fr;
    }

    // expand
    if (!happy && fr < fv(0)) {
      arma::vec xe = (1 + rho*chi)*xbar - rho*chi*x.col(n);
      double fe = minmax*f(xe, args...);
      if (fe < fr) {
        xn = xe;
        fn = fe;
        happy = 1;
      } else {
        xn = xr;
        fn = fr;
        happy = 1;
      }
    }

    // contract
    arma::vec xc;
    double fc;
    if (!happy && fr >= fv(n - 1) && fr < fv(n)) {
      // outside contraction
      arma::vec xc = (1 + rho*gamma)*xbar - rho*gamma*x.col(n);
      fc = minmax*f(xc, args...);
      if (fc <= fr) {
        xn = xc;
        fn = fc;
        happy = 1;
      }
    }
    if (!happy && fr >= fv(n)) {
      // inside contraction
      xc = (1 - gamma)*xbar + gamma*x.col(n);
      fc = minmax*f(xc, args...);
      if (fc < fv(n)) {
        xn = xc;
        fn = fc;
        happy = 1;
      }
    }

    // test for sufficient decrease
    // do an oriented shrink if necessary
    if (happy && oshrink) {
      arma::mat xt = x;
      xt.col(n) = xn;
      arma::vec ft = fv;
      ft(n) = fn;
      double fbt = arma::accu(ft)/(n + 1);
      double delfb = fbt - fbc;
      double armtst = alpha*std::pow(arma::norm(sgrad), 2);
      if (delfb > -armtst/n) {
        restarts++;
        orth = 1;
        double diams = arma::min(diam);
        arma::vec sx = arma::sign(0.5*arma::sign(sgrad));

        happy = 0;

        for (arma::uword i = 1; i < m; ++i) {
          x(i - 1, i) -= diams*sx(i - 1);
        }
      }
    }

    // if we have accepted a new point, nuke the old point and resort
    if (happy) {
      x.col(n) = xn;
      fv(n) = fn;
      arma::uvec is = arma::sort_index(fv);
      fv = fv(is);
      x = x.cols(is);
    }

    // You're in trouble now! Shrink or restart.
    if (restarts >= restartmax)

    if (!happy && restarts < restartmax) {
      if (orth) {
        orth = 0;
      }
      for (arma::uword j = 1; j < m; ++j) {
        x.col(j) = x.col(0) + sigma*(x.col(j) - x.col(0));
        fv(j) = minmax*f(x.col(j), args...);
      }

      arma::uvec is = arma::sort_index(fv);
      fv = fv(is);
      x = x.cols(is);
    }
    // compute the diameter of the new simplex and the iteration data
    for (arma::uword j = 1; j < m; ++j) {
      v.col(j - 1) = -x.col(0) + x.col(j);
      delf(j - 1) = fv(j) - fv(0);
      diam(j - 1) = arma::norm(v.col(j - 1));
    }

    dist = fv(n) - fv(0);
    solved = arma::solve(sgrad, v.t(), delf, arma::solve_opts::no_approx);
    if (!solved) {
      x.col(0).fill(arma::datum::nan);
      return x.col(0);
    }
    itc++;
  }

  return x.col(0);
}

# endif
