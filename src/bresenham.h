#ifndef BRESENHAM
#define BRESENHAM

#include <Rcpp.h>

extern int Sign2(const int dxy);
extern Rcpp::IntegerMatrix bresenham_map(const int x0, const int y0, const int radius, const int nc);
extern Rcpp::NumericVector tangentsMap(const int x0, const int y0, const double h0, const int nr, const int nc, const int r, const Rcpp::NumericVector &dsm_vec);
extern void LoS(const Rcpp::IntegerMatrix &los_ref_mat, const Rcpp::NumericVector &tan_vec, Rcpp::LogicalVector &visibility_vec, const int &j, const int &r);
extern Rcpp::IntegerVector LoS_reference(const int x0_ref, const int y0_ref, const int r, const int nc_ref);
extern Rcpp::IntegerVector shared_LoS(const int r, const Rcpp::IntegerVector &los_ref_vec);

#endif