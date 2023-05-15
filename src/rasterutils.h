#ifndef RASTERUTILS
#define RASTERUTILS

#include <Rcpp.h>
#include "rsinfo.h"

extern void recycle(std::vector<int> &x, std::vector<int> &y);
extern Rcpp::IntegerVector cellFromColRowSensitive(Rcpp::S4 &raster, Rcpp::IntegerVector &rcpp_col, Rcpp::IntegerVector &rcpp_row);
extern Rcpp::IntegerVector cellFromColRow(const Rcpp::IntegerVector &x, const Rcpp::IntegerVector &y, const int ncol);
extern Rcpp::IntegerMatrix colRowFromCell(const Rcpp::IntegerVector &cell, const int ncol);
extern std::vector<std::vector<double>> xyFromCell(RasterInfo ras, const std::vector<int> &cell);
extern std::vector<std::vector<double>> xyFromCell(RasterInfo ras, const int cell);
extern std::vector<int> cellFromXY(RasterInfo ras, const std::vector<std::vector<double>> &xy);
extern int cellFromXY2(RasterInfo ras, double x, double y);

#endif