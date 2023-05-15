#include <Rcpp.h>
#include "rsinfo.h"
#include "rasterutils.h"

using namespace Rcpp;

void recycle(std::vector<int> &x, std::vector<int> &y) {
  size_t xsize = x.size();
  size_t ysize = y.size();
  if (xsize != ysize) {
    size_t n = std::max(xsize, ysize);
    if (xsize > ysize) {
      y.resize(n);
      for (size_t i=ysize; i<n; i++) {
        y[i] = y[i % ysize];
      } 				
    } else {
      x.resize(n);
      for (size_t i=xsize; i<n; i++) {
        x[i] = x[i % xsize];
      } 				
    }
  }
}

Rcpp::IntegerVector cellFromColRowSensitive(Rcpp::S4 &raster,
                                            Rcpp::IntegerVector &rcpp_col,
                                            Rcpp::IntegerVector &rcpp_row) {
  std::vector<int> col = Rcpp::as<std::vector<int> >(rcpp_col);
  std::vector<int> row = Rcpp::as<std::vector<int> >(rcpp_row);
  
  RasterInfo ras(raster);
  int nr = ras.nrow;
  int nc = ras.ncol;
  
  recycle(row, col);
  size_t n = row.size();
  Rcpp::IntegerVector result(n);
  
  for (size_t i=0; i<n; i++) {
    result[i] = (row[i]<0 || row[i] >= nr || col[i]<0 || col[i] >= nc) ? NA_INTEGER : row[i] * nc + col[i];
  }
  return result;
}

Rcpp::IntegerVector cellFromColRow(const Rcpp::IntegerVector &x, 
                                   const Rcpp::IntegerVector &y, const int ncol){
  Rcpp::IntegerVector out(x.size());
  for(int i = 0; i < x.size(); i++){
    if ( Rcpp::IntegerVector::is_na(y[i]) || Rcpp::IntegerVector::is_na(x[i]) ) {
      out[i] = NA_INTEGER;
    } else {
      out[i] = y[i]*ncol + x[i];
    }
  }
  return out;
}

Rcpp::IntegerMatrix colRowFromCell(const Rcpp::IntegerVector &cell, const int ncol) {
  size_t cs = cell.size();
  Rcpp::IntegerMatrix result(cs,2);
  
  for (size_t i = 0; i < cs; i++) {
    result(i,1) = Rcpp::IntegerVector::is_na(cell[i]) ? NA_INTEGER : trunc(cell[i]/ncol);
    result(i,0) = Rcpp::IntegerVector::is_na(cell[i]) ? NA_INTEGER : (cell[i] - ((result(i,1)) * ncol));
  }
  return result;
}

std::vector<std::vector<double>> xyFromCell(RasterInfo ras, const std::vector<int> &cell) {
  //RasterInfo ras(raster);
  int n = cell.size();
  
  std::vector<std::vector<double>> out;
  out.resize(n, std::vector<double>(2, NA_REAL));
  
  for (int i = 0; i<n; i++) {
    if (Rcpp::IntegerVector::is_na(cell[i]) || (cell[i] < 0) || (cell[i] >= ras.ncell)) continue;
    int row = cell[i] / ras.ncol;
    int col = cell[i] - (row * ras.ncol);
    out[i][0] = ras.xmin + (col + 0.5) * ras.res;
    out[i][1] = ras.ymax - (row + 0.5) * ras.res;
  }
  return out;
}

std::vector<std::vector<double>> xyFromCell(RasterInfo ras, const int cell) {
  std::vector<int> vcell;
  vcell.push_back(cell);
  return xyFromCell(ras, vcell);
}

std::vector<int> cellFromXY(RasterInfo ras, const std::vector<std::vector<double>> &xy) {
  // size of x and y should be the same
  //RasterInfo ras(raster);
  
  int s = xy.size();
  std::vector<int> cells(s);
  
  double yr_inv = ras.nrow / (ras.ymax - ras.ymin);
  double xr_inv = ras.ncol / (ras.xmax - ras.xmin);
  
  for (int i = 0; i < s; i++) {
    int row = floor((ras.ymax - xy[i][1]) * yr_inv);
    // points in between rows go to the row below
    // except for the last row, when they must go up
    if (xy[i][1] == ras.ymin) {
      row = ras.nrow-1 ;
    }
    
    int col = floor((xy[i][0] - ras.xmin) * xr_inv);
    // as for rows above. Go right, except for last column
    if (xy[i][0] == ras.xmax) {
      col = ras.ncol - 1 ;
    }
    if (row < 0 || row >= ras.nrow || col < 0 || col >= ras.ncol) {
      cells[i] = NA_INTEGER;
    } else {
      cells[i] = row * ras.ncol + col;
    }
  }
  
  return cells;
}

int cellFromXY2(RasterInfo ras, double x, double y) {
  
  int cell;
  
  double yr_inv = ras.nrow / (ras.ymax - ras.ymin);
  double xr_inv = ras.ncol / (ras.xmax - ras.xmin);
  
  int row = floor((ras.ymax - y) * yr_inv);
  // points in between rows go to the row below
  // except for the last row, when they must go up
  if (y == ras.ymin) {
    row = ras.nrow-1 ;
  }
  
  int col = floor((x - ras.xmin) * xr_inv);
  // as for rows above. Go right, except for last column
  if (x == ras.xmax) {
    col = ras.ncol - 1 ;
  }
  if (row < 0 || row >= ras.nrow || col < 0 || col >= ras.ncol) {
    cell = NA_INTEGER;
  } else {
    cell = row * ras.ncol + col;
  }
  
  return cell;
}