#ifndef RSINFO
#define RSINFO

#include <Rcpp.h>

struct RasterInfo {
  double xmin, xmax, ymin, ymax, xres, yres, res;
  int nrow, ncol, ncell;
  
  RasterInfo(Rcpp::S4 raster) {
    Rcpp::S4 extent = raster.slot("extent");
    xmin = extent.slot("xmin");
    xmax = extent.slot("xmax");
    ymin = extent.slot("ymin");
    ymax = extent.slot("ymax");
    nrow = raster.slot("nrows");
    ncol = raster.slot("ncols");
    ncell = nrow*ncol;
    res = (xmax - xmin)/ncol;
    
    if(raster.slot("rotated")) {
      Rcpp::stop("No current support for rotated rasters.");
      // Rcpp::S4 rotation = raster.slot("rotation");
      // Rcpp::NumericVector geotrans = rotation.slot("geotrans");
      // xres = geotrans[2];
      // yres = geotrans[4];
    }
  }
};

#endif