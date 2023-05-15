#include <Rcpp.h>
#include "rsinfo.h"
using namespace Rcpp;

std::vector<int> findBestIndices(std::vector<double> &d, const int &N)
{   
  std::vector<int> indices(d.size());
  std::iota(indices.begin(), indices.end(), 0); // fill with 0,1,2,...
  
  std::partial_sort(indices.begin(), indices.begin()+N, indices.end(),
                    [&d](int i,int j) {return d[i]<d[j];});
  
  return std::vector<int>(indices.begin(), indices.begin()+N);
}

double calc_idw(std::vector<double> &d, std::vector<double> &v, const double b){
  double numerator = 0.0;
  double denominator = 0.0;
  
  // Sum from i to n
  for(std::size_t i = 0; i < d.size(); i++){
    numerator += v[i] / pow(d[i], b);
    denominator += 1 / pow(d[i], b);
  }
  
  return numerator/denominator;
}


#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

// [[Rcpp::export]]
NumericVector IDW_cpp(S4 &rast, const NumericVector &x,
                      const NumericVector &sf_x, const NumericVector &sf_y, const NumericVector &sf_z,
                      const size_t n, const double b, const double radius, const int mode=1,
                      const bool na_only=false, const int ncores=1, const bool display_progress=false)
{
  // Basic raster information
  RasterInfo rast_info(rast);
  
  // Convert radius to pixel
  const int r_pxl = (int)(radius/rast_info.res);
  
  
  // Output
  NumericVector out(x.size(), NA_REAL);
  
  // Progress bar
  Progress pb(x.size(), display_progress);

  // Main loop: Loop over all values of the raster x
#if defined(_OPENMP)
  omp_set_num_threads(ncores);
#pragma omp parallel for schedule(dynamic) shared(out)
#endif
  for(int j = 0; j < x.size(); j++){
    if ( pb.increment() ) {

      if(na_only == false || (na_only && NumericVector::is_na(x[j]))){
        // 1. Convert j to row/col and X/Y coordinates
        // row col from cell
        const int row_j = j / rast_info.ncol;
        const int col_j = j - (row_j * rast_info.ncol);
        
        // XY from cell
        const double y_j = rast_info.ymax - (row_j + 0.5) * rast_info.res;
        const double x_j = rast_info.xmin + (col_j + 0.5) * rast_info.res;
        
        
        // 2. Calculate distance to all cells and store their values
        // Distance (d) and value (z) vector
        std::vector<double> d;
        std::vector<double> z;
        
        // Iterate over all cells that are within the radius
        if(mode == 0){
          for(int i = 0; i < sf_x.size(); i++){
            // Distance
            const double dist = sqrt((x_j-sf_x[i])*(x_j-sf_x[i]) + (y_j-sf_y[i])*(y_j-sf_y[i]));
            
            // If distance <= 0, use a small value (resolution / 4) instead
            if(dist <= 0){
              d.push_back(rast_info.res/4);
              z.push_back(sf_z[i]);
            } else if(dist <= radius) {
              d.push_back(dist);
              z.push_back(sf_z[i]);
            }
          }
        } else {
          for(int row_i = row_j-r_pxl; row_i <= row_j+r_pxl; row_i++){
            if(row_i > 0 && row_i < rast_info.nrow){
              
              for(int col_i = col_j-r_pxl; col_i <= col_j+r_pxl; col_i++){
                if(col_i > 0 && col_i < rast_info.ncol){
                  
                  // Cell from row/col
                  const int i = row_i * rast_info.ncol + col_i;
                  const double i_value = x[i];
                  
                  if(!NumericVector::is_na(i_value)) {
                    // XY from cell
                    const double y_i = rast_info.ymax - (row_i + 0.5) * rast_info.res;
                    const double x_i = rast_info.xmin + (col_i + 0.5) * rast_info.res;
                    
                    // Distance
                    const double dist = sqrt((x_j-x_i)*(x_j-x_i) + (y_j-y_i)*(y_j-y_i));
                    
                    // Save distance and value
                    if(i == j){
                      d.push_back(rast_info.res/4);
                      z.push_back(i_value);
                    } else if(dist <= radius) {
                      d.push_back(dist);
                      z.push_back(i_value);
                    }
                  }
                }
              }
            }
          }
        }
        
        
        if(d.size() > 0) {
          
          // 3. Sort by distance and select top n
          int nn = (d.size() < n) ? d.size() : n;
          
          // Index of n shortest distances
          std::vector<int> idx = findBestIndices(d, nn);
          
          // And select value (z) and distance (d) in that order
          std::vector<double> z_top_n;
          std::vector<double> d_top_n;
          for(auto t=idx.begin(); t!=idx.end(); ++t){
            z_top_n.push_back(z[*t]);
            d_top_n.push_back(d[*t]);
          }
          
          
          // 4. Compute IDW
          out[j] = calc_idw(d_top_n, z_top_n, b);
        }
      } else {
        out[j] = x[j];
      }
    }
  }
  
  return out;
}