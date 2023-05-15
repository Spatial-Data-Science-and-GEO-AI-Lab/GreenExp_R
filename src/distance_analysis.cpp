#include <Rcpp.h>
#include "rsinfo.h"
#include "rasterutils.h"
#include "bresenham.h"
#include "integrate.h"

using namespace Rcpp;

#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

// [[Rcpp::export]]
Rcpp::IntegerMatrix viewshed_distance_analysis_cpp(Rcpp::S4 &dsm, const Rcpp::NumericVector &dsm_values,
                                                   const Rcpp::IntegerVector x0, const Rcpp::IntegerVector y0,
                                                   const int radius, const Rcpp::NumericVector & h0,
                                                   const int ncores=1, const bool display_progress=false)
{
  // Cells from x0,y0
  Rcpp::IntegerVector x0_o = x0-1;
  Rcpp::IntegerVector y0_o = y0-1;
  const Rcpp::IntegerVector input_cells = Rcpp::na_omit(cellFromColRowSensitive(dsm, x0_o, y0_o));
  
  // Basic raster information
  const RasterInfo ras(dsm);
  
  // Parameters
  const int r = (int)(radius/ras.res);
  const int nc_ref = (2*r)+1;
  const int x0_ref = r, y0_ref = x0_ref;
  const int c0_ref = y0_ref*nc_ref + x0_ref;
  const int s = input_cells.size();
  
  // Output
  Rcpp::IntegerMatrix output( r*s,3 );
  for(int k = 0; k < s; k++) {
    for(int d = 0; d<r; d++) {
      output( (d*s + k),0 ) = (d+1) * (int)ras.res;
    }
  }
  //for(int d = 0; d<r; d++) {
  //  output( d,0 ) = d+1;
  //}
  
  
  
  // Reference matrix (los_mat): Project reference Bresenham Lines to all perimeter cells
  // Will be used as a reference for all input points
  const Rcpp::IntegerVector los_ref_vec = LoS_reference(x0_ref, y0_ref, r, nc_ref);
  
  // Progress bar
  Progress p(s, display_progress);
  
  // Main loop
  for(int k = 0; k < s; k++)
  {
    if(h0[k] > dsm_values[input_cells[k]]){
      // Viewshed Analysis
      const int x = input_cells[k] - c0_ref - r*(ras.ncol-nc_ref);
      
#if defined(_OPENMP)
      omp_set_num_threads(ncores);
#pragma omp parallel for shared(output)
#endif    
      for(int i = 0; i < (r*8); i++){
        if (!p.is_aborted()) {
          Progress::check_abort();
          
          double max_tan = -9999.0;
          
          for(int j = 0; j < r; j++){
            const int los_ref_cell = los_ref_vec[i*r + j];
            if(!Rcpp::IntegerVector::is_na(los_ref_cell)){
              // Project reference cells to actual cell value
              int cell = x + los_ref_cell + trunc(los_ref_cell/nc_ref)*(ras.ncol-nc_ref);
              int row = trunc(cell/ras.ncol);
              int col = cell - (row * ras.ncol);
              int dcol = abs(col-x0_o[k]);
              
              if(!(cell < 0 || cell > ras.ncell || Rcpp::NumericVector::is_na(dsm_values[cell]) || dcol>r)){
                // Compute tangents of LoS_vec and update output
                double distance_traveled = sqrt((x0_o[k] - col)*(x0_o[k] - col) + (y0_o[k] - row)*(y0_o[k] - row));
                double this_tan = (dsm_values[cell] - h0[k]) / (distance_traveled);
                
                int d = round(distance_traveled) - 1;
                d = (d < 0) ? 0 : d;
                d = (d >= r) ? (r-1) : d;
                
                output( (d*s + k),1 ) += 1;
                //output( d,1 ) += 1;
                
                if(this_tan > max_tan){
                  max_tan = this_tan;
                  
                  output( (d*s + k),2 ) += 1;
                  //output( d,2 ) += 1;
                }
              }
            } else {
              break;
            }
          }
        }
      }
    }
    p.increment();
  }
  
  return output;
}


// [[Rcpp::export]]
Rcpp::IntegerMatrix viewshed_and_greenness_distance_analysis_cpp(Rcpp::S4 &dsm, const Rcpp::NumericVector &dsm_values,
                                                                 Rcpp::S4 &greenspace, const Rcpp::NumericVector &greenspace_values,
                                                                 const Rcpp::IntegerVector x0, const Rcpp::IntegerVector y0,
                                                                 const int radius, const Rcpp::NumericVector & h0,
                                                                 const int ncores=1, const bool display_progress=false)
{
  // Cells from x0,y0
  Rcpp::IntegerVector x0_o = x0-1;
  Rcpp::IntegerVector y0_o = y0-1;
  const Rcpp::IntegerVector input_cells = Rcpp::na_omit(cellFromColRowSensitive(dsm, x0_o, y0_o));
  
  // Basic raster information
  const RasterInfo ras(dsm);
  const RasterInfo gs_ras(greenspace);
  
  // Parameters
  const int r = (int)(radius/ras.res);
  const int nc_ref = (2*r)+1;
  const int x0_ref = r, y0_ref = x0_ref;
  const int c0_ref = y0_ref*nc_ref + x0_ref;
  const int s = input_cells.size();
  
  // Output
  // v1: Distance
  // V2: Total number of cells per distance
  // V3: Number of all visible cells per distance
  // V4: Total number of green cells per distance
  // V5: Number of green visible cells per distance
  Rcpp::IntegerMatrix output( r*s,5 );
  for(int k = 0; k < s; k++) {
    for(int d = 0; d<r; d++) {
      output( (d*s + k),0 ) = (d+1) * (int)ras.res;
    }
  }
  
  
  
  // Reference matrix (los_mat): Project reference Bresenham Lines to all perimeter cells
  // Will be used as a reference for all input points
  const Rcpp::IntegerVector los_ref_vec = LoS_reference(x0_ref, y0_ref, r, nc_ref);
  
  // Progress bar
  Progress p(s, display_progress);
  
  // Main loop
#if defined(_OPENMP)
  omp_set_num_threads(ncores);
#pragma omp parallel for shared(output)
#endif
  for(int k = 0; k < s; k++)
  {
    if(h0[k] > dsm_values[input_cells[k]]){
      // Viewshed Analysis
      const int x = input_cells[k] - c0_ref - r*(ras.ncol-nc_ref);
      
      for(int i = 0; i < (r*8); i++){
        if (!p.is_aborted()) {
          Progress::check_abort();
          
          double max_tan = -9999.0;
          
          for(int j = 0; j < r; j++){
            const int los_ref_cell = los_ref_vec[i*r + j];
            if(los_ref_cell != NA_INTEGER){ //!Rcpp::IntegerVector::is_na(los_ref_cell)
              // Project reference cells to actual cell value
              int cell = x + los_ref_cell + trunc(los_ref_cell/nc_ref)*(ras.ncol-nc_ref);
              
              // row/col
              int row = trunc(cell/ras.ncol);
              int col = cell - (row * ras.ncol);
              int dcol = abs(col-x0_o[k]);
              
              // X/Y coordinates
              double x_j = ras.xmin + (col + 0.5) * ras.res;
              double y_j = ras.ymax - (row + 0.5) * ras.res;
              
              // Cell of greenspace raster
              // OpenMP can't call cellFromXY2 function without causing R to crash.
              // Therefore, I'll calcualte the cell here manually
              int cell_gs = cellFromXY2(gs_ras, x_j, y_j);
              
              
              if(!(cell < 0 || cell > ras.ncell || Rcpp::NumericVector::is_na(dsm_values[cell]) || dcol>r)){ 
                // Compute tangents of LoS_vec and update output
                double distance_traveled = sqrt((x0_o[k] - col)*(x0_o[k] - col) + (y0_o[k] - row)*(y0_o[k] - row));
                double this_tan = (dsm_values[cell] - h0[k]) / (distance_traveled);
                
                int d = round(distance_traveled) - 1;
                d = (d < 0) ? 0 : d;
                d = (d >= r) ? (r-1) : d;
                
                output( (d*s + k),1 ) += 1;
                if(cell_gs != NA_INTEGER && greenspace_values[cell_gs] == 1){
                  output( (d*s + k),3 ) += 1;  
                }
                
                if(this_tan > max_tan){
                  max_tan = this_tan;
                  
                  output( (d*s + k),2 ) += 1;
                  if(cell_gs != NA_INTEGER && greenspace_values[cell_gs] == 1){
                    output( (d*s + k),4 ) += 1;  
                  }
                }
              }
            } else {
              break;
            }
          }
        }
      }
    }
    p.increment();
  }
  
  return output;
}