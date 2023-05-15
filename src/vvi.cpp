#include <Rcpp.h>
#include <chrono>
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
#include "eta_progress_bar.hpp"

// [[Rcpp::export]]
std::list<std::vector<int>> VVI_cpp(Rcpp::S4 &dsm, const Rcpp::NumericVector &dsm_values,
                             const Rcpp::IntegerVector &x0, const Rcpp::IntegerVector &y0,
                             const Rcpp::NumericVector &h0, const int radius,
                             const int ncores=1, const bool display_progress=false)
{
  // Cells from x0, y0
  Rcpp::IntegerVector x0_o = x0-1;
  Rcpp::IntegerVector y0_o = y0-1;
  const Rcpp::IntegerVector input_cells = Rcpp::na_omit(cellFromColRowSensitive(dsm, x0_o, y0_o));
  
  // Basic raster information
  const RasterInfo dsm_ras(dsm);

  // Parameters
  const int r = (int)round(radius/dsm_ras.res);
  const int nc_ref = (2*r)+1, nr_ref = (2*r)+1;
  const int x0_ref = r, y0_ref = x0_ref;
  const int c0_ref = y0_ref*nc_ref + x0_ref;
  
  // Output list
  std::list<std::vector<int>> output;
  
  // Prototype of Line of Sight (LoS) paths:
  // Will be used as a reference for all input points
  const Rcpp::IntegerVector los_ref_vec = LoS_reference(x0_ref, y0_ref, r, nc_ref);
  const Rcpp::IntegerVector los_start = shared_LoS(r, los_ref_vec);
  
  // Progress bar
  ETAProgressBar pb_ETA;
  Progress pb(input_cells.size(), display_progress, pb_ETA);
  
  // Main loop
#if defined(_OPENMP)
  omp_set_num_threads(ncores);
#pragma omp parallel for schedule(dynamic) shared(output)
#endif
  for(int k = 0; k < input_cells.size(); ++k){
    if ( !pb.is_aborted() ) {
      Progress::check_abort();
      
      const int this_input_cell = input_cells[k];
      
      // Viewshed
      std::vector<int> viewshed(nc_ref*nr_ref, NA_INTEGER);
      
      // Viewshed at x0/y0 is always visible
      viewshed[c0_ref] = input_cells[k]+1;
      
      // A: Viewshed Analysis
      // Eye-level height at x0/y0 > DSM height?
      if(h0[k] > dsm_values[this_input_cell]){
        
        // Parameter for projecting reference cell to true cell values
        const int x = this_input_cell - c0_ref - r*(dsm_ras.ncol-nc_ref);
        
        // Vector for storing max tangent from previous LoS
        std::vector<double> max_tan_vec(r, -9999.0);
        
        // Iterate over all LoS paths
        for(int i = 0; i < (r*8); ++i){
          // Re-use tangents calculated from prior LoS or assign -9999.0
          const int k_i = los_start[i];
          double max_tan = (k_i > 1) ? max_tan_vec[k_i-1] : -9999.0;
          
          // Iterate over all cells of this LoS path starting at k_i
          for(int j = k_i; j < r; ++j){
            
            // This LoS path cell
            const int los_ref_cell = los_ref_vec[i*r + j];
            
            if(!Rcpp::IntegerVector::is_na(los_ref_cell)){
              
              // Project reference cell to true cell value
              const int cell = x + los_ref_cell + trunc(los_ref_cell/nc_ref)*(dsm_ras.ncol-nc_ref);
              
              // DSM height at this LoS path cell
              const double h_cell = dsm_values[cell];
              
              // Test if this LoS path cell is within DSM raster
              const int row = trunc(cell/dsm_ras.ncol);
              const int col = cell - (row * dsm_ras.ncol);
              const int dcol = abs(col-x0_o[k]);
              
              if(!(cell<0 || cell > dsm_ras.ncell || Rcpp::NumericVector::is_na(h_cell) || dcol>r)){
                // Compute tangent of x0/y0 (observer location) and this LoS path cell
                const double distance_traveled = sqrt(
                  (x0_o[k] - col)*(x0_o[k] - col) + (y0_o[k] - row)*(y0_o[k] - row)
                );
                const double this_tan = (h_cell - h0[k]) / (distance_traveled);
                
                // Update viewshed and max tangent
                if(this_tan > max_tan){
                  max_tan = this_tan;
                  viewshed[los_ref_cell] = cell+1;
                }
              }
              
              max_tan_vec[j] = max_tan;
            } else {
              break;
            }
          }
        }
      }
      
      
      viewshed.erase(std::remove_if(std::begin(viewshed),
                                    std::end(viewshed),
                                    [](const int& value) { return Rcpp::IntegerVector::is_na(value); }),
                                    std::end(viewshed));
      
      
      // B: viewshed indices
      output.push_back(viewshed);
      pb.increment();
    }
  }
  
  return output;
}
