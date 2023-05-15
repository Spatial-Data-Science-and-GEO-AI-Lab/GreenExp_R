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
std::vector<double> VGVI_cpp(Rcpp::S4 &dsm, const Rcpp::NumericVector &dsm_values,
                             Rcpp::S4 &greenspace, const Rcpp::NumericVector &greenspace_values,
                             const Rcpp::IntegerVector &x0, const Rcpp::IntegerVector &y0,
                             const Rcpp::NumericVector &h0, const int radius,
                             const int fun, const double m, const double b,
                             const int ncores=1, const bool display_progress=false)
{
  // Cells from x0, y0
  Rcpp::IntegerVector x0_o = x0-1;
  Rcpp::IntegerVector y0_o = y0-1;
  const Rcpp::IntegerVector input_cells = Rcpp::na_omit(cellFromColRowSensitive(dsm, x0_o, y0_o));
  
  // Basic raster information
  const RasterInfo dsm_ras(dsm);
  const RasterInfo gs_ras(greenspace);
  
  // Parameters
  const int r = (int)round(radius/dsm_ras.res);
  const int nc_ref = (2*r)+1, nr_ref = (2*r)+1;
  const int x0_ref = r, y0_ref = x0_ref;
  const int c0_ref = y0_ref*nc_ref + x0_ref;
  
  // Output vector
  std::vector<double> output(input_cells.size(), NA_REAL);
  
  // Protoptype of Line of Sight (LoS) paths:
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
      
      
      // B: Greenspace Visibility Index
      const int cs = viewshed.size();
      
      // Get XY coordinates of visible cells
      const std::vector<std::vector<double>> dsm_xy = xyFromCell(dsm_ras, viewshed);
      const std::vector<std::vector<double>> xy0 = xyFromCell(dsm_ras, input_cells[k]);
      
      // Calculate distance
      std::vector<int> dxy(cs);
      for(int i = 0; i < cs; i++){
        double d = sqrt( ((xy0[0][0] - dsm_xy[i][0])*(xy0[0][0] - dsm_xy[i][0])) + ((xy0[0][1] - dsm_xy[i][1])*(xy0[0][1] - dsm_xy[i][1])) );
        int di = round(d);
        if(di < 1) {
          dxy[i] = 1;
        } else {
          dxy[i] = di;
        }
      }
      
      // Intersect XY with greenspace mask
      std::vector<int> greenspace_cells = cellFromXY(gs_ras, dsm_xy);
      std::vector<double> greenspace_cell_values(cs, 0.0);
      for(int i = 0; i < cs; i++){
        int gs_cell = greenspace_cells[i]; 
        if(!Rcpp::IntegerVector::is_na(gs_cell)){
          double gs = greenspace_values[gs_cell];
          greenspace_cell_values[i] =  Rcpp::NumericVector::is_na(gs) ? 0.0 : gs;
        }
      }
      
      // Get number of green visible cells and total visible cells per distance
      const double max_d = *std::max_element(dxy.begin(), dxy.end()) + 0.0;
      
      std::vector<int> dxy_seq(max_d);
      std::iota(dxy_seq.begin(), dxy_seq.end(), 1);
      
      const int n = dxy_seq.size();
      
      std::vector<int> visibleTotal(n);
      std::vector<int> visibleGreen(n);
      
      for(int i = 0; i < cs; i++){
        int this_dxy = (int)dxy[i];
        visibleTotal[this_dxy-1] += 1;
        visibleGreen[this_dxy-1] += greenspace_cell_values[i];
      }
      for(int i = 0; i < n; i++){
        if(visibleTotal[i] == 0){
          visibleTotal[i] = 1;
        }
      }
      
      // Proportion of visible green cells
      if(max_d == 1.0){
        output[k] = visibleGreen[0]/visibleTotal[0];
        continue;
      }
      std::vector<double> raw_GVI(n);
      for(int i = 0; i < n; i++){
        raw_GVI[i] = (double)visibleGreen[i] / visibleTotal[i];
      }
      
      
      // Normalize distance
      std::vector<double> nDxy(n);
      for(int i = 0; i < n; i++){
        nDxy[i] = dxy_seq[i] / (double)radius;
      }
      
      
      // Calculate weights by taking the proportion of the integral
      // of each step from the integral of the whole area
      const double min_dist = *std::min_element(nDxy.begin(), nDxy.end());
      std::vector<double> decayWeights(n);
      for(int i = 0; i < n; i++){
        double d = nDxy[i];
        decayWeights[i] = integrate(d-min_dist, d, 200, fun, m, b);
      }
      const double big_integral = std::accumulate(decayWeights.begin(), decayWeights.end(), 0.0);
      
      
      // Proportion of visible green
      double vgvi_sum = 0.0; 
      for(int i = 0; i < n; i++){
        vgvi_sum += raw_GVI[i] * (decayWeights[i]/big_integral);
      }
      
      output[k] = vgvi_sum;
      pb.increment();
    }
  }
  
  return output;
}