#include <Rcpp.h>
#include "rsinfo.h"
#include "rasterutils.h"
#include "bresenham.h"

using namespace Rcpp;

int Sign2(const int dxy)
{
  if(dxy<0) return -1; 
  else if(dxy>0) return 1; 
  else return 0;
}


Rcpp::IntegerMatrix bresenham_map(const int x0, const int y0, const int radius, const int nc)
{
  int x1;
  int y1 = y0+radius;
  
  int Dx, Dy, Sx, Sy, X, Y, c;
  double R;
  
  Rcpp::IntegerMatrix out(radius+1,radius);
  std::fill( out.begin(),out.end(),NA_INTEGER );
  
  Dy = y1 - y0;
  Sy = Sign2(Dy); 
  
  for(int r=0; r<=radius; r++) {
    x1 = x0+r;
    
    // Segment length
    Dx = x1 - x0;
    
    // Increments
    Sx = Sign2(Dx);
    
    // Initial remainder
    R = radius / 2;
    
    X = x0;
    Y = y0;
    
    // Initial update (X, Y) and R
    Y+= Sy; R+= Dx; // Lateral move
    if(R >= Dy) {    
      X+= Sx; 
      R-= Dy; // Diagonal move
    }
    
    c = 0;
    while( ((x0-X)*(x0-X)+(y0-Y)*(y0-Y)) <= (radius*radius) ) {
      out(r,c) = Y*nc + X;
      
      // Update (X, Y) and R
      Y+= Sy; R+= Dx; // Lateral move
      if(R >= Dy) {    
        X+= Sx; 
        R-= Dy; // Diagonal move
      }
      c++;  
    }
  }
  
  return out;
}


Rcpp::NumericVector tangentsMap(const int x0, const int y0, const double h0, 
                                const int nr, const int nc,
                                const int r, const Rcpp::NumericVector &dsm_vec){
  const int range = (2*r)+1;
  Rcpp::NumericVector out(range*range, NA_REAL);
  int i = 0;
  
  for (int y = y0-r; y <= y0+r; ++y) {
    for (int x = x0-r; x<=x0+r; ++x) {
      const int cell = y*nc + x;
      
      if( !(y<0 || y >= nr || x<0 || x >= nc || Rcpp::NumericVector::is_na(dsm_vec[cell])) ){
        const double distance_traveled = sqrt((x0 - x)*(x0 - x) + (y0 - y)*(y0 - y));
        if(distance_traveled <= r) {
          out[i] = (dsm_vec[cell] - h0) / (distance_traveled);
        }
      }
      i++;
    }
  }
  return out;
}


void LoS(const Rcpp::IntegerMatrix &los_ref_mat,
         const Rcpp::NumericVector &tan_vec,
         Rcpp::LogicalVector &visibility_vec,
         const int &j, const int &r){
  
  double max_tangent = -9999;
  
  for(int i = 0; i < r; ++i) {
    int cell = los_ref_mat( j,i );
    
    if(!Rcpp::IntegerVector::is_na(cell)) {
      double this_tangent = tan_vec[cell];
      
      if(!Rcpp::NumericVector::is_na(this_tangent)) {
        if (this_tangent > max_tangent) {
          max_tangent = this_tangent;
          visibility_vec[cell] = TRUE;
        }
      }
    } else {
      break;
    }
  }
}


// [[Rcpp::export]]
Rcpp::IntegerVector LoS_reference(const int x0_ref, const int y0_ref, const int r, const int nc_ref) {
  const int l = r+1;
  
  // 1. Reference Bresenham Lines for the first eigth of circle
  const Rcpp::IntegerMatrix bh_mat = bresenham_map(x0_ref, y0_ref, r, nc_ref);
  
  // 2. Reference matrix (los_mat): Project reference Bresenham Lines to all perimeter cells
  // Will be used as a reference for all input points
  Rcpp::IntegerVector los_ref_vec = Rcpp::IntegerVector(r*8 * r);
  
  for(int i=0; i<l; i++){
    const Rcpp::IntegerMatrix bs_xy = colRowFromCell(bh_mat( i,_ ), nc_ref);
    
    for(int j=0; j<r; j++){
      if ( Rcpp::IntegerVector::is_na(bs_xy( j,0 )) || Rcpp::IntegerVector::is_na(bs_xy( j,1 )) ) {
        los_ref_vec[(0*r+i)*r + j] = NA_INTEGER;
        los_ref_vec[(2*r+i)*r + j] = NA_INTEGER;
        los_ref_vec[(4*r+i)*r + j] = NA_INTEGER;
        los_ref_vec[(6*r+i)*r + j] = NA_INTEGER;
        
        if( i != 0 && i != (l-1) ) {
          los_ref_vec[(2*r-i)*r + j] = NA_INTEGER;
          los_ref_vec[(4*r-i)*r + j] = NA_INTEGER;
          los_ref_vec[(6*r-i)*r + j] = NA_INTEGER;
          los_ref_vec[(8*r-i)*r + j] = NA_INTEGER;
        }
      } else {
        const int x = bs_xy( j,0 )-x0_ref;
        const int y = bs_xy( j,1 )-x0_ref;
        los_ref_vec[(0*r+i)*r + j] = (y+y0_ref)*nc_ref + (x+x0_ref);
        los_ref_vec[(2*r+i)*r + j] = (-x+y0_ref)*nc_ref + (y+x0_ref);
        los_ref_vec[(4*r+i)*r + j] = (-y+y0_ref)*nc_ref + (-x+x0_ref);
        los_ref_vec[(6*r+i)*r + j] = (x+y0_ref)*nc_ref + (-y+x0_ref);
        
        if( i != 0 && i != (l-1) ) {
          los_ref_vec[(2*r-i)*r + j] = (x+y0_ref)*nc_ref + (y+x0_ref);
          los_ref_vec[(4*r-i)*r + j] = (-y+y0_ref)*nc_ref + (x+x0_ref);
          los_ref_vec[(6*r-i)*r + j] = (-x+y0_ref)*nc_ref + (-y+x0_ref);
          los_ref_vec[(8*r-i)*r + j] = (y+y0_ref)*nc_ref + (-x+x0_ref);
        }
        
      }
    }
  }
  
  return los_ref_vec;
}


Rcpp::IntegerVector shared_LoS(const int r, const Rcpp::IntegerVector &los_ref_vec){
  Rcpp::IntegerVector out(r*8);
  out[0] = 0;
  
  for(int i = 1; i < (r*8); i++){
    // Re-use tangents calculated from prior LoS
    int k_i = 0;
    for(int j = 0; j < r; j++){
      k_i = j;
      if(los_ref_vec[i*r + j] != los_ref_vec[(i-1)*r + j]){
        out[i] = k_i;
        break;
      }
    }
  }
  
  return(out);
}