#include <Rcpp.h>
#include "integrate.h"

using namespace Rcpp;

double f1(double x, double m, double b) {
  return 1/(1 + exp(b * (x - m)));
}
double f2(double x, double m, double b) {
  return 1 / (1 + (b * pow(x, m)));
}

double integrate(double lower, double upper, int n, int fun, double m, double b){
  
  double sum=0;
  std::vector<double> x(n+1);
  std::vector<double> y(n+1);    
  double h = (upper-lower)/n;     //get the width of the subintervals
  for (int i = 0; i <= n; i++) {  //loop to evaluate x0,...xn and y0,...yn
    x[i]=lower+i*h;               //and store them in arrays
    if ( fun == 1){
      y[i]=f1(x[i], m, b);
    } else {
      y[i]=f2(x[i], m, b);
    }
  }
  
  for (int i = 1; i < n; i++) {   //loop to evaluate h*(y1+...+yn-1)
    sum += h*y[i];
  }
  
  return h / 2.0*(y[0]+y[n]) + sum;
}