#ifndef INTEGRATE
#define INTEGRATE

#include <Rcpp.h>

extern double f1(double x, double m, double b);
extern double f2(double x, double m, double b);
extern double integrate(double lower, double upper, int n, int fun, double m, double b);

#endif