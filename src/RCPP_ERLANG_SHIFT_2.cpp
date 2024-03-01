#include <Rcpp.h>
#include <math.h>       
#include <list>      // list class-template definition
#include <algorithm> // copy algorithm
#include <iterator>  // ostream_iterator
#include <iostream> 
#include <cmath>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]

List ErlangInsect_Shift(
    double t,  
    NumericVector y, 
    NumericVector params) {
  
  int n1 = params[0];//shape parameter
  int n2= params[1];//shape parameter
  double dev1 = params[2];// #development rate out of S1 parameter
  double dev2 = params[3];// #development rate out of S1 parameter
  double mu1 = params[4]; //#mortality parameter
  double mu2 = params[5]; //#mortality parameter
  double m1 = params[6];
  double m2 = params[7];
  double mk = params[8];
  double a = params[9];
  double shift = params[10];
  double svar; 
  
  if(t < shift)
    svar = 0;
  if (t >= shift)
    svar = 1;
  
  NumericVector dy(n1+n2+3);
  
  
  double m = m1*((pow( y[n1+n2+2]/m2, mk))/(pow(1+(y[n1+n2+2]/m2),mk)));
  
  dy[0] =  -n1* dev1 * y[0];
  
  for (int i=1; i < n1; ++i) {
    dy[i] = n1 * dev1 * y[i - 1] - n1 * dev1 *y[i];
  }
  
  dy[n1]  = n1 * dev1 * y[n1-1] - mu1 * y[n1]- 
    m*y[n1];
  
  
  dy[n1+1] =  -svar*n2* dev2* y[n1+1];
  
  for (int i=1; i < n2; ++i) {
    dy[n1+1+i] = svar*n2 * dev2 * y[n1+i] -svar* n2 * dev2 *y[n1+1+i];
  }
  dy[n1+n2+1]  = svar*n2 * dev2 * y[n1+n2] - mu2 * y[n1+n2+1]- 
    
    m*y[n1+n2+1];
  
  
  dy[n1+n2+2] =  -a * y[n1+n2+2];
  
  
  
  return List ::create(dy);
  
  
}




