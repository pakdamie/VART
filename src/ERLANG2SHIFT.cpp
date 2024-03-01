#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or vi#include <Rcpp.h>
#include <math.h>       
#include <list>      // list class-template definition
#include <algorithm> // copy algorithm
#include <iterator>  // ostream_iterator

using namespace Rcpp;


// [[Rcpp::export]]

List ErlangInsect2Shift(
    double t,  
    NumericVector y, 
    NumericVector params) {
  
  
  int n1 = params[0];//shape parameter
  int n2 = params[1];//shape parameter
  double dev1 = params[2];// #development rate out of S1 parameter
  double dev2 = params[3];// #development rate out of S1 parameter
  double mu1 = params[4]; //#mortality parameter
  double mu2 = params[5]; //#mortality parameter
  double top1 =params[6];
  double top2 =params[7];
  double top3 =params[8];
  double m = params[9];
  double a = params[10];
  int shift = params[11];
  double spray1;
  double spray2;
  double spray3;
  
  double top1s = top1 -  shift;
  double top2s = top2 - shift;
  double top3s = top3 - shift;
  
  
  double spray1s;
  double spray2s;
  double spray3s;
  
  if(t < top1)
    spray1 = 0;
  if (t >= top1)
    spray1 = 1;
  
  if(t < top2)
    spray2 = 0;
  if (t >= top2)
    spray2 = 1;
  
  if(t < top3)
    spray3 = 0;
  if (t >= top3)
    spray3 = 1;
  
  //shifted species 2 
  if(t < top1s)
    spray1s = 0;
  if (t >= top1s)
    spray1s = 1;
  
  if(t < top2s)
    spray2s = 0;
  if (t >= top2s)
    spray2s = 1;
  
  if(t < top3s)
    spray3s = 0;
  if (t >= top3s)
    spray3s = 1;
  
  double Z1 =  m * exp(-a * (t-top1));
  double Z2 =  m * exp(-a * (t-top2));
  double Z3 =  m * exp(-a * (t-top3));
  
  
  double Z1s =  m * exp(-a * (t-top1s));
  double Z2s =  m * exp(-a * (t-top2s));
  double Z3s =  m * exp(-a * (t-top3s));
  
  NumericVector dy(n1+n2+2);
  
  dy[0] =  -n1* dev1 * y[0];
  for (int i=1; i < n1; ++i) {
    dy[i] = n1 * dev1 * y[i - 1] - n1 * dev1 *y[i];
  }
  
  dy[n1]  = n1 * dev1 * y[n1-1] - mu1 * y[n1]-
    spray1*Z1 * y[n1]-
    spray2*Z2*y[n1]-
    spray3*Z3 *y[n1];
  
  dy[n1+1]= -n2 *dev2 *y[n1+1];
  for (int i=1; i < n2; ++i) {
    dy[n1+1+i] = n2 * dev2 * y[n1+i ] - n2 * dev2 *y[n1+1+i];
  }
  dy[n1+n2+1]  = n2 * dev2 * y[n1+n2] - mu2 * y[n1+n2+1]-
    spray1s*Z1s * y[n1+n2+1]-
    spray2s*Z2s*y[n1+n2+1]-
    spray3s*Z3s *y[n1+n2+1];
  return List ::create(dy);
  
  
}


