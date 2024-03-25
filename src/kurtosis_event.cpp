//kurtosis_event.cpp
//Function evaluates kurtosis of input data
//
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(".kurtosis_event")]]
NumericVector kurtosis_event(NumericVector x, int k) {
  
  int n = x.size();
  double m = 0;
  double s = 0;
  
  NumericVector kurt(n);
  IntegerVector idx = seq_len(k) - 1;
  NumericVector y(idx);
  
  for (int i = 1; i < (n - k); i++) {
    
    y = x[idx + i - 1];
    m = mean(y);
    s = sd(y);
    
    kurt[i] = sum(pow(((y - m) / s), 4)) / k;
  }
  
  return kurt;
}



