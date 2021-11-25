//kurtosis_event.cpp
//Function evaluates kurtosis of input data
//
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(".kurtosis_event")]]
NumericVector kurtosis_event(NumericVector x, int k) {
  
  int n = x.size();
  
  NumericVector kurt(n);
  
  IntegerVector idx = seq_len(k) - 1;
  
  for (int i = 1; i < (n - k); i++) {
    
    NumericVector y = x[idx + i - 1];
    double m = mean(y);
    double s = sd(y);
    
    kurt[i] = sum(pow(((y - m) / s), 4)) / k;
  }
  
  return kurt;
}



