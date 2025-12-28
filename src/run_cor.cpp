//run_cor.cpp
//Function evaluates running correlation of input data
//
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(".run_cor")]]
NumericVector run_cor(NumericVector x, NumericVector y, int k) {
  
  // define length of x
  int n=x.size();
  
  // define output data set
  NumericVector z(n);
  
  // define counter index
  IntegerVector idx = seq_len(k) - 1;
  NumericVector x_i(idx);
  NumericVector y_i(idx);
    
  for (int i = 1; i < (n - k); i++) {
    
    // extract subset for calculation
    x_i=x[idx+i];
    y_i=y[idx+i];
    
    // calculate subset's mean and sd
    double m_x_i = mean(x_i);
    double m_y_i = mean(y_i);
    double s_x_i = sd(x_i);
    double s_y_i = sd(y_i);
    
    // calculate and assign Pearson's correlation coefficient
    z[i]= sum(((x_i - m_x_i) / s_x_i) * ((y_i - m_y_i) / s_y_i)) / k;
  }

  // return output
  return z;
}