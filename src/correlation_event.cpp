//correlation_event.cpp
//Function evaluates correlation of input data
//
  #include <Rcpp.h>
  
  using namespace Rcpp;

// [[Rcpp::export(".correlation_event")]]
NumericVector correlation_event(NumericVector x, NumericVector y) {
  
  // define length of x
  int n=x.size();
  
  // define length of y, i.e. template window size
  int w=y.size();
  
  // define output data set
  NumericVector z(n);
  
  // define counter index
  IntegerVector idx=seq_len(w)-1;
  
  // calculate template mean and sd
  double y_m = mean(y);
  double y_v = sd(y);
  
  // loop through data set
  for (int i=1; i<(n-w-1); i++) {
    
    // extract subset for calculation
    NumericVector s=x[idx+i];
    
    // calculate subset mean and sd
    double s_m = mean(s);
    double s_v = sd(s);
    
    // calculate and assign Pearson's correlation coefficient
    z[i]= sum(s * y - w * s_m * y_m) / ((w - 1) * s_v * y_v);
  }
  
  // return output
  return z;
}