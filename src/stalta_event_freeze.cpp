//stalta_event_freeze.cpp
//Function evaluates states for sta-lta-ratio trigger
//
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(".stalta_event_freeze")]]
NumericVector stalta_event_freeze(int event_length, NumericVector data_sta, NumericVector data_lta, double on, double off) {
  
  //set variable
  NumericVector event = event_length;
  double ratio = 0;
  int T1 = 0;
  int T2 = 0;
  
  //loop
  for (int i = 0;i<event_length - 1;i++){
    
    ratio = data_sta[i] / data_lta[i];
    
    //reset
    if((ratio > on) | (T2 == 1)) {
      
      data_lta[i + 1] = data_lta[i];
      T1 = 1;
      
    }else{
      
      T1 = 0;
      
    }
    if((T1 == 1) & (ratio > off)) {
      
      T1 = 1;
      T2 = 1;
      
      event[i] = 1;
    }else{
      
      T2 = 0;
    }
  }

  return event;
}
