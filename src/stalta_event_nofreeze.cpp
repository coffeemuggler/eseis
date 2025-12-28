//stalta_event_nofreeze.cpp
//Function evaluates states for sta-lta-ratio trigger
//
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(".stalta_event_nofreeze")]]
NumericVector stalta_event_nofreeze(int event_length, NumericVector ratio, double on, double off) {
  
  //set variable
  NumericVector event = event_length;
  int T1 = 0;
  int T2 = 0;
  
  //loop
  for (int i = 0;i<event_length - 1;i++){

    //reset
    if((ratio[i] > on) | (T2 == 1)) {
      
      T1 = 1;
      
    }else{
      
      T1 = 0;
      
    }
    if((T1 == 1) & (ratio[i] > off)) {
      
      T1 = 1;
      T2 = 1;
      
      event[i] = 1;
    }else{
      
      T2 = 0;
    }
  }

  return event;
}