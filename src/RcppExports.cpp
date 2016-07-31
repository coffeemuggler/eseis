// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// stalta_event_freeze
NumericVector stalta_event_freeze(int event_length, NumericVector data_sta, NumericVector data_lta, double on, double off);
RcppExport SEXP eseis_stalta_event_freeze(SEXP event_lengthSEXP, SEXP data_staSEXP, SEXP data_ltaSEXP, SEXP onSEXP, SEXP offSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type event_length(event_lengthSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type data_sta(data_staSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type data_lta(data_ltaSEXP);
    Rcpp::traits::input_parameter< double >::type on(onSEXP);
    Rcpp::traits::input_parameter< double >::type off(offSEXP);
    __result = Rcpp::wrap(stalta_event_freeze(event_length, data_sta, data_lta, on, off));
    return __result;
END_RCPP
}
// stalta_event_nofreeze
NumericVector stalta_event_nofreeze(int event_length, NumericVector ratio, double on, double off);
RcppExport SEXP eseis_stalta_event_nofreeze(SEXP event_lengthSEXP, SEXP ratioSEXP, SEXP onSEXP, SEXP offSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type event_length(event_lengthSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ratio(ratioSEXP);
    Rcpp::traits::input_parameter< double >::type on(onSEXP);
    Rcpp::traits::input_parameter< double >::type off(offSEXP);
    __result = Rcpp::wrap(stalta_event_nofreeze(event_length, ratio, on, off));
    return __result;
END_RCPP
}