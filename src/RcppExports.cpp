// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcppDigitalNetPoints
NumericMatrix rcppDigitalNetPoints(DataFrame df, int id, int dimR, int dimF2, uint64_t count, bool digitalShift);
RcppExport SEXP rmcqmcint_rcppDigitalNetPoints(SEXP dfSEXP, SEXP idSEXP, SEXP dimRSEXP, SEXP dimF2SEXP, SEXP countSEXP, SEXP digitalShiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< int >::type id(idSEXP);
    Rcpp::traits::input_parameter< int >::type dimR(dimRSEXP);
    Rcpp::traits::input_parameter< int >::type dimF2(dimF2SEXP);
    Rcpp::traits::input_parameter< uint64_t >::type count(countSEXP);
    Rcpp::traits::input_parameter< bool >::type digitalShift(digitalShiftSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppDigitalNetPoints(df, id, dimR, dimF2, count, digitalShift));
    return rcpp_result_gen;
END_RCPP
}
// rcppQMCIntegration
List rcppQMCIntegration(Function integrand, uint32_t N, DataFrame df, int id, int s, int m, double probability);
RcppExport SEXP rmcqmcint_rcppQMCIntegration(SEXP integrandSEXP, SEXP NSEXP, SEXP dfSEXP, SEXP idSEXP, SEXP sSEXP, SEXP mSEXP, SEXP probabilitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Function >::type integrand(integrandSEXP);
    Rcpp::traits::input_parameter< uint32_t >::type N(NSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< int >::type id(idSEXP);
    Rcpp::traits::input_parameter< int >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type probability(probabilitySEXP);
    rcpp_result_gen = Rcpp::wrap(rcppQMCIntegration(integrand, N, df, id, s, m, probability));
    return rcpp_result_gen;
END_RCPP
}
// rcppMCIntegration
List rcppMCIntegration(Function integrand, uint32_t N, int s, int m, double probability);
RcppExport SEXP rmcqmcint_rcppMCIntegration(SEXP integrandSEXP, SEXP NSEXP, SEXP sSEXP, SEXP mSEXP, SEXP probabilitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Function >::type integrand(integrandSEXP);
    Rcpp::traits::input_parameter< uint32_t >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type probability(probabilitySEXP);
    rcpp_result_gen = Rcpp::wrap(rcppMCIntegration(integrand, N, s, m, probability));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"rmcqmcint_rcppDigitalNetPoints", (DL_FUNC) &rmcqmcint_rcppDigitalNetPoints, 6},
    {"rmcqmcint_rcppQMCIntegration", (DL_FUNC) &rmcqmcint_rcppQMCIntegration, 7},
    {"rmcqmcint_rcppMCIntegration", (DL_FUNC) &rmcqmcint_rcppMCIntegration, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_rmcqmcint(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}