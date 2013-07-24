#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool is_promise2(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);

  return (TYPEOF (object) == PROMSXP);
}

// [[Rcpp::export]]
SEXP promise_code(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  return PRCODE(object);
}
// [[Rcpp::export]]
SEXP promise_value(Symbol name, Environment env) {  
  SEXP object = Rf_findVar(name, env);
  return PRVALUE(object);
}
// [[Rcpp::export]]
bool promise_evaled(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  return PRVALUE(object) != R_UnboundValue;
}
// [[Rcpp::export]]
SEXP promise_env(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  return PRENV(object);
}
