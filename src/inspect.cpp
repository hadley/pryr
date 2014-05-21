#include <Rcpp.h>
using namespace Rcpp;

//' @export
//' @rdname inspect
// [[Rcpp::export]]
std::string address(SEXP x) {
  std::ostringstream s;
  s << x;
  return s.str();
}

// [[Rcpp::export]]
std::string address2(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  std::ostringstream s;
  s << object;
  return s.str();
}

// [[Rcpp::export]]
int named2(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  return NAMED(object);
}
