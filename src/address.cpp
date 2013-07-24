#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string address2(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  std::ostringstream s;  
  s << object;
  return s.str();
}
