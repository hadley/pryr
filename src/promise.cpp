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


// [[Rcpp::export]]
RObject makeExplicit(SEXP prom) {
  if (TYPEOF(prom) != PROMSXP) {
    stop("Not a promise");
  }

  // recurse until we find the real promise, not a promise of a promise
  while(true) {
    SEXP code = PRCODE(prom);
    if(TYPEOF(code) != PROMSXP) break;
    prom = code;
  }

  SEXP args = PROTECT(Rf_lcons(PRCODE(prom), R_NilValue));
  RObject formula = Rf_lcons(Rf_install("~"), args);
  UNPROTECT(1);

  formula.attr(".Environment") = PRENV(prom);
  formula.attr("class") = "formula";

  return formula;
}

// [[Rcpp::export]]
RObject explicitPromise(Symbol name, Environment env) {
  SEXP prom = Rf_findVar(name, env);
  return makeExplicit(prom);
}

// [[Rcpp::export]]
std::vector<RObject> explicitDots(Environment env) {
  SEXP dots = env.find("...");

  std::vector<RObject> out;
  std::vector<std::string> names;

  dots = env.find("...");

  SEXP el;
  for(SEXP nxt = dots; nxt != R_NilValue; el = CAR(nxt), nxt = CDR(nxt)) {
    out.push_back(makeExplicit(el));

    SEXP name = TAG(nxt);
    if (Rf_isNull(name)) {
      names.push_back("");
    } else {
      names.push_back("");
    }
  }


  return out;
}
