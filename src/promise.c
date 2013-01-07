#include <R.h>
#include <Rinternals.h>

SEXP is_promise(SEXP name, SEXP env) {
  SEXP object = findVar(name, env);
  SEXP result;

  PROTECT(result = allocVector(LGLSXP, 1));
  LOGICAL(result)[0] = (TYPEOF (object) == PROMSXP);
  UNPROTECT(1);
  
  return(result);
}

SEXP promise_code(SEXP name, SEXP env) {
  SEXP object = findVar(name, env);
  return(PRCODE(object));
}
SEXP promise_value(SEXP name, SEXP env) {  
  SEXP object = findVar(name, env);
  return(PRVALUE(object));
}
SEXP promise_evaled(SEXP name, SEXP env) {
  SEXP object = findVar(name, env);
  return(ScalarLogical(PRVALUE(object) != R_UnboundValue));
}
SEXP promise_env(SEXP name, SEXP env) {
  SEXP object = findVar(name, env);
  return(PRENV(object));
}
