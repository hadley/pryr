#include <R.h>
#include <Rinternals.h>

SEXP substituteList(SEXP el, SEXP rho);

// Language objects (LANGSXP) are calls (including formulae and so on). 
// Internally they are pairlists with first element a reference2 to the function 
// to be called with remaining elements the actual arguments for the call (and 
// with the tags if present giving the specified argument names). Although 
// this is not enforced, many places in the code assume that the pairlist is of 
// length one or more, often without checking.
SEXP substitute2(SEXP lang, SEXP rho) {
  SEXP t;
  switch (TYPEOF(lang)) {
    case SYMSXP:
      t = findVarInFrame3(rho, lang, TRUE);
            
      if (t != R_UnboundValue) {
        return(lang);
      } else {
        return(t);
      }
    case LANGSXP:
      return substituteList(lang, rho);
    case PROMSXP:
      // No promises in this version
    default:
      return(lang);
  }
}

/* Work through a pairlist doing substitute on the
   elements taking particular care to handle '...' */
SEXP substituteList(SEXP el, SEXP rho) {
  SEXP h, p = R_NilValue, res = R_NilValue;

  if (isNull(el)) return el;

  while (el != R_NilValue) {
    /* walk along the pairlist, substituting elements.
       res is the result
       p is the current last element
       h is the element currently being processed
    */
    if (CAR(el) == R_DotsSymbol) {
      if (rho == R_NilValue) {
        h = R_UnboundValue;   /* so there is no substitution below */
      } else {
        h = findVarInFrame3(rho, CAR(el), TRUE);
      }

      if (h == R_UnboundValue) {
        h = LCONS(R_DotsSymbol, R_NilValue);
      } else if (h == R_NilValue  || h == R_MissingArg) {
        h = R_NilValue;
      } else if (TYPEOF(h) == DOTSXP) {
        h = substituteList(h, R_NilValue);
      } else {
        error("'...' used in an incorrect context");
      }
    } else {
      h = substitute2(CAR(el), rho);
      if (isLanguage(el)) {
        h = LCONS(h, R_NilValue);
      } else {
        h = CONS(h, R_NilValue);
      }
      SET_TAG(h, TAG(el));
    }
    
    if (h != R_NilValue) {
      if (res == R_NilValue) {
        PROTECT(res = h);
      } else {
        SETCDR(p, h);
      }
      /* now set 'p': dots might have expanded to a list of length > 1 */
      while (CDR(h) != R_NilValue) {
        h = CDR(h);
      }
      p = h;
    }
    el = CDR(el);
  }
  if(res != R_NilValue) {
    UNPROTECT(1);
  }
  return res;
}


