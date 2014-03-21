#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
Environment dots2env(Environment parent){
  SEXP dots = parent.find("..." ) ;
  
  Environment res = parent.new_child(true) ;
  if( dots == R_MissingArg ) return res ;
  
  int i=0 ;
  while( dots != R_NilValue ){
    i++ ;
    SEXP name = TAG(dots) ;
    if( Rf_isNull(name) ){
      std::stringstream s ;
      s << "unnamed variable at index " << i ;
      stop( s.str() ) ;
    }
    
    // recurse until we find the real promise, not a promise of a promise
    SEXP prom = CAR(dots) ;
    while( true ){
      SEXP code = PRCODE(prom) ;
      if( TYPEOF(code) != PROMSXP ) break ;
      prom = code ;
    }
  
    // debug: 
    // Rprintf( "variable %s with type %s\n", CHAR(PRINTNAME(name)), type2name(PRCODE(prom)) ) ;
      
    Rf_defineVar( name, prom, res) ;
    
    dots = CDR(dots) ;
  }
  
  return res ;
  
}
