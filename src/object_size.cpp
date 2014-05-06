#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double v_size(double n, int size) {
  if (n == 0) return 8;

  int vec_size = std::max(sizeof(SEXP), sizeof(double));
  int elements_per_byte = vec_size / size;
  double n_bytes = ceil(n / elements_per_byte);
  // Rcout << n << " elements, each of " << elements_per_byte << " = " <<
  //  n_bytes << "\n";

  double bytes = 0;
  // Big vectors always allocated in 8 byte chunks
  if      (n_bytes > 16) bytes = n_bytes * 8;
  // For small vectors, round to sizes allocated in small vector pool
  else if (n_bytes > 8)  bytes = 128;
  else if (n_bytes > 6)  bytes = 64;
	else if (n_bytes > 4)  bytes = 48;
	else if (n_bytes > 2)  bytes = 32;
	else if (n_bytes > 1)  bytes = 16;
	else if (n_bytes > 0)  bytes = 8;

  return bytes +
    vec_size; // SEXPREC_ALIGN padding
}


double object_size_rec(SEXP x, std::set<SEXP>& seen) {
  //  NILSXP is a singleton, so occupies no space
  if (TYPEOF(x) == NILSXP) return 0;

  // If we've seen it before already, don't count it again
  if (!seen.insert(x).second) return 0;

  // As of R 3.1.0, all SEXPRECs start with sxpinfo (4 bytes + 4 bytes padding
  // automatically added by compiler), pointer  to attribute pairlist (8 bytes),
  // then two pointers to manage doubly linked list of all objects in
  // memory (2 * 8 bytes)
  double size = 4 * 8;

  switch (TYPEOF(x)) {
    // Simple vectors
    case LGLSXP:
    case INTSXP:
      size += v_size(XLENGTH(x), sizeof(int));
      size += object_size_rec(ATTRIB(x), seen);
      break;
    case REALSXP:
      size += v_size(XLENGTH(x), sizeof(double));
      size += object_size_rec(ATTRIB(x), seen);
      break;
    case CPLXSXP:
      size += v_size(XLENGTH(x), sizeof(Rcomplex));
      size += object_size_rec(ATTRIB(x), seen);
      break;
    case RAWSXP:
      size += v_size(XLENGTH(x), 1);
      size += object_size_rec(ATTRIB(x), seen);
      break;

    // Strings
    case STRSXP:
      size += v_size(XLENGTH(x), 1);
    	for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
  	    size += object_size_rec(STRING_ELT(x, i), seen);
    	}
      size += object_size_rec(ATTRIB(x), seen);
    	break;
    case CHARSXP:
      size += v_size(LENGTH(x) / 2 + 1, 1);
      break;

    // Generic vectors
    case VECSXP:
    case EXPRSXP:
    case WEAKREFSXP:
  	  size += v_size(XLENGTH(x), sizeof(SEXP));
    	for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
  	    size += object_size_rec(VECTOR_ELT(x, i), seen);
    	}
      size += object_size_rec(ATTRIB(x), seen);
    	break;

    // Linked lists
    case LISTSXP:
    case LANGSXP:
    case BCODESXP:
      size += 3 * sizeof(SEXP); // tag, car, cdr
      size += object_size_rec(TAG(x), seen); // name of first element
    	size += object_size_rec(CAR(x), seen); // first element
    	size += object_size_rec(CDR(x), seen); // pairlist (subsequent elements) or NILSXP
    	break;

    // Functions
    case CLOSXP:
      size += 3 * sizeof(SEXP); // formals, body, env
      size += object_size_rec(FORMALS(x), seen);
	    size += object_size_rec(BODY(x), seen);
      // size += object_size(CLOENV(s));
	    break;

    case SYMSXP:
      size += 3 * sizeof(SEXP); // pname, value, internal
      break;

    default:
      stop("Unimplemented type");
  }

  // Rcout << "type: " << TYPEOF(x) << " size: " << size << "\n";
  return size;
}

// [[Rcpp::export]]
double object_size(SEXP x) {
  std::set<SEXP> seen;

  return object_size_rec(x, seen);
}

// [[Rcpp::export]]
double object_size_(Symbol name, Environment env) {
  SEXP x = Rf_findVar(name, env);

  return object_size(x);
}
