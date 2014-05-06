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

bool is_package(Environment env) {
  return Rf_findVarInFrame3(env, Rf_install(".Depends"), FALSE) != R_UnboundValue;
}
bool is_namespace(Environment env) {
  return Rf_findVarInFrame3(env, Rf_install(".__NAMESPACE__."), FALSE) != R_UnboundValue;
}


double object_size_rec(SEXP x, std::set<SEXP>& seen) {
  // NILSXP is a singleton, so occupies no space. Similarly SPECIAL and
  // BUILTIN are fixed and unchanging
  if (TYPEOF(x) == NILSXP ||
      TYPEOF(x) == SPECIALSXP ||
      TYPEOF(x) == BUILTINSXP) return 0;

  // If we've seen it before in this object, don't count it again
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
      size += v_size(XLENGTH(x), 8);
      for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
        size += object_size_rec(STRING_ELT(x, i), seen);
      }
      size += object_size_rec(ATTRIB(x), seen);
      break;
    case CHARSXP:
      size += v_size(LENGTH(x) + 1, 1);
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
      size += object_size_rec(ATTRIB(x), seen);
      break;

    // Environments
    case ENVSXP:
      // Recurse through all environments, stopping at base, empty or any
      // package or namespace
      if (x == R_BaseEnv || x == R_EmptyEnv ||
          is_package(x) || is_namespace(x)) return 0;

      size += 3 * sizeof(SEXP); // frame, enclos, hashtab
      size += object_size_rec(FRAME(x), seen);
      size += object_size_rec(ENCLOS(x), seen);
      size += object_size_rec(HASHTAB(x), seen);
      size += object_size_rec(ATTRIB(x), seen);

    // Functions
    case CLOSXP:
      size += 3 * sizeof(SEXP); // formals, body, env
      size += object_size_rec(FORMALS(x), seen);
      size += object_size_rec(BODY(x), seen);
      size += object_size_rec(CLOENV(x), seen);
      size += object_size_rec(ATTRIB(x), seen);
      break;

    case PROMSXP:
      size += 3 * sizeof(SEXP); // value, expr, env
      size += object_size_rec(PRVALUE(x), seen);
      size += object_size_rec(PRCODE(x), seen);
      size += object_size_rec(PRENV(x), seen);

    case EXTPTRSXP:
      size += sizeof(void *); // the actual pointer
      size += object_size_rec(EXTPTR_PROT(x), seen);
      size += object_size_rec(EXTPTR_TAG(x), seen);
      break;

    case S4SXP:
      // Only has TAG and ATTRIB
      size += object_size_rec(TAG(x), seen);
      size += object_size_rec(ATTRIB(x), seen);
      break;

    case SYMSXP:
      size += 3 * sizeof(SEXP); // pname, value, internal
      break;

    default:
       Rcout << "type: " << TYPEOF(x);
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
