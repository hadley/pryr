#include <Rcpp.h>
using namespace Rcpp;

bool is_namespace(Environment env);
std::string sexp_type(SEXP x);

std::string address(SEXP x) {
  std::ostringstream s;
  s << x;
  return s.str();
}


List inspect_rec(SEXP x, Environment base_env, std::set<SEXP>& seen) {
  // If we've seen it before, return nothing
  if (!seen.insert(x).second) {
    List out =  List::create(
      _["address"] = address(x),
      _["type"] = sexp_type(x),
      _["named"] = NAMED(x),
      _["seen"] = true
    );
    std::vector<std::string> klass;
    klass.push_back("inspect_" + sexp_type(x));
    klass.push_back("inspect");
    out.attr("class") = klass;
    return out;
  }

  List children;

  switch (TYPEOF(x)) {
    // Base case: non recursive objects
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
    case CHARSXP:
    case SYMSXP:
    case NILSXP:
    case SPECIALSXP:
    case BUILTINSXP:
      break;

    // Strings
    case STRSXP:
      children = List(LENGTH(x));
      for (int i = 0; i < LENGTH(x); i++) {
        children[i] = inspect_rec(STRING_ELT(x, i), base_env, seen);
      }
      break;

    // Generic vectors
    case VECSXP:
    case EXPRSXP:
    case WEAKREFSXP:
      children = List(XLENGTH(x));
      for (int i = 0; i < LENGTH(x); i++) {
        children[i] = inspect_rec(VECTOR_ELT(x, i), base_env, seen);
      }
      break;

    // Linked lists
    case LISTSXP:
    case LANGSXP:
    case BCODESXP:
      children = List::create(
        _["tag"] = inspect_rec(TAG(x), base_env, seen), // name of first element
        _["car"] = inspect_rec(CAR(x), base_env, seen), // first element
        _["cdr"] = inspect_rec(CDR(x), base_env, seen) // pairlist (subsequent elements) or NILSXP
      );
      break;

    // Environments
    case ENVSXP:
      if (x == R_BaseEnv || x == R_GlobalEnv || x == R_EmptyEnv ||
          x == base_env || is_namespace(x)) break;

      children = List::create(
        _["frame"] = inspect_rec(FRAME(x), base_env, seen),
        _["enclos"] = inspect_rec(ENCLOS(x), base_env, seen),
        _["hashtab"] = inspect_rec(HASHTAB(x), base_env, seen)
      );
      break;

    // Functions
    case CLOSXP:
      children = List::create(
        _["formals"] = inspect_rec(FORMALS(x), base_env, seen),
        _["body"] = inspect_rec(BODY(x), base_env, seen),
        _["env"]  = inspect_rec(CLOENV(x), base_env, seen)
      );
      break;

    case PROMSXP:
      children = List::create(
        _["value"] = inspect_rec(PRVALUE(x), base_env, seen),
        _["code"] = inspect_rec(PRCODE(x), base_env, seen),
        _["env"]  = inspect_rec(PRENV(x), base_env, seen)
      );
      break;

    case EXTPTRSXP:
      children = List::create(
        _["prot"] = inspect_rec(EXTPTR_PROT(x), base_env, seen),
        _["tag"] = inspect_rec(EXTPTR_TAG(x), base_env, seen)
      );
      break;

    case S4SXP:
      children = List::create(
        _["tag"] = inspect_rec(TAG(x), base_env, seen)
      );
      break;

    default:
      Rcout << "type: " << TYPEOF(x);
      stop("Unimplemented type");
  }


  List out = List::create(
    _["address"] = address(x),
    _["type"] = sexp_type(x),
    _["named"] = NAMED(x),
    _["seen"] = false
  );

  if (ATTRIB(x) != R_NilValue) {
    children["attributes"] = inspect_rec(ATTRIB(x), base_env, seen);
  }
  if (children.size() > 0) {
    out["children"] = children;
  }

  std::vector<std::string> klass;
  klass.push_back("inspect_" + sexp_type(x));
  klass.push_back("inspect");
  out.attr("class") = klass;

  return out;
}

// [[Rcpp::export]]
List inspect_(SEXP x, Environment base_env) {
  std::set<SEXP> seen;

  return inspect_rec(x, base_env, seen);
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
