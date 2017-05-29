// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// binary_repr
CharacterVector binary_repr(SEXP x);
RcppExport SEXP pryr_binary_repr(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(binary_repr(x));
    return rcpp_result_gen;
END_RCPP
}
// hex_repr
CharacterVector hex_repr(SEXP x);
RcppExport SEXP pryr_hex_repr(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(hex_repr(x));
    return rcpp_result_gen;
END_RCPP
}
// binary2hex
CharacterVector binary2hex(CharacterVector x);
RcppExport SEXP pryr_binary2hex(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(binary2hex(x));
    return rcpp_result_gen;
END_RCPP
}
// inspect_
List inspect_(SEXP x, Environment base_env);
RcppExport SEXP pryr_inspect_(SEXP xSEXP, SEXP base_envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Environment >::type base_env(base_envSEXP);
    rcpp_result_gen = Rcpp::wrap(inspect_(x, base_env));
    return rcpp_result_gen;
END_RCPP
}
// address2
std::string address2(Symbol name, Environment env);
RcppExport SEXP pryr_address2(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(address2(name, env));
    return rcpp_result_gen;
END_RCPP
}
// named2
int named2(Symbol name, Environment env);
RcppExport SEXP pryr_named2(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(named2(name, env));
    return rcpp_result_gen;
END_RCPP
}
// v_size
double v_size(double n, int size);
RcppExport SEXP pryr_v_size(SEXP nSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(v_size(n, size));
    return rcpp_result_gen;
END_RCPP
}
// object_sizes
double object_sizes(List objects, Environment base_env);
RcppExport SEXP pryr_object_sizes(SEXP objectsSEXP, SEXP base_envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type objects(objectsSEXP);
    Rcpp::traits::input_parameter< Environment >::type base_env(base_envSEXP);
    rcpp_result_gen = Rcpp::wrap(object_sizes(objects, base_env));
    return rcpp_result_gen;
END_RCPP
}
// object_size_
double object_size_(SEXP x, Environment base_env);
RcppExport SEXP pryr_object_size_(SEXP xSEXP, SEXP base_envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Environment >::type base_env(base_envSEXP);
    rcpp_result_gen = Rcpp::wrap(object_size_(x, base_env));
    return rcpp_result_gen;
END_RCPP
}
// is_promise2
bool is_promise2(Symbol name, Environment env);
RcppExport SEXP pryr_is_promise2(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(is_promise2(name, env));
    return rcpp_result_gen;
END_RCPP
}
// promise_code
SEXP promise_code(Symbol name, Environment env);
RcppExport SEXP pryr_promise_code(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(promise_code(name, env));
    return rcpp_result_gen;
END_RCPP
}
// promise_value
SEXP promise_value(Symbol name, Environment env);
RcppExport SEXP pryr_promise_value(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(promise_value(name, env));
    return rcpp_result_gen;
END_RCPP
}
// promise_evaled
bool promise_evaled(Symbol name, Environment env);
RcppExport SEXP pryr_promise_evaled(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(promise_evaled(name, env));
    return rcpp_result_gen;
END_RCPP
}
// promise_env
SEXP promise_env(Symbol name, Environment env);
RcppExport SEXP pryr_promise_env(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(promise_env(name, env));
    return rcpp_result_gen;
END_RCPP
}
// makeExplicit
RObject makeExplicit(SEXP prom);
RcppExport SEXP pryr_makeExplicit(SEXP promSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type prom(promSEXP);
    rcpp_result_gen = Rcpp::wrap(makeExplicit(prom));
    return rcpp_result_gen;
END_RCPP
}
// explicitPromise
RObject explicitPromise(Symbol name, Environment env);
RcppExport SEXP pryr_explicitPromise(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(explicitPromise(name, env));
    return rcpp_result_gen;
END_RCPP
}
// explicitDots
std::vector<RObject> explicitDots(Environment env);
RcppExport SEXP pryr_explicitDots(SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(explicitDots(env));
    return rcpp_result_gen;
END_RCPP
}
// slice
CharacterVector slice(CharacterVector x, int k, std::string sep);
RcppExport SEXP pryr_slice(SEXP xSEXP, SEXP kSEXP, SEXP sepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< std::string >::type sep(sepSEXP);
    rcpp_result_gen = Rcpp::wrap(slice(x, k, sep));
    return rcpp_result_gen;
END_RCPP
}
// sexp_type
std::string sexp_type(SEXP x);
RcppExport SEXP pryr_sexp_type(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sexp_type(x));
    return rcpp_result_gen;
END_RCPP
}
// typename2
std::string typename2(Symbol name, Environment env);
RcppExport SEXP pryr_typename2(SEXP nameSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Symbol >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Environment >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(typename2(name, env));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"pryr_binary_repr", (DL_FUNC) &pryr_binary_repr, 1},
    {"pryr_hex_repr", (DL_FUNC) &pryr_hex_repr, 1},
    {"pryr_binary2hex", (DL_FUNC) &pryr_binary2hex, 1},
    {"pryr_inspect_", (DL_FUNC) &pryr_inspect_, 2},
    {"pryr_address2", (DL_FUNC) &pryr_address2, 2},
    {"pryr_named2", (DL_FUNC) &pryr_named2, 2},
    {"pryr_v_size", (DL_FUNC) &pryr_v_size, 2},
    {"pryr_object_sizes", (DL_FUNC) &pryr_object_sizes, 2},
    {"pryr_object_size_", (DL_FUNC) &pryr_object_size_, 2},
    {"pryr_is_promise2", (DL_FUNC) &pryr_is_promise2, 2},
    {"pryr_promise_code", (DL_FUNC) &pryr_promise_code, 2},
    {"pryr_promise_value", (DL_FUNC) &pryr_promise_value, 2},
    {"pryr_promise_evaled", (DL_FUNC) &pryr_promise_evaled, 2},
    {"pryr_promise_env", (DL_FUNC) &pryr_promise_env, 2},
    {"pryr_makeExplicit", (DL_FUNC) &pryr_makeExplicit, 1},
    {"pryr_explicitPromise", (DL_FUNC) &pryr_explicitPromise, 2},
    {"pryr_explicitDots", (DL_FUNC) &pryr_explicitDots, 1},
    {"pryr_slice", (DL_FUNC) &pryr_slice, 3},
    {"pryr_sexp_type", (DL_FUNC) &pryr_sexp_type, 1},
    {"pryr_typename2", (DL_FUNC) &pryr_typename2, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_pryr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
