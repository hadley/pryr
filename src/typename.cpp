// Modified from src/main/inspect.C
//
// Copyright (C) 2009-2012 The R Core Team.
// Copyright (C) 2013 Hadley Wickham
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// A copy of the GNU General Public License is available at
// http://www.r-project.org/Licenses/

#include <Rcpp.h>
using namespace Rcpp;

//' @export
//' @rdname inspect
// [[Rcpp::export]]
std::string sexp_type(SEXP x) {
  switch (TYPEOF(x)) {
    case NILSXP:  return "NILSXP";
    case SYMSXP:  return "SYMSXP";
    case LISTSXP: return "LISTSXP";
    case CLOSXP:  return "CLOSXP";
    case ENVSXP:  return "ENVSXP";
    case PROMSXP: return "PROMSXP";
    case LANGSXP: return "LANGSXP";
    case SPECIALSXP:  return "SPECIALSXP";
    case BUILTINSXP:  return "BUILTINSXP";
    case CHARSXP: return "CHARSXP";
    case LGLSXP:  return "LGLSXP";
    case INTSXP:  return "INTSXP";
    case REALSXP: return "REALSXP";
    case CPLXSXP: return "CPLXSXP";
    case STRSXP:  return "STRSXP";
    case DOTSXP:  return "DOTSXP";
    case ANYSXP:  return "ANYSXP";
    case VECSXP:  return "VECSXP";
    case EXPRSXP: return "EXPRSXP";
    case BCODESXP:  return "BCODESXP";
    case EXTPTRSXP: return "EXTPTRSXP";
    case WEAKREFSXP:  return "WEAKREFSXP";
    case S4SXP:   return "S4SXP";
    case RAWSXP:  return "RAWSXP";
    default:   return "<unknown>";
  }
}

// [[Rcpp::export]]
std::string typename2(Symbol name, Environment env) {
  SEXP object = Rf_findVar(name, env);
  return sexp_type(object);
}
