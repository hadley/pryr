#include <Rcpp.h>
using namespace Rcpp;

std::string slice(std::string const& x, int k, std::string const& sep = " ") {
  std::string output;
  int size = x.size();
  int nSlices = size / k;
  output.reserve(size + nSlices * sep.size() - 1);
  for (int i=0; i < nSlices - 1; ++i) {
    output += x.substr(i * k, k);
    output += sep;
  }
  output += x.substr(size - k, k);
  return output;
}

// [[Rcpp::export]]
CharacterVector slice(CharacterVector x, int k, std::string sep = " ") {
  int n = x.size();
  CharacterVector output = no_init(n);
  for (int i=0; i < n; ++i) {
    output[i] = slice( as<std::string>(x[i]), k, sep );
  }
  return output;
}
