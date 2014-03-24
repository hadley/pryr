#include <iomanip>
#include <Rcpp.h>

using namespace Rcpp;

namespace pryr {

// traits to denote the internal C storage of an R type
namespace traits {

template <int RTYPE>
struct dataptr {
  typedef typename Rcpp::traits::storage_type<RTYPE>::type* type;
};

template <>
struct dataptr<STRSXP> {
  typedef const char* type;
};

} // namespace traits

// Declaring some types
struct Bits{};
struct Hex{};

// Utility functions
template <int RTYPE>
typename traits::dataptr<RTYPE>::type get_pointer(const Rcpp::Vector<RTYPE>& x, int i) {
  return static_cast<typename traits::dataptr<RTYPE>::type>(dataptr(x)) + i;
}

template <>
const char* get_pointer(const Rcpp::Vector<STRSXP>& x, int i) {
  return CHAR(STRING_ELT(x, i));
}

template <int RTYPE>
size_t get_size(const Rcpp::Vector<RTYPE>& x, int i) {
  return sizeof(typename ::Rcpp::traits::storage_type<RTYPE>::type);
}

template <>
size_t get_size(const Rcpp::Vector<STRSXP>& x, int i) {
  return strlen( CHAR(STRING_ELT(x, i)) );
}

// Class handling the conversion logic (from T to bits or hex)
template <typename Repr>
struct Representation {

  std::string operator()(const char* ptr, size_t n) {
    return repr(ptr, n);
  }

  std::string repr(const char* ptr, size_t n);

};

template<>
std::string Representation<Bits>::repr(const char* ptr, size_t n) {
  size_t nBits = n * 8;

  char output[nBits + 1];
  output[nBits] = '\0';

  int counter = nBits - 1;
  for (int i=0; i < n; ++i) {
    char curr = ptr[i];
    for (int j=0; j < 8; ++j) {
      output[counter--] = curr & 1 ? '1' : '0';
      curr >>= 1;
    }
  }
  return std::string(output);
}

template<>
std::string Representation<Hex>::repr(const char* ptr, size_t n) {
  size_t nout = n * 2;

  char output[nout + 1];
  output[nout] = '\0';

  int counter = 0;
  for (int i = n - 1; i >= 0; --i) {
    sprintf(output + counter * 2, "%02X", ptr[i] & 0xFF);
    ++counter;
  }
  return std::string(output);
}

template <int RTYPE, typename Representation>
std::vector<std::string> representation(const Vector<RTYPE>& x, Representation as) {
  typedef typename traits::dataptr<RTYPE>::type storage_t;
  int n = x.size();
  std::vector<std::string> output;
  output.reserve(n);
  for (int i=0; i < n; ++i) {
    const char* ptr = reinterpret_cast<const char*>(get_pointer(x, i));
    size_t size = get_size(x, i);
    output.push_back( as(ptr, size) );
  }
  return output;
}

} // namespace pryr

using namespace pryr;

// [[Rcpp::export]]
std::vector<std::string> binary_repr(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return representation<INTSXP>(x, Representation<Bits>());
  case REALSXP: return representation<REALSXP>(x, Representation<Bits>());
  case LGLSXP: return representation<LGLSXP>(x, Representation<Bits>());
  case STRSXP: return representation<STRSXP>(x, Representation<Bits>());
  default: {
    std::stringstream ss;
    ss << "can't print binary representation for objects of type '" <<
      CHAR(Rf_type2str(TYPEOF(x))) << "'";
    stop(ss.str());
  }
  }
  return std::vector< std::string >(0);
}

// [[Rcpp::export]]
std::vector<std::string> hex_repr(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return representation<INTSXP>(x, Representation<Hex>());
  case REALSXP: return representation<REALSXP>(x, Representation<Hex>());
  case LGLSXP: return representation<LGLSXP>(x, Representation<Hex>());
  case STRSXP: return representation<STRSXP>(x, Representation<Hex>());
  default: {
    std::stringstream ss;
    ss << "can't print binary representation for objects of type '" <<
      CHAR(Rf_type2str(TYPEOF(x))) << "'";
    stop(ss.str());
  }
  }
  return std::vector< std::string >(0);
}

namespace pryr {

std::string binary2hex(const std::string& x) {
  int n = x.size();
  if (n % 8 != 0) {
    stop("expecting a string of length 8n");
  }
  std::stringstream output;
  int nBytes = n / 8;
  for (int i=0; i < nBytes; ++i) {
    char curr;
    int value = 0;
    for (int j=0; j < 8; ++j) {
      curr = x[i*8 + j];
      if (!(curr == '0' or curr == '1')) stop("each character must be '0' or '1'");
      if (curr == '1') value += 1 << (7 - j);
    }
    std::stringstream ss;
    ss << std::setfill('0') << std::setw(2) << std::uppercase << std::hex << (int) value;
    output << ss.str();
  }
  return output.str();
}

} // namespace pryr

// [[Rcpp::export]]
CharacterVector binary2hex(CharacterVector x) {
  int n = x.size();
  CharacterVector output = no_init(n);
  for (int i=0; i < n; ++i) {
    output[i] = binary2hex( as<std::string>(x[i]) );
  }
  return output;
}
