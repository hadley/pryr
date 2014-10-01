#include <iomanip>
#include <Rcpp.h>

using namespace Rcpp;

// good enough for now, I suppose
#if defined(__sparc__) || defined(__sparc)
#define IS_BIG_ENDIAN true
#else
#define IS_BIG_ENDIAN false
#endif


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
// We store the number of characters needed to represent a single byte of data
// for a given representation
struct Bits {
  static const int chars_per_byte = 8;
};
struct Hex {
  static const int chars_per_byte = 2;
};

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
inline size_t get_length_in_bytes(const Rcpp::Vector<RTYPE>& x, int i) {
  return sizeof(typename ::Rcpp::traits::storage_type<RTYPE>::type);
}

template <>
inline size_t get_length_in_bytes(const Rcpp::Vector<STRSXP>& x, int i) {
  return strlen( CHAR(STRING_ELT(x, i)) );
}

// Class handling the conversion logic (from T to bits or hex)
template <typename Repr, bool is_string>
struct Representation {

  static const int chars_per_byte = Repr::chars_per_byte;

  inline void operator()(const char* ptr, size_t n, char* output) {
    return repr(ptr, n, output);
  }

  inline void repr(const char* ptr, size_t n, char* output);

};

// Depending on the type of data, we either want to read from left-to-right,
// or right-to-left, to give an output that matches what we might expect
// from the binary representation. In particular, we read the bits in a
// string from left to right, while we read the bits in a numeric value
// from right to left (endianness; TODO is to handle that in the dispatch
// later on)
template <>
void Representation<Bits, false>::repr(const char* ptr, size_t n, char* output) {
  int counter = n * 8 - 1;
  for (size_t i=0; i < n; ++i) {
    char curr = ptr[i];
    for (int j=0; j < 8; ++j) {
      output[counter--] = curr & 1 ? '1' : '0';
      curr >>= 1;
    }
  }
}

template<>
void Representation<Bits, true>::repr(const char* ptr, size_t n, char* output) {
  int counter = n * 8 - 1;
  for (int i = n - 1; i >= 0; --i) {
    char curr = ptr[i];
    for (int j=0; j < 8; ++j) {
      output[counter--] = curr & 1 ? '1' : '0';
      curr >>= 1;
    }
  }
}

// The hex version
template<>
void Representation<Hex, true>::repr(const char* ptr, size_t n, char* output) {
  int counter = 0;
  for (size_t i = 0; i < n; ++i) {
    sprintf(output + counter * 2, "%02X", ptr[i] & 0xFF);
    ++counter;
  }
}

template<>
void Representation<Hex, false>::repr(const char* ptr, size_t n, char* output) {
  int counter = 0;
  for (int i = n - 1; i >= 0; --i) {
    sprintf(output + counter * 2, "%02X", ptr[i] & 0xFF);
    ++counter;
  }
}

// generic version for non-STRSXP
template <int RTYPE, typename Representation>
CharacterVector representation(const Vector<RTYPE>& x, Representation fill_as) {

  int n = x.size();
  CharacterVector output = no_init(n);

  // Allocate a buffer to hold printed results
  size_t num_bytes = sizeof(typename Rcpp::traits::storage_type<RTYPE>::type);
  size_t num_chars = Representation::chars_per_byte * num_bytes;
  char* buff = new char[num_chars + 1];
  buff[num_chars] = '\0';

  // Fill the buffer and the output vector
  for (int i=0; i < n; ++i) {
    const char* ptr = reinterpret_cast<const char*>(get_pointer(x, i));
    fill_as(ptr, num_bytes, buff);
    SET_STRING_ELT(output, i, Rf_mkChar(buff));
  }

  // Clean up and return
  delete[] buff;
  return output;
}

// STRSXP
template <typename Representation>
CharacterVector representation_str(const Vector<STRSXP>& x, Representation fill_as) {

  int n = x.size();
  CharacterVector output = no_init(n);
  size_t chars_per_byte = Representation::chars_per_byte;

  for (int i=0; i < n; ++i) {
    const char* ptr = reinterpret_cast<const char*>(get_pointer(x, i));
    size_t num_bytes = get_length_in_bytes(x, i);
    size_t num_chars = chars_per_byte * num_bytes;
    char* buff = new char[num_chars + 1];
    buff[num_chars] = '\0';
    fill_as(ptr, num_bytes, buff);
    SET_STRING_ELT(output, i, Rf_mkChar(buff));
    delete[] buff;
  }

  return output;
}

} // namespace pryr

using namespace pryr;

// [[Rcpp::export]]
CharacterVector binary_repr(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return representation<INTSXP>(x, Representation<Bits, IS_BIG_ENDIAN>());
  case REALSXP: return representation<REALSXP>(x, Representation<Bits, IS_BIG_ENDIAN>());
  case LGLSXP: return representation<LGLSXP>(x, Representation<Bits, IS_BIG_ENDIAN>());
  case STRSXP: return representation_str(x, Representation<Bits, true>());
  default: {
    std::stringstream ss;
    ss << "can't print binary representation for objects of type '" <<
      CHAR(Rf_type2str(TYPEOF(x))) << "'";
    stop(ss.str());
  }
  }
  return CharacterVector();
}

// [[Rcpp::export]]
CharacterVector hex_repr(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return representation<INTSXP>(x, Representation<Hex, IS_BIG_ENDIAN>());
  case REALSXP: return representation<REALSXP>(x, Representation<Hex, IS_BIG_ENDIAN>());
  case LGLSXP: return representation<LGLSXP>(x, Representation<Hex, IS_BIG_ENDIAN>());
  case STRSXP: return representation_str(x, Representation<Hex, true>());
  default: {
    std::stringstream ss;
    ss << "can't print binary representation for objects of type '" <<
      CHAR(Rf_type2str(TYPEOF(x))) << "'";
    stop(ss.str());
  }
  }
  return CharacterVector();
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
