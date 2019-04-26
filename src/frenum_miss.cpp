#include <Rcpp.h>
using namespace Rcpp;

namespace impl {
template <int RTYPE>
Vector<RTYPE> frenum_miss(const Vector<RTYPE>& id,
                          const Vector<RTYPE>& miss) {
  Vector<RTYPE> out(id);
  if(out.inherits("factor")) {
    stop("Only numeric, integer and character vectors are supported");
  }
  LogicalVector idx = in(id, miss);
  for (int i = 0; i < out.size(); i++) {
    if (idx[i]) out[i] = Vector<RTYPE>::get_na();
  }
  return out;
}
}

//' Fast renumbering of missing id
//'
//' `frenum_miss()` will replace certain values with `NA`. For numeric vectors
//' ids coded as `0` or `-99` will be considered as missing or unknown. For
//' character vectors this is extended to include `'.'`, `'*'`, `'NA'` and `' '`.
//'
//' @param id a numeric or character vector for which the missing codes will be
//'   replaced with NA
//' @return a numeric or character vector
//'
//' @seealso [renum_miss()] which is the R implementation of the same
//'
//' @examples
//' set.seed(42L)
//' ids <- rpois(50, 100)
//' ids[sample.int(50, 10)] <- c(0L, -99)[sample.int(2, 10, replace = TRUE)]
//' frenum_miss(ids)
//'
//' ids <- c("A", "B", ".", "D", "*", "NA", " ", "  ", "K")
//' frenum_miss(ids)
//'
//' @backref src/frenum_miss.cpp
//' @export frenum_miss
// [[Rcpp::export(frenum_miss)]]
SEXP frenum_miss(SEXP id) {
  switch (TYPEOF(id)) {
  case INTSXP: {
    IntegerVector miss = IntegerVector::create(0, -99);
    return impl::frenum_miss(as<IntegerVector>(id), miss);
  }
  case REALSXP: {
    NumericVector miss = NumericVector::create(0, -99);
    return impl::frenum_miss(as<NumericVector>(id), miss);
  }
  case STRSXP: {
    CharacterVector miss = CharacterVector::create(".", "*", "NA", " ", "0", "-99");
    return impl::frenum_miss(as<CharacterVector>(id), miss);
  }
  default: {
    stop("Only numeric, integer and character vectors are supported");
  }
  }
}

// Sources used to achieve this; (TODO: blog post?)
// http://gallery.rcpp.org/articles/fast-factor-generation/
// http://gallery.rcpp.org/articles/rcpp-wrap-and-recurse/
// https://stackoverflow.com/a/19829440/4524755
// https://stackoverflow.com/questions/15953768/templated-rcpp-function-to-erase-na-values
// https://stackoverflow.com/a/37030625/4524755
// http://gallery.rcpp.org/articles/rcpp-return-macros/
// https://www.ironholds.org/multiple-types/
// https://stackoverflow.com/a/23745470/4524755
