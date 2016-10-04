#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool isDfNonNegative(DataFrame df) {
  NumericVector w = as<NumericVector>(df["w"]);
  NumericVector v = as<NumericVector>(df["v"]);
  
  for (int i = 0; i < w.size(); i++) {
    if (w[i] < 0 || v[i] < 0) {
      return false;
    }
  }
  
  return true;
}
