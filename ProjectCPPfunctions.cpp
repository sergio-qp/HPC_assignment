
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
mat basicmult(List X) {
  mat out = as<arma::mat>(X[0]);
  for (int i = 1; i<X.size();i++){ //start at 1 so we can skip out 0 starting index
    out =out*as<arma::mat>(X[i]);
  }
  return out;
}

// [[Rcpp::export]]
mat simplemult(List X) {
  mat out = as<arma::mat>(X[0]) * as<arma::mat>(X[1]) * as<arma::mat>(X[2]) * as<arma::mat>(X[3]) * as<arma::mat>(X[4]);
  return out;
}

// [[Rcpp::export]]
mat evensimpler(const arma::mat A,const arma::mat B,const arma::mat C,const arma::mat D,const arma::mat E) {
  mat out = A * B * C * D * E;
  return out;
}


//vector tells us what operations to do and then we save over them, i think
//IDEA 1: subtract 1 from pvec values each time the dimensions of the total vector change
//IDEA 2: declare and redeclare a current matrix that then has multiplications applied to it
//to decide side of mult, check i value for < or >
//pvec is 2,3,1,4,5
// [[Rcpp::export]]
mat order_mult(List X, NumericVector Y) {
  
  mat out = as<arma::mat>(X[Y[0]-1]);
  for (int i = 1; i<Y.size();i++){ //1 to skip declare
    if (Y[i]-1 > Y[i-1]-1){
      out = out * as<arma::mat>(X[Y[i]-1]);
    } else {
      out = as<arma::mat>(X[Y[i]-1]) * out;
    }
  }
  
  return out;
}




