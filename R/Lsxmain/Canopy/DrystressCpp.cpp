#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void drystressCpp(Environment env, double jday, double time_param) {
  // global
  double nsoilay = env["nsoilay"];
  NumericMatrix froot = env["froot"];
  NumericVector sfield = env["sfield"];
  NumericVector stressl = env["stressl"];
  double stresstl = env["stresstl"];
  double stresstu = env["stresstu"];
  NumericVector stressu = env["stressu"];
  NumericVector swilt = env["swilt"];
  NumericVector wisoi = env["wisoi"];
  NumericVector wsoi = env["wsoi"];
  NumericVector stressBeta1 = env["stressBeta1"];
  NumericVector stressBeta0 = env["stressBeta0"];
  // local
  double awc;
  double zwilt;
  
  stresstl = 0.0;
  stresstu = 0.0;
  
  for (int k = 0; k < nsoilay ; k++) {
    
    awc = std::min(1.0, std::max(0.0, (wsoi[k]*(1 - wisoi[k]) - swilt[k]) / (sfield[k] - swilt[k])));
    
    //stressBeta0 = -20;
    //stressBeta1 = 900;
    
    zwilt = 1.0 - (std::log(1 + stressBeta1[0] * std::exp(stressBeta0[0] * awc)) / std::log(stressBeta1[0]));
    
    stressl[k] = froot(k , 0) * std::max(0.0, std::min(1.0, zwilt));
    stressu[k] = froot(k , 1) * std::max(0.0, std::min(1.0, zwilt));
    
    stresstl = stresstl + stressl[k];
    
    stresstu = stresstu + stressu[k];
    
  }
  
  env.assign("stresstl", stresstl);
  env.assign("stresstu", stresstu);
  env.assign("stressl", stressl);
  env.assign("stressu", stressu);
}
