#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cpp_sign(double x){
  if(x == 0) return 0;
  return ((x > 0) ? (1) : (-1));
}

// [[Rcpp::export]]
double cpp_hvapf(Environment env, double t, double tair) {
  // global
  double hvap = env["hvap"];
  double cvap = env["cvap"];
  double ch2o = env["ch2o"];
  
  return (hvap + cvap*(tair-273.16) - ch2o*(t-273.16));
}

// [[Rcpp::export]]
double cpp_hsubf(Environment env, double t, double tair) {
  // global
  double hsub = env["hsub"];
  double cvap = env["cvap"];
  double cice = env["cice"];
  
  return (hsub + cvap*(tair-273.16) - cice*(t-273.16));
}

// [[Rcpp::export]]
double cpp_qsat(double e1, double p1) {
  return (0.622 * e1 / std::max( p1 - (1.0 - 0.622) * e1, 0.622 * e1 ));
}

// [[Rcpp::export]]
double cpp_tsatl(double t) {
  double aux1 = 100;
  double aux2 = 0;
  return std::min(aux1, std::max((t - 273.16), aux2));
}

// [[Rcpp::export]]
double cpp_tsati(double t) {
  double aux1 = -60;
  double aux2 = 0;
  return std::max(aux1, std::min((t - 273.16), aux2));
}

// [[Rcpp::export]]
double cpp_esat(double t) {
  double asat0 = 6.1078000;
  double asat1 = 4.4365185e-1;
  double asat2 = 1.4289458e-2;
  double asat3 = 2.6506485e-4;
  double asat4 = 3.0312404e-6;
  double asat5 = 2.0340809e-8;
  double asat6 = 6.1368209e-11;
  
  double bsat0 = 6.1091780;
  double bsat1 = 5.0346990e-1;
  double bsat2 = 1.8860134e-2;
  double bsat3 = 4.1762237e-4;
  double bsat4 = 5.8247203e-6;
  double bsat5 = 4.8388032e-8;
  double bsat6 = 1.8388269e-10;
  double retorno;
  
  if (t >= 273.16){
    retorno = 100 * (asat0 + cpp_tsatl(t) * (asat1 + cpp_tsatl(t) * (asat2 + cpp_tsatl(t)*(asat3 + cpp_tsatl(t) * (asat4 + cpp_tsatl(t)*(asat5 + cpp_tsatl(t)* asat6))))) + 
      cpp_tsati(t) * (bsat1 + cpp_tsati(t) * (bsat2 + cpp_tsati(t)*(bsat3 + cpp_tsati(t) * (bsat4 + cpp_tsati(t)*(bsat5 + cpp_tsati(t)* bsat6))))));
  } else {
    retorno = 100 * (bsat0 + cpp_tsatl(t) * (asat1 + cpp_tsatl(t) * (asat2 + cpp_tsatl(t)*(asat3 + cpp_tsatl(t) * (asat4 + cpp_tsatl(t)*(asat5 + cpp_tsatl(t)* asat6))))) + 
      cpp_tsati(t) * (bsat1 + cpp_tsati(t) * (bsat2 + cpp_tsati(t)*(bsat3 + cpp_tsati(t) * (bsat4 + cpp_tsati(t)*(bsat5 + cpp_tsati(t)* bsat6))))));
  }
  return retorno;
}

double cpp_desat(double t) {
  double csat0 = 4.4381000e-1;
  double csat1 = 2.8570026e-2;
  double csat2 = 7.9380540e-4;
  double csat3 = 1.2152151e-5;
  double csat4 = 1.0365614e-7;
  double csat5 = 3.5324218e-10;
  double csat6 = -7.0902448e-13;
  
  double dsat0 =  5.0303052e-1;
  double dsat1 =  3.7732550e-2;
  double dsat2 =  1.2679954e-3;
  double dsat3 =  2.4775631e-5;
  double dsat4 =  3.0056931e-7;
  double dsat5 =  2.1585425e-9;
  double dsat6 =  7.1310977e-12;
  double retorno;
  
  if (t >= 273.16) {
    retorno = 100 * (csat0 + cpp_tsatl(t) * (csat1 + cpp_tsatl(t) * (csat2 + cpp_tsatl(t) * (csat3 + cpp_tsatl(t) * (csat4 + cpp_tsatl(t) * (csat5 + cpp_tsatl(t) * csat6))))) +
      cpp_tsati(t) * (dsat1 + cpp_tsati(t) * (dsat2 + cpp_tsati(t) * (dsat3 + cpp_tsati(t) * (dsat4 + cpp_tsati(t) * (dsat5 + cpp_tsati(t) * dsat6))))));
  } else {
    retorno = 100 * (dsat0 + cpp_tsatl(t) * (csat1 + cpp_tsatl(t) * (csat2 + cpp_tsatl(t) * (csat3 + cpp_tsatl(t)*(csat4 + cpp_tsatl(t)*(csat5 + cpp_tsatl(t)* csat6))))) +
      cpp_tsati(t)*(dsat1 + cpp_tsati(t)*(dsat2 + cpp_tsati(t)*(dsat3 + cpp_tsati(t)*(dsat4 + cpp_tsati(t)*(dsat5 + cpp_tsati(t)* dsat6))))));
  }
  return retorno;
}

// [[Rcpp::export]]
double cpp_dqsat(double t, double q1) {
  return cpp_desat(t) * q1 * (1. + q1*(1./0.622 - 1.)) / cpp_esat(t);
}