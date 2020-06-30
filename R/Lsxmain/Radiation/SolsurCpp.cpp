#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void solsurCpp(Environment env, int ibb) {
  // global 
  NumericMatrix tsno = env["tsno"];
  NumericMatrix wisoi = env["wisoi"];
  NumericMatrix wsoi = env["wsoi"];
  double albsan = env["albsan"];
  double albsav = env["albsav"];
  double albsnd = env["albsnd"];
  double albsni = env["albsni"];
  double albsod = env["albsod"];
  double albsoi = env["albsoi"];
  double coszen = env["coszen"];
  double indsol = env["indsol"];
  double nsol = env["nsol"];
  double tmelt = env["tmelt"];
  // local 
  int ib = ibb - 1;
  double a7svlo, a7svhi, a7snlo, a7snhi, t7shi, t7slo, zw, dinc, i, x, zfac;
  
  a7svlo = 0.90;
  a7svhi = 0.70;
  a7snlo = 0.60;
  a7snhi = 0.40;
  
  t7shi = tmelt;
  t7slo = tmelt - 15.0;
  
  if (nsol == 0) return;
  if (ib == 0) {
    for (int j = 0; j < nsol; j++) {
      i = indsol;
      zw = wsoi(i-1 , 0) * (1. - wisoi(i-1 , 0));
      
      dinc = 1.0 + 1.0 * std::min(1., std::max(0.0, 1. - (zw /.50) ));
      
      albsod = std::min(albsav * dinc, .80);
      albsoi = albsod;
      
    }
    for (int j = 0; j < nsol; j++) {
      i = indsol;
      x = (a7svhi * (tsno(i-1 , 0) - t7slo) + a7svlo * (t7shi - tsno(i-1 , 0))) / (t7shi - t7slo);
      
      x = std::min(a7svlo, std::max(a7svhi, x));
      
      zfac = std::max( 0., 1.5 / (1.0 + 4. * coszen) - 0.5 );
      albsnd = std::min(0.99, x + (1. - x) * zfac);
      albsni = std::min(1., x);
    }
  }else{
    for (int j = 0; j < nsol; j++) {
      i = indsol;
      
      zw = wsoi(i-1 , 0) * (1. - wisoi(i-1 , 0));
      
      dinc = 1.0 + 1.0 * std::min(1., std::max(0.0, 1.0 - (zw / .50)  ));
      
      albsod = std::min(albsan * dinc, .80);
      albsoi = albsod;
    }
    for (int j = 0; j < nsol; j++) {
      i = indsol;
      
      x = (a7snhi * (tsno(i-1 , 0) - t7slo) + a7snlo * (t7shi - tsno(i-1 , 0))) / (t7shi - t7slo);
      x = std::min(a7snlo, std::max(a7snhi, x));
      
      zfac = std::max( 0., (1.5/(1. + 4. * coszen) - 0.5) );
      
      albsnd = std::min(0.99, x + (1. - x) * zfac);
      albsni = std::min(1., x);
    }
  }
  
  env.assign("albsod", albsod);
  env.assign("albsoi", albsoi);
  env.assign("albsnd", albsnd);
  env.assign("albsni", albsni);
}
