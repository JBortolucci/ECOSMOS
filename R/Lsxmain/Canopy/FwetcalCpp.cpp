#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void fwetcalCpp(Environment env) {
  double fmax;
  double xliq;
  double xtot;
  double fwets = env["fwets"];
  double wliqu = env["wliqu"];
  double wliqumax = env["wliqumax"];
  double wsnou = env["wsnou"];
  double wsnoumax = env["wsnoumax"];
  double fwetu = env["fwetu"];
  double rliqu = env["rliqu"];
  double epsilon = env["epsilon"];
  double tu = env["tu"];
  double tmelt = env["tmelt"];
  double wliqs = env["wliqs"];
  double wliqsmax = env["wliqsmax"];
  double wsnos = env["wsnos"];
  double wsnosmax = env["wsnosmax"];
  double rliqs = env["rliqs"];
  double ts = env["ts"];
  double wliql = env["wliql"];
  double wliqlmax = env["wliqlmax"];
  double wsnol = env["wsnol"];
  double wsnolmax = env["wsnolmax"];
  double fwetl = env["fwetl"];
  double rliql = env["rliql"];
  double tl = env["tl"];
  
  fmax = 0.08;
  
  // upper leaves
  xliq = wliqu / std::max(wliqumax, 0.01);
  xtot = xliq + wsnou / std::max(wsnoumax, 0.01);
  
  fwetu = std::min(fmax, xtot);
  
  rliqu = xliq / std::max(xtot, epsilon);
  
  if (fwetu == 0.0) {
    rliqu = 1.0;
    if (tu < tmelt) {
      rliqu = 0.0;
    }
  }
  
  // upper stems
  
  xliq = wliqs / std::max(wliqsmax, 0.01);
  xtot = xliq + wsnos / std::max(wsnosmax, 0.01);
  
  fwets = std::min(fmax, xtot);
  rliqs = xliq / std::max(xtot, epsilon);
  
  if (fwets == 0.0) {
    rliqs = 1.0;
    if (ts < tmelt) 
      rliqs = 0.0;
  }
  
  // lower veg
  
  xliq = wliql / std::max(wliqlmax, 0.01);
  xtot = xliq + wsnol / std::max(wsnolmax, 0.01);
  
  fwetl = std::min(fmax, xtot);
  rliql = xliq / std::max(xtot, epsilon);
  
  if (fwetl == 0.) {
    rliql = 1.0;
    if (tl < tmelt) 
      rliql = 0.0;
  }
  
  env.assign( "fwetu" , fwetu );
  env.assign( "rliqu" , rliqu );
  env.assign( "fwets" , fwets );
  env.assign( "rliqs" , rliqs );
  env.assign( "fwetl" , fwetl );
  env.assign( "rliql" , rliql );
}
