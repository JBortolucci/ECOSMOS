#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void novegCpp(Environment env) {
  // global 
  NumericVector sai = env["sai"];
  NumericVector lai = env["lai"];
  double fi = env["fi"];
  double fl = env["fl"];
  double fu = env["fu"];
  double tg = env["tg"];
  double ti = env["ti"];
  double tl = env["tl"];
  double ts = env["ts"];
  double tu = env["tu"];
  double wliql = env["wliql"];
  double wliqs = env["wliqs"];
  double wliqu = env["wliqu"];
  double wsnol = env["wsnol"];
  double wsnos = env["wsnos"];
  double wsnou = env["wsnou"];
  // local
  double tav;
  double x;
  double y;
  
  tav = (1 - fi) * tg + fi * ti;
  
  if ((lai[1] == 0) | (fu == 0)) {
    tu = tav;
    wliqu = 0;
    wsnou = 0;
  }
  
  if ((sai[1] == 0) | (fu == 0)) {
    ts = tav;
    wliqs = 0;
    wsnos = 0;
  }
  
  x = 2.0 * (lai[0] + sai[0]);
  y = fl * (1 - fi);
  
  if ((x == 0) | (y == 0)) {
    tl = tav;
    wliql = 0;
    wsnol = 0;
  }
  
  env.assign("tu", tu);
  env.assign("wliqu", wliqu);
  env.assign("wsnou", wsnou);
  env.assign("ts", ts);
  env.assign("wliqs", wliqs);
  env.assign("wsnos", wsnos);
  env.assign("tl", tl);
  env.assign("wliql", wliql);
  env.assign("wsnol", wsnol);
}