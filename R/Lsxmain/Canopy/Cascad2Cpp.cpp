#include <Rcpp.h>
#include <../../Lsxmain/Comsat.cpp>
using namespace Rcpp;

List steph2o2Cpp(Environment env, double tveg, double wliq, double wsno, double fveg, double xai, double rliq, double fvapw, double cveg);

// [[Rcpp::export]]
void cascad2Cpp(Environment env) {
  // global
  NumericVector lai = env["lai"];
  NumericVector sai = env["sai"];
  double chl = env["chl"];
  double chs = env["chs"];
  double chu = env["chu"];
  double fi = env["fi"];
  double fl = env["fl"];
  double fu = env["fu"];
  double fvaplw = env["fvaplw"];
  double fvaps = env["fvaps"];
  double fvapuw = env["fvapuw"];
  double rliql = env["rliql"];
  double rliqs = env["rliqs"];
  double rliqu = env["rliqu"];
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
  double fveg;
  double xai;
  List retorno;
  
  fveg = fu;
  xai = 2.0 * lai[1];
  
  retorno = steph2o2Cpp(env,tu,wliqu,wsnou,fveg,xai,rliqu,fvapuw,chu);
  tu = retorno["tveg"];
  wliqu = retorno["wliq"];
  wsnou = retorno["wsno"];
  
  fveg = fu;
  xai = 2.0 * sai[1];
  
  retorno = steph2o2Cpp(env,ts,wliqs,wsnos,fveg,xai,rliqs,fvaps,chs);
  ts = retorno["tveg"];
  wliqs = retorno["wliq"];
  wsnos = retorno["wsno"];
  
  fveg = (1 - fi) * fl;
  xai = 2.0 * (lai[0] + sai[0]);
  
  retorno = steph2o2Cpp(env,tl,wliql,wsnol,fveg,xai,rliql,fvaplw,chl);
  tl = retorno["tveg"];
  wliql = retorno["wliq"];
  wsnol = retorno["wsno"];
  
  // double jday = env["jday"];
  // if (jday == 305){
  //   Rprintf("tu %f \n", tu);
  //   Rprintf("wliqu %f \n", wliqu);
  //   Rprintf("wsnou %f \n", wsnou);
  //   Rprintf("ts %f \n", ts);
  //   Rprintf("wliqs %f \n", wliqs);
  //   Rprintf("wsnos %f \n", wsnos);
  //   Rprintf("tl %f \n", tl);
  //   Rprintf("wliql %f \n", wliql);
  //   Rprintf("wsnol %f \n", wsnol);
  // }
  
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

// [[Rcpp::export]]
List steph2o2Cpp(Environment env, double tveg, double wliq, double wsno, double fveg, double xai, double rliq, double fvapw, double cveg) {
  // global
  double ch2o = env["ch2o"];
  double cice = env["cice"];
  double dtime = env["dtime"];
  double fsena = env["fsena"];
  double fvapa = env["fvapa"];
  double hfus = env["hfus"];
  double ta = env["ta"];
  double tmelt = env["tmelt"];
  // local
  double zm;
  double rwork;
  double chav;
  double dh;
  double dw;
  
  wliq = wliq - dtime * rliq * fvapw;
  wsno = wsno - dtime * (1. - rliq) * fvapw;
  
  if (((wliq < 0.) | (wsno < 0.)) & (fveg * xai > 0.) )  {
    zm = std::max(-wliq, 0.) * fveg * xai / dtime;
    fvapa = fvapa + zm;
    fsena = fsena - zm * cpp_hvapf(env, tveg,ta);
    wliq = std::max(wliq, 0.);
    
    zm = std::max(-wsno, 0.) * fveg * xai / dtime;
    fvapa = fvapa + zm;
    fsena = fsena - zm * cpp_hsubf(env, tveg,ta);
    wsno = std::max(wsno, 0.);
    
    env.assign("fvapa", fvapa);
    env.assign("fsena", fsena);
  }
  
  rwork = 1. / hfus;
  
  chav = cveg + ch2o * wliq + cice * wsno;
  
  if ((tveg < tmelt) & (wliq > 0.0)) {
    dh = chav * (tmelt - tveg);
    dw = std::min(wliq, std::max(0., dh * rwork));
    wliq = wliq - dw;
    wsno = wsno + dw;
    chav = cveg + ch2o * wliq + cice * wsno;
    tveg = tmelt - (dh - hfus * dw) / chav;
  }
  
  if ((tveg > tmelt) & (wsno > 0.0)) {
    dh = chav * (tveg - tmelt);
    dw = std::min(wsno, std::max(0., dh * rwork));
    wsno = wsno - dw;
    wliq = wliq + dw;
    chav = cveg + ch2o * wliq + cice * wsno;
    tveg = tmelt + (dh - hfus * dw) / chav;
  }
  
  return(List::create(Named("tveg") = tveg, Named("wliq") = wliq, Named("wsno") = wsno, Named("fveg") = fveg, Named("xai") = xai, Named("rliq") = rliq, Named("fvapw") = fvapw, Named("cveg") = cveg));
}