#include <Rcpp.h>
#include <../Lsxmain/Comsat.cpp>

using namespace Rcpp;

// [[Rcpp::export]]
void diurnalmetCpp(Environment env, double time, double jday, double plens, double startp, double endp, double irrigate, double ilens, double starti, double endi){
  // Global
  NumericVector solad = env["solad"];
  NumericVector solai = env["solai"];
  double latscale = env["latscale"];
  double cloud = env["cloud"];
  double coszen = env["coszen"];
  double daylength = env["daylength"];
  double dtime = env["dtime"];
  double fira = env["fira"];
  // double latindex = env["latindex"];
  double nband = env["nband"];
  double precip = env["precip"];
  double psurf = env["psurf"];
  double qa = env["qa"];
  double qd = env["qd"];
  double raina = env["raina"];
  double rh = env["rh"];
  double snowa = env["snowa"];
  double stef = env["stef"];
  double ta = env["ta"];
  double totirrig = env["totirrig"];
  // double ua = env["ua"];
  double xirrig = env["xirrig"];
  double xirriga = env["xirriga"];
  double pi = M_PI;
  // local
  double rtime, orbit, angle, xdecl, sw, xlat, trans, fdiffuse, wfrac, qsa, emb, ea, ec, dtair, dtcloud, truecloud;
  // double gamma, jj;
  // int niter;
  
  rtime = time  / 3600;
  
  orbit = 2 * pi * jday / 365.2425;
  
  angle = 2 * pi * (rtime  - 12) / 24;
  
  xdecl = 0.006918 - 0.399912 * cos(     orbit) + 0.070257 * sin(        orbit) - 
    0.006758 * cos(2.0 * orbit) + 0.000907 * sin(2.0 * orbit) - 
    0.002697 * cos(3.0 * orbit) + 0.001480 * sin(3.0 * orbit);
  
  sw = 1370 * (1.000110 + 0.034221 * cos(      orbit) +  
    0.001280 * sin(      orbit) + 
    0.000719 * cos(2.0 * orbit) + 
    0.000077 * sin(2.0 * orbit));
  
  // jj = latindex;
  
  xlat = latscale * pi / 180;
  
  coszen = std::max(0.0, (sin(xlat) * sin(xdecl) + cos(xlat) * cos(xdecl) * cos(angle)));
  
  daylength = (180 / pi) * ((2 * 60) / 15) * (acos((coszen - (sin(xlat) * sin(xdecl))) / (cos(xlat) * cos(xdecl))));
  
  coszen = std::max(0.0, (sin(xlat) * sin(xdecl) + cos(xlat) * cos(xdecl) * cos(angle)));
  
  daylength = (180 / pi) * ((2 * 60) / 15) * (acos((coszen - (sin(xlat) * sin(xdecl))) / (cos(xlat) * cos(xdecl))));
  
  if(cloud > 0){
    trans = cloud / (sw * coszen);
  } else {
    trans = 0;
  } 
  trans = std::max(0.0, std::min(1.0,trans));
  
  fdiffuse = 1.0045 + 0.0435 * trans -  
    3.5227 * pow(trans, 2) + 
    2.6313 * pow(trans, 3);
  
  if (trans > 0.75) 
    fdiffuse = 0.166;
  
  for(int ib = 1; ib <= nband; ib++) { 
    wfrac = 0.46 + 0.08 * (ib - 1);
    
    solad[ib - 1] = wfrac * cloud * (1 - fdiffuse);
    solai[ib - 1] = wfrac * cloud * fdiffuse;
  }
  
  // gamma = 0.44 - 0.46 * sin ( pi / 12 * rtime  + 0.9) + 0.11 * sin (2 * pi / 12 * rtime  + 0.9);
  
  qsa = 0.99 * cpp_qsat(cpp_esat(ta), psurf);
  
  qa = std::min(qsa, qd);
  
  rh = 100 * qa / cpp_qsat(cpp_esat(ta), psurf);
  
  emb = 0.01 * (psurf * qa / (0.622 + qa));
  ea = 0.70 + 5.95e-5 * emb * exp (1500 / ta);
  
  ec = 0.950;
  
  dtair = 2.0;
  dtcloud = 2.0;
  
  truecloud = 1 - ((trans - 0.251) / 0.509);
  fira = (1 - truecloud) * ea * stef * pow((ta - dtair  ), 4) + truecloud * ec * stef * pow((ta - dtcloud), 4);
  
  snowa = 0.0;
  raina = 0.0;
  
  // niter = 86400 / dtime;
  
  if(ta - 273.15 > 2.5) {
    raina = precip / plens;
  } else {
    snowa = precip / plens;
  }
  
  xirriga = 0.0;
  
  if((time >= starti) && (time  < endi) && (irrigate  == 1) &&(precip == 0)) {  
    
    xirriga = xirrig / ilens;
    
    totirrig = totirrig + (xirriga * dtime);
    
  } 
  
  
  env.assign("coszen", coszen);
  env.assign("daylength", daylength);
  env.assign("solad", solad);
  env.assign("solai", solai);
  env.assign("qa", qa);
  env.assign("rh", rh);
  env.assign("fira", fira);
  env.assign("snowa", snowa);
  env.assign("raina", raina);
  env.assign("xirriga", xirriga);
  env.assign("totirrig", totirrig);
  
}