#include <Rcpp.h>
#include <../Lsxmain/Comsat.cpp>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
void diurnalCpp(Environment env, double time, double jday, double plens, double startp, double endp, double irrigate, double ilens, double starti, double endi) {
  // Global
  NumericVector solad = env["solad"];
  NumericVector solai = env["solai"];
  double cloud = env["cloud"];
  double coszen = env["coszen"];
  double daylength = env["daylength"];
  double dtime = env["dtime"];
  double fira = env["fira"];
  // double latindex = env["latindex"];
  double latscale = env["latscale"];
  double nband = env["nband"];
  double precip = env["precip"];
  double psurf = env["psurf"];
  double qa = env["qa"];
  double qd = env["qd"];
  double raina = env["raina"];
  double rh = env["rh"];
  double snowa = env["snowa"];
  double stef = env["stef"];
  double stinrad = env["stinrad"];
  double ta = env["ta"];
  double tmax = env["tmax"];
  double tmin = env["tmin"];
  double totirrig = env["totirrig"];
  double ua = env["ua"];
  double ud = env["ud"];
  double xirrig = env["xirrig"];
  double xirriga = env["xirriga"];
  double pi = M_PI;
  
  // Local
  double dailyrad, rtime, orbit, angle, xdecl, sw, xlat, cosz, trans, fdiffuse, gamma, qmin, qmax, qsa, fracw, emb, ea, ec;
  double dtair, dtcloud, plen, plenmin, plenmax, checkP;
  int niter;
  
  dailyrad = 0;
  
  rtime = time  / 3600;
  
  
  orbit = 2 * pi * jday / 365.2425;
  
  angle = 2 * pi * (rtime  - 12) / 24;
  
  xdecl = 0.006918 - 0.399912 * 
    cos(orbit) + 0.070257 * 
    sin(orbit) - 0.006758 * 
    cos(2 * orbit) + 0.000907 * 
    sin(2 * orbit) - 0.002697 * 
    cos(3 * orbit) + 0.001480 * 
    sin(3 * orbit);
  
  
  sw = 1370 * (1.000110 + 0.034221 * 
    cos(orbit) + 0.001280 * 
    sin(orbit) + 0.000719 * 
    cos(2 * orbit) + 0.000077 * 
    sin(2 * orbit)) ;
  
  // jj = latindex;
  
  xlat = latscale * pi / 180;
  
  coszen = std::max(0.0, (sin(xlat) * sin(xdecl) + cos(xlat) * cos(xdecl) * cos(angle)));
  
  daylength = (180 / pi) * ((2 * 60) / 15) * (acos((coszen - (sin(xlat) * sin(xdecl))) / (cos(xlat) * cos(xdecl))));
  
  if(NumericVector::is_na(daylength)) {
    daylength = 0;
  }  
  
  cosz  = 0;
  trans = 0;
  if(time == 0) {
    
    double ij = ((24 * 3600 / dtime) - 1);
    
    for(int j = 0; j < ij; ++j) { 
      
      cosz =  std::max(0.0, (sin(xlat) * sin(xdecl) + cos(xlat) * cos(xdecl) * cos( (2 * pi * (j - 12) / 24) )));
      
      trans = 0.251 + 0.509;
      
      dailyrad = dailyrad + (sw * cosz * trans) * (3600 / pow(10, 6));
    }
    
    if(stinrad >= 0) {
      cloud = 0.76 * (1 - (stinrad / dailyrad)) / 0.509;
      cloud = std::max(0.0, std::min(cloud,1.0));
      if(jday == 1)
        Rprintf("be sure that is reading solar radiation (MJ / m2day) stinrad \n");
    }
    
    dailyrad = 0;
  }
  
  trans = 0.251 + 0.509 * (1 - cloud); 
  
  fdiffuse = 1.0045 + 0.0435 * trans - 3.5227 * pow(trans, 2) + 2.6313 * pow(trans, 3);
  
  if (trans > 0.75) 
    fdiffuse = 0.166;
  
  for(int ib = 0; ib < nband; ib++) { 
    fracw = 0.46 + 0.08 * (ib);
    
    solad[ib] = sw * coszen * fracw * trans * (1 - fdiffuse);
    
    solai[ib] = sw * coszen * fracw * trans * fdiffuse;
  }
  
  gamma = 0.44 - 0.46 * sin (pi / 12 * rtime  + 0.9) + 0.11 * sin (2 * pi / 12 * rtime  + 0.9);
  
  ta = tmax * gamma + tmin * (1 - gamma);
  qmin = std::min(qd, 0.99 * cpp_qsat(cpp_esat(tmin), psurf));
  qmax = (qd - 0.56 * qmin) / 0.44;
  
  qsa = 0.99 * cpp_qsat(cpp_esat(ta), psurf);
  
  qa = std::min(qsa, qmax * gamma + qmin * (1 - gamma));
  
  rh = 100 * qa / cpp_qsat(cpp_esat(ta), psurf);
  
  ua = 1.13989 * ud * pow((-log(0.5)), 0.30 );
  
  ua = std::max(0.2, std::min(10.0, ua));
  
  emb = 0.01 * (psurf * qa / (0.622 + qa));
  ea = 0.70 + 5.95e-5 * emb * std::exp(1500 / ta);
  
  ec = 0.950;

    dtair = 2;
  dtcloud = 2;
  
  fira = (1 - cloud) * ea * stef * pow((ta - dtair), 4) + cloud * ec * stef * pow((ta - dtcloud), 4);
  
  do {
    
    snowa = 0;
    raina = 0;
    
    niter = (86400 / dtime);
    
    
    if(time  < dtime) {
      plen = plens / dtime;
      plenmin = 1 + (4 * 3600 - 1) / dtime;
      plenmax = std::max((24 * 3600 / dtime), plenmin);
      checkP = 0;
      
      while(precip / plen > 95 && plen < plenmax) {
        plen = plen + 1;
        checkP = 1;
      }
      
      if(checkP == 1) {
        plens = dtime  * plen;
        startp = dtime  * std::min(niter - plen, (0.5 * (niter - plen + 1)));
        endp = startp + plen * dtime;
      } else {
        break;
      }
    } else {
      break;
    }
  } while (1==1);
  
  if(time  >= startp && time  < endp) {  
    if(ta - 273.15 > 2.5) {
      raina = precip / plens;
    } else {
      snowa = precip / plens;
    }
  }
  
  xirriga = 0;
  
  if(time  >= starti && time  < endi &&  
     irrigate  == 1 && 
     precip == 0) {  
    
    xirriga = xirrig / ilens;
    
    totirrig = totirrig + (xirriga * dtime);
  } 
  
  if(NumericVector::is_na(daylength)) {
    daylength = 0;
  }  
  
  env.assign("coszen", coszen);
  env.assign("daylength", daylength);
  env.assign("cloud", cloud);
  env.assign("solad", solad);
  env.assign("solai", solai);
  env.assign("ta", ta);
  env.assign("qa", qa);
  env.assign("rh", rh);
  env.assign("ua", ua);
  env.assign("fira", fira);
  env.assign("snowa", snowa);
  env.assign("raina", raina);
  env.assign("xirriga", xirriga);
  env.assign("totirrig", totirrig);
  
}