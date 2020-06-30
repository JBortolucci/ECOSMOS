#include <Rcpp.h>
using namespace Rcpp;

List fstratCpp(Environment env, double tb, double tt, double ttfac, double qb, double qt, double zb, double zt, double albm, double albh, double alt, double u, double rich, double stram, double strah, int iter);

// [[Rcpp::export]]
void turcofCpp(Environment env, int iter, double time_param, double jday) {
  // global
  NumericVector lai = env["lai"];
  NumericVector dleaf = env["dleaf"];
  NumericVector dstem = env["dstem"];
  double alog1 = env["alog1"];
  double alog2 = env["alog2"];
  double alog3 = env["alog3"];
  double alog4 = env["alog4"];
  double aloga = env["aloga"];
  double alogav = env["alogav"];
  double alogl = env["alogl"];
  double alogu = env["alogu"];
  double bdl = env["bdl"];
  double bdu = env["bdu"];
  double cgrass = env["cgrass"];
  double cl = env["cl"];
  double cleaf = env["cleaf"];
  double cstem = env["cstem"];
  double cu = env["cu"];
  double dil = env["dil"];
  double diu = env["diu"];
  double dtime = env["dtime"];
  double exphl = env["exphl"];
  double exphu = env["exphu"];
  double expl = env["expl"];
  double expu = env["expu"];
  double q12 = env["q12"];
  double q34 = env["q34"];
  double qa = env["qa"];
  double rhoa = env["rhoa"];
  double richl = env["richl"];
  double richu = env["richu"];
  double sg = env["sg"];
  double si = env["si"];
  double sl = env["sl"];
  double ss = env["ss"];
  double strahl = env["strahl"];
  double strahu = env["strahu"];
  double straml = env["straml"];
  double stramu = env["stramu"];
  double su = env["su"];
  double t12 = env["t12"];
  double t34 = env["t34"];
  double ta = env["ta"];
  double tfac = env["tfac"];
  double u1 = env["u1"];
  double u12 = env["u12"];
  double u2 = env["u2"];
  double u3 = env["u3"];
  double u34 = env["u34"];
  double u4 = env["u4"];
  double ua = env["ua"];
  double ustar = env["ustar"];
  double vonk = env["vonk"];
  double z1 = env["z1"];
  double z12 = env["z12"];
  double z2 = env["z2"];
  double z3 = env["z3"];
  double z34 = env["z34"];
  double z4 = env["z4"];
  double za = env["za"];
  // local
  double yu, yl, xfac, x, rwork, cdmax, tauu,a, b, c, d, taul, ca, cai, cbi, cci, cdi, cei, cfi, sg0, si0;
  List res;
  
  yu = 0;
  yl = 0;
  
  xfac = 1;
  
  res = fstratCpp(env, t34, t12, xfac, q34, q12, z3, z2, 
               alogl, alogl, alog2, u2, richl, straml, strahl, iter);
  
  richl  = res["rich"];
  straml = res["stram"];
  strahl = res["strah"];
  
  res = fstratCpp(env, t12, ta,  tfac, q12, qa,  z1, za, 
               alogu, alogu, aloga, ua, richu, stramu, strahu, iter);
  
  richu  = res["rich"];
  stramu = res["stram"];
  strahu = res["strah"];
  
  x = pow(((alog4 - alogav) / vonk), 2)  * bdl;
  
  rwork = 1 / expl;
  yl = ((x + 1) * expl + (x - 1) * rwork) / ((x + 1) * expl - (x - 1) * rwork);
  
  alogl = alog3 - vonk * sqrt(yl / bdl);
  
  x = pow(((alog2 - alogl) / vonk), 2)  * bdu / straml;
  
  rwork = 1 / expu;
  yu = ((x + 1) * expu + (x - 1) * rwork) / ((x + 1) * expu - (x - 1) * rwork);
  
  alogu = alog1 - vonk * sqrt(yu / bdu);
  
  cdmax = 300 / (2 * dtime);
  
  tauu = pow((ua * vonk / (aloga - alogu)), 2)  * stramu;
  
  ustar = pow(tauu, 0.5 );
  
  a = 0.5 * tauu * (yu + 1) / bdu;
  b = 0.5 * tauu * (yu - 1) / bdu;
  
  taul = bdu * (a / expu - b*expu);
  
  c = 0.5 * taul * (yl + 1) / bdl;
  d = 0.5 * taul * (yl - 1) / bdl;
  
  u1 = std::max(0.01, sqrt(std::max(0.0, (a + b))));
  u12 = std::max(0.01, sqrt(std::max(0.0, (a / exphu + b * exphu))));
  u2 = std::max(0.01, sqrt(std::max(0.0, (a / expu + b * expu))));
  u3 = std::max(0.01, sqrt(std::max(0.0, (c + d))));
  u34 = std::max(0.01, sqrt(std::max(0.0, (c / exphl + d * exphl))));
  u4 = std::max(0.01, sqrt(std::max(0.0, (c / expl + d * expl))));
  
  ca = ua * strahu * pow(vonk, 2)  / ((aloga - alogu) * (aloga - alog1));
  
  ca = std::min(cdmax, ca / (1 + ca * (1e-20)));
  
  cai = 1 / (rhoa * ca);
  
  cbi = diu * (z1 - z12) / (rhoa * 0.5 * (u1 + u12));
  cci = diu * (z12 - z2) / (rhoa * 0.5 * (u12 + u2));
  
  cdi = (alog2 - alogl) * (alog2 - alog3) / (rhoa * u2 * strahl * pow(vonk, 2));
  
  cei = dil * (z3 - z34) / (rhoa * 0.5 * (u3 + u34));
  cfi = dil * (z34 - z4) / (rhoa * 0.5 * (u34 + u4));
  
  cu = 1 / (cai + cbi);
  cl = 1 / (cci + cdi + cei);
  
  su = rhoa * cleaf * sqrt(u12 / dleaf[1]);
  ss = rhoa * cstem * sqrt(u12 / dstem[1]);
  sl = rhoa * cgrass * sqrt(u34 / dleaf[0]);

    sg0 = rhoa * (0.004 + 0.012 * u4);
  si0 = rhoa * (0.003 + 0.010 * u4);
  
  sg = 1 / (cfi + 1 / sg0);
  si = 1 / (cfi + 1 / si0);
  
  // Rprintf("richl %.16f \n", richl);
  // Rprintf("straml %.16f \n", straml);
  // Rprintf("strahl %.16f \n", strahl);
  // Rprintf("richu %.16f \n", richu);
  // Rprintf("stramu %.16f \n", stramu);
  // Rprintf("strahu %.16f \n", strahu);
  // Rprintf("alogl %.16f \n", alogl);
  // Rprintf("alogu %.16f \n", alogu);
  // Rprintf("ustar %.16f \n", ustar);
  // Rprintf("u1 %.16f \n", u1);
  // Rprintf("u12 %.16f \n", u12);
  // Rprintf("u2 %.16f \n", u2);
  // Rprintf("u3 %.16f \n", u3);
  // Rprintf("u34 %.16f \n", u34);
  // Rprintf("u4 %.16f \n", u4);
  // Rprintf("cu %.16f \n", cu);
  // Rprintf("cl %.16f \n", cl);
  // Rprintf("su %.16f \n", su);
  // Rprintf("ss %.16f \n", ss);
  // Rprintf("sl %.16f \n", sl);
  // // Rprintf("use %.16f \n", use);
  // Rprintf("sg %.16f \n", sg);
  // Rprintf("si %.16f \n", si);
  
  env.assign("richl", richl);
  env.assign("straml", straml);
  env.assign("strahl", strahl);
  env.assign("richu", richu);
  env.assign("stramu", stramu);
  env.assign("strahu", strahu);
  env.assign("alogl", alogl);
  env.assign("alogu", alogu);
  env.assign("ustar", ustar);
  env.assign("u1", u1);
  env.assign("u12", u12);
  env.assign("u2", u2);
  env.assign("u3", u3);
  env.assign("u34", u34);
  env.assign("u4", u4);
  env.assign("cu", cu);
  env.assign("cl", cl);
  env.assign("su", su);
  env.assign("ss", ss);
  env.assign("sl", sl);
  env.assign("sg", sg);
  env.assign("si", si);
}

// [[Rcpp::export]]
List fstratCpp(Environment env, double tb, double tt, double ttfac, double qb, double qt, double zb, double zt, double albm, double albh, double alt, double u, double rich, double stram, double strah, int iter){
  // global
  double grav = env["grav"];
  double vonk = env["vonk"];
  // local
  double stramx, strahx, np, nq, zht, zhb, xm, xh, rwork, ym, yh, z, w;
  
  stramx = 0;
  strahx = 0;

  np = 0;
  nq = 0;
  
  zht = tt * ttfac * (1 + 0.622 * qt);
  zhb = tb * (1 + 0.622 * qb);
  
  rich = grav * std::max(zt - zb, 0.0) * (zht - zhb) / (0.5 * (zht + zhb) * pow(u, 2));
  
  rich = std::max( (-2.0), std::min(rich, 1.0));
  
  if(rich <= 0) {
    np = np + 1;
  } else {
    nq = nq + 1;
  }
  
  if(np > 0) {
    for(int j = 0 ; j < np; j++) { 
      xm = std::max(alt - albm, 0.5);
      xh = std::max(alt - albh, 0.5);
      
      rwork = sqrt(-rich);
      
      ym = pow((vonk / xm), 2)  * std::exp(0.5 * xm) * rwork;
      yh = pow((vonk / xh), 2)  * std::exp(0.5 * xh) * rwork;
      
      stramx = 1 - 2 * 5 * rich / (1 + 75 * ym);
      strahx = 1 - 3 * 5 * rich / (1 + 75 * yh);
    }
  }
  
  if(nq > 0) {
    for(int j = 0;j < nq; j++) { 
      z = sqrt(1 + 5  * rich);
      
      stramx = 1 / (1 + 2 * 5 * rich / z);
      strahx = 1 / (1 + 3 * 5 * rich * z);
    }
  }
  if(iter == 1) {
    stram = stramx;
    strah = strahx;
  } else {
    w = 0.5;
    
    stram = w * stramx + (1 - w) * stram;
    strah = w * strahx + (1 - w) * strah;
  }
  
  return(List::create(Named("rich") = rich, Named("stram") = stram, Named("strah") = strah));
}