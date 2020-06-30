#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void caniniCpp(Environment env, double jday) {
  // Global 
  NumericVector lai = env["lai"];
  NumericVector sai = env["sai"];
  NumericVector zbot = env["zbot"];
  NumericVector ztop = env["ztop"];
  double alaiml = env["alaiml"];
  double alaimu = env["alaimu"];
  double alog1 = env["alog1"];
  double alog2 = env["alog2"];
  double alog3 = env["alog3"];
  double alog4 = env["alog4"];
  double aloga = env["aloga"];
  double alogav = env["alogav"];
  double alogg = env["alogg"];
  double alogi = env["alogi"];
  double alogl = env["alogl"];
  double alogu = env["alogu"];
  double bdl = env["bdl"];
  double bdu = env["bdu"];
  double cair = env["cair"];
  double cappa = env["cappa"];
  double cp = env["cp"];
  double cvap = env["cvap"];
  double dil = env["dil"];
  double displ = env["displ"];
  double dispu = env["dispu"];
  double diu = env["diu"];
  double exphl = env["exphl"];
  double exphu = env["exphu"];
  double expl = env["expl"];
  double expu = env["expu"];
  double fi = env["fi"];
  double fl = env["fl"];
  double fu = env["fu"];
  double grav = env["grav"];
  double psurf = env["psurf"];
  double qa = env["qa"];
  double rair = env["rair"];
  double rhoa = env["rhoa"];
  double rvap = env["rvap"];
  double ta = env["ta"];
  double tfac = env["tfac"];
  double u2 = env["u2"];
  double ua = env["ua"];
  double z0sno = env["z0sno"];
  double z0soi = env["z0soi"];
  double z1 = env["z1"];
  double z12 = env["z12"];
  double z2 = env["z2"];
  double z3 = env["z3"];
  double z34 = env["z34"];
  double z4 = env["z4"];
  double za = env["za"];
  // Local
  double siga;
  double pa;
  double x;
  double x1;
  double rwork;
  double cvegl;
  double dvegl;
  double bvegl;
  double cvegu;
  double dvegu;
  double bvegu;
  
  siga = 0.997;
  tfac = 1.0 / pow(siga,cappa);
  
  pa = psurf * siga;
  rhoa = pa / ( rair * ta * (1.0 + (rvap / rair - 1.0) * qa) );
  cp = cair * (1.0 + (cvap / cair - 1.0) * qa);
  za = (psurf - pa) / (rhoa * grav);
  za = std::max(za, ztop[1] + 1.0);
  x = fl * (1.0 - fi) * 2.0 * (lai[0] + sai[0]) / alaiml;
  x = std::min(x, 3.0);
  x1 = std::min(x, 1.0);
  rwork = std::max(ztop[0] - zbot[0],0.01);
  cvegl = (0.4 / rwork) * std::max(1.e-5, x);
  dvegl = (0.1 * rwork) / std::max(1.e-5, std::max(x, pow(x,2)));
  
  bvegl = sqrt(2.0 * cvegl / dvegl );
  bdl = 0.5 * bvegl * dvegl;
  
  dil = 1. / dvegl;
  
  rwork = (1.0 - x1) * (std::max(z0soi,z0sno) + 0.01);
  
  z3 = x1 * ztop[0] + rwork;
  
  z4 = x1 * zbot[0] + rwork;
  
  z34 = 0.5 * (z3 + z4);
  
  exphl = std::exp(0.5 * bvegl * (z3-z4));
  expl = pow(exphl,2);
  
  displ = x1 * 0.7 * z3;
  
  x = fu * 2.0 * (lai[1]+sai[1]) / alaimu;
  
  x = std::min(x, 3.0);
  x1 = std::min(x, 1.0);
  
  rwork = std::max(ztop[1] - zbot[1],.01);
  cvegu = (0.4 / rwork) * std::max(1.e-5,x);
  
  dvegu = (0.1 * rwork) / std::max(1.e-5,std::max(x,pow(x,2)));
  
  rwork = 1. / dvegu;
  bvegu = sqrt (2.0 * cvegu * rwork);
  bdu = 0.5 * bvegu * dvegu;
  diu = rwork;
  
  rwork = (1.0 - x1) * (z3 + 0.01);
  z1 = x1 * ztop[1] + rwork;
  z2 = x1 * zbot[1] + rwork;
  
  z12 = 0.5 * (z1 + z2);
  
  exphu = std::exp(0.5 * bvegu * (z1 - z2));
  expu = pow(exphu,2);
  
  dispu = x1 * 0.7 * z1 + (1.0 - x1) * displ;
  
  alogg = std::log(z0soi);
  alogi = std::log(z0sno);
  alogav = (1.0 - fi) * alogg + fi * alogi;
  
  alog4 = std::log( std::max(z4, std::max(1.1*z0soi, 1.1*z0sno)) );
  alog3 = std::log(z3-displ);
  alog2 = std::log(z2-displ);
  alog1 = std::log(z1-dispu);
  aloga = std::log(za-dispu);
  
  u2 = ua/exphu;
  alogu = std::log( std::max(.01, .1*(z1-z2)));
  alogl = std::log( std::max(.01, .1*(z3-z4)));
  
  env.assign("tfac", tfac);
  env.assign("rhoa", rhoa);
  env.assign("cp", cp);
  env.assign("bdl", bdl);
  env.assign("dil", dil);
  env.assign("z3", z3);
  env.assign("z4", z4);
  env.assign("z34", z34);
  env.assign("exphl", exphl);
  env.assign("expl", expl);
  env.assign("displ", displ);
  env.assign("bdu", bdu);
  env.assign("diu", diu);
  env.assign("z1", z1);
  env.assign("z2", z2);
  env.assign("z12", z12);
  env.assign("exphu", exphu);
  env.assign("expu", expu);
  env.assign("dispu", dispu);
  env.assign("alogg", alogg);
  env.assign("alogi", alogi);
  env.assign("alogav", alogav);
  env.assign("alog4", alog4);
  env.assign("alog3", alog3);
  env.assign("alog2", alog2);
  env.assign("alog1", alog1);
  env.assign("aloga", aloga);
  env.assign("u2", u2);
  env.assign("alogu", alogu);
  env.assign("alogl", alogl);
}