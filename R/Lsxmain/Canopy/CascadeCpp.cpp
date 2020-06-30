#include <Rcpp.h>
#include <../../Lsxmain/Comsat.cpp>
using namespace Rcpp;

double twet3Cpp(Environment env, double tak, double q, double p);
List mixCpp(Environment env, double xm, double tm, double x1, double t1, double x2, double t2, double x3, double t3);
List steph2oCpp(Environment env, double tveg, double wliq,  double wsno, double xai, double pflux, double rain, double train, double snow, double tsnow, double tdrip, double tblow, double wliqmax, double wsnomax, double wliqmin, double wsnomin);

// [[Rcpp::export]]
void cascadeCpp(Environment env) {
  // global  
  NumericVector sai = env["sai"];
  NumericVector lai = env["lai"];
  double dtime     = env["dtime"];
  double fl        = env["fl"];
  double fu        = env["fu"];
  double pfluxl    = env["pfluxl"];
  double pfluxs    = env["pfluxs"];
  double pfluxu    = env["pfluxu"];
  double psurf     = env["psurf"];
  double qa        = env["qa"];
  double raina     = env["raina"];
  double raing     = env["raing"];
  double rainl     = env["rainl"];
  double rainu     = env["rainu"];
  double snowa     = env["snowa"];
  double snowg     = env["snowg"];
  double snowl     = env["snowl"];
  double snowu     = env["snowu"];
  double t12       = env["t12"];
  double t34       = env["t34"];
  double ta        = env["ta"];
  double tblowl    = env["tblowl"];
  double tblows    = env["tblows"];
  double tblowu    = env["tblowu"];
  double tdripl    = env["tdripl"];
  double tdrips    = env["tdrips"];
  double tdripu    = env["tdripu"];
  double tl        = env["tl"];
  double tmelt     = env["tmelt"];
  double traing    = env["traing"];
  double trainl    = env["trainl"];
  double trainu    = env["trainu"];
  double ts        = env["ts"];
  double tsnowg    = env["tsnowg"];
  double tsnowl    = env["tsnowl"];
  double tsnowu    = env["tsnowu"];
  double tu        = env["tu"];
  double vzero     = env["vzero"];
  double wliql     = env["wliql"];
  double wliqlmax  = env["wliqlmax"];
  double wliqmin   = env["wliqmin"];
  double wliqs     = env["wliqs"];
  double wliqsmax  = env["wliqsmax"];
  double wliqu     = env["wliqu"];
  double wliqumax  = env["wliqumax"];
  double wsnol     = env["wsnol"];
  double wsnolmax  = env["wsnolmax"];
  double wsnomin   = env["wsnomin"];
  double wsnos     = env["wsnos"];
  double wsnosmax  = env["wsnosmax"];
  double wsnou     = env["wsnou"];
  double wsnoumax  = env["wsnoumax"];
  double xirriga   = env["xirriga"];
  
  // local
  List retorno;
  double twetbulb;
  double x1;
  double x2;
  double x3;
  double x4;
  double xai;
  double rain;
  double train;
  double snow;
  double tsnow;
  
  wliqmin = 0.0010 * (dtime/3600.) * (wliqumax / 0.2);
  wsnomin = 0.0010 * (dtime/3600.) * (wsnoumax / 2.0);
  
  rainu = raina;
  
  rainu = rainu + xirriga;
  
  if (ta > tmelt) {
    twetbulb = twet3Cpp(env, ta, qa, psurf );
  }else{
    twetbulb = tmelt;
  }
  trainu = std::max(twetbulb, tmelt);
  x1 = 0.0;
  x2 = std::max(t12, tmelt);
  retorno = mixCpp(env, rainu,trainu, rainu,trainu, x1,x2, vzero,vzero);
  
  rainu = retorno["xm"];
  trainu = retorno["tm"];
  
  snowu = snowa;
  tsnowu = std::min(ta, tmelt);
  x1 = 0.0;
  x2 = std::min(t12, tmelt);
  
  retorno = mixCpp(env, snowu,tsnowu, snowu,tsnowu, x1,x2, vzero,vzero);
  snowu = retorno["xm"];
  tsnowu = retorno["tm"];
  
  xai = 2.0 * lai[1]; 
  rain = rainu;
  train = trainu;
  snow = snowu;
  tsnow = tsnowu;
  
  retorno = steph2oCpp(env, tu,  wliqu,  wsnou,  xai,  pfluxu,  rain, train, snow, tsnow, tdripu, tblowu, wliqumax, wsnoumax, wliqmin, wsnomin);
  
  wliqu = retorno["wliq"];
  wsnou = retorno["wsno"];
  pfluxu = retorno["pflux"];
  rain = retorno["rain"];
  snow = retorno["snow"];
  train = retorno["train"];
  tsnow = retorno["tsnow"];
  
  
  xai = 2.0 * sai[1]; 
  
  retorno = steph2oCpp (env, ts,  wliqs,  wsnos,  xai,  pfluxs,  rain, train, snow, tsnow, tdrips, tblows, wliqsmax, wsnosmax, wliqmin, wsnomin);
  
  wliqs = retorno["wliq"];
  wsnos = retorno["wsno"];
  pfluxs = retorno["pflux"];
  rain = retorno["rain"];
  snow = retorno["snow"];
  train = retorno["train"];
  tsnow = retorno["tsnow"];
  
  x1 = fu*rain;
  x2 = (1.-fu)*rainu;
  x3 = 0.0;
  x4 = std::max (t34, tmelt);
  
  retorno = mixCpp(env ,rainl,trainl, x1,train, x2,trainu, x3,x4);
  rainl = retorno["xm"];
  trainl = retorno["tm"];
  
  x1 = fu*snow;
  x2 = (1.-fu)*snowu;
  x3 = 0.0;
  x4 = std::min(t34, tmelt);
  
  retorno = mixCpp(env, snowl,tsnowl, x1,tsnow, x2,tsnowu, x3,x4);
  snowl = retorno["xm"];
  tsnowl = retorno["tm"];
  
  xai = 2.0 * (lai[0] + sai[0]); 
  rain = rainl;
  train = trainl;
  snow = snowl;
  tsnow = tsnowl;
  
  retorno = steph2oCpp(env, tl,  wliql,  wsnol,  xai,  pfluxl,  rain, train, snow, tsnow, tdripl, tblowl, wliqlmax, wsnolmax, wliqmin, wsnomin);
  
  wliql = retorno["wliq"];
  wsnol = retorno["wsno"];
  pfluxl = retorno["pflux"];
  rain = retorno["rain"];
  snow = retorno["snow"];
  train = retorno["train"];
  tsnow = retorno["tsnow"];
  
  x1 = fl * rain;
  x2 = (1.-fl) * rainl;
  
  retorno = mixCpp(env, raing,traing, x1,train, x2,trainl, vzero,vzero);
  raing = retorno["xm"];
  traing = retorno["tm"];
  
  x1 = fl * snow;
  x2 = (1.-fl) * snowl;
  
  retorno = mixCpp(env, snowg,tsnowg, x1,tsnow, x2,tsnowl, vzero,vzero);
  snowg = retorno[0];
  tsnowg = retorno[1];
  
  env.assign( "wliqmin", wliqmin );
  env.assign( "wsnomin", wsnomin );
  env.assign( "rainu", rainu );
  env.assign( "trainu", trainu );
  env.assign( "snowu", snowu );
  env.assign( "tsnowu", tsnowu );
  env.assign( "wliqu", wliqu );
  env.assign( "wsnou", wsnou );
  env.assign( "pfluxu", pfluxu );
  env.assign( "wliqs", wliqs );
  env.assign( "wsnos", wsnos );
  env.assign( "pfluxs", pfluxs );
  env.assign( "rainl", rainl );
  env.assign( "trainl", trainl );
  env.assign( "snowl", snowl );
  env.assign( "tsnowl", tsnowl );
  env.assign( "wliql", wliql );
  env.assign( "wsnol", wsnol );
  env.assign( "pfluxl", pfluxl );
  env.assign( "raing", raing );
  env.assign( "traing", traing );
  env.assign( "snowg", snowg );
  env.assign( "tsnowg", tsnowg );
}

// [[Rcpp::export]]
List steph2oCpp(Environment env, double tveg, double wliq,  double wsno, double xai, double pflux, double rain, double train, double snow, double tsnow, double tdrip, double tblow, double wliqmax, double wsnomax, double wliqmin, double wsnomin) {
  // Global
  double ch2o = env["ch2o"];
  double cice = env["cice"];
  double dtime = env["dtime"];
  double epsilon = env["epsilon"];
  double hfus = env["hfus"];
  double tmelt = env["tmelt"];
  double vzero = env["vzero"];
  // Local
  List retorno;
  double fint;
  double rwork;
  double drip;
  double x;
  double dw;
  double rwork2;
  double blow;
  
  if (xai >= epsilon) {
    fint = ( 1. - std::exp(-0.5*xai) )/ xai; 
  }else{
    fint = 0.5;
  }
  
  rwork = 1. / dtime;
  
  drip = xai*wliq/tdrip;
  wliq = wliq * (1.-dtime/tdrip);
  
  wliq = wliq + dtime*rain*fint;
  pflux = rain*fint * (tveg-train)*ch2o;
  rain = rain*(1.-xai*fint);
  
  x = wliq;
  wliq = std::min(wliq, wliqmax);
  if (wliq < wliqmin) 
    wliq  = 0;
  drip = drip + xai*(x-wliq)*rwork;
  
  blow = xai*wsno/tblow;
  wsno = wsno * (1.-dtime/tblow);
  
  wsno = wsno + dtime*snow*fint;
  pflux = pflux + snow*fint * (tveg-tsnow)*cice;
  snow = snow*(1.-xai*fint);
  
  x = wsno;
  wsno = std::min(wsno, wsnomax);
  if (wsno < wsnomin) 
    wsno = 0;
  blow = blow + xai*(x-wsno)*rwork;
  
  rwork2 = ch2o - cice;
  
  dw = 0;
  if (tveg < tmelt) 
    dw = wliq;
  
  pflux = pflux + dw * (rwork2*(tmelt-tveg) - hfus) * rwork;
  wliq = wliq - dw;
  wsno = wsno + dw;
  
  dw = 0;
  if (tveg > tmelt)  
    dw = wsno;
  
  pflux = pflux + dw * (rwork2*(tveg-tmelt) + hfus) * rwork;
  wsno = wsno - dw;
  wliq = wliq + dw;
  
  retorno = mixCpp(env, rain,train, rain,train, drip,tveg, vzero,vzero);
  rain = retorno["xm"];
  train = retorno["tm"];
  
  retorno = mixCpp(env, snow,tsnow, snow,tsnow, blow,tveg, vzero,vzero);
  snow = retorno["xm"];
  tsnow = retorno["tm"];
  
  return(List::create(Named("wliq") = wliq, Named("pflux") = pflux, Named("rain") = rain, Named("wsno") = wsno, Named("snow") = snow, Named("train") = train, Named("tsnow") = tsnow));
}

// [[Rcpp::export]]
List mixCpp(Environment env, double xm, double tm, double x1, double t1, double x2, double t2, double x3, double t3) {
  // Global
  double epsilon = env["epsilon"];
  // Local
  double xtmp;
  double ytmp;
  double ttmp;
  
  xtmp = x1 + x2 + x3;
  ytmp = std::max((double)std::abs(xtmp), epsilon) * cpp_sign(xtmp);
  // ytmp <- sign (max (abs(xtmp), epsilon), xtmp)
  if (std::abs(xtmp) >= epsilon) {
    ttmp = (t1*x1 + t2*x2 + t3*x3) / ytmp;
  }else{
    ttmp = 0;
    xtmp = 0;
  }
  xm = xtmp;
  tm = ttmp;
  
  return(List::create(Named("xm") = xm, Named("tm") = tm));
}



// [[Rcpp::export]]
double twet3Cpp(Environment env, double tak, double q, double p) {
  // global
  double cair = env["cair"];
  // local
  double ta;
  double twet3;
  double twold;
  double diff;
  double twk;
  
  ta = tak - 273.16;
  
  twet3 = ta * q / cpp_qsat(cpp_esat(tak),p);
  
  twk = twet3 + 273.16;
  
  for (int i = 0;i < 20; i++) {
    twold = twk - 273.16;
    twet3 = ta - (cpp_hvapf(env,twk,tak)/cair) * ( cpp_qsat( cpp_esat(twk),p ) - q );
    diff = twet3 - twold;
    
    twk = twold + 0.2 * diff + 273.16;
    
    if (std::abs(twk - 273.16 - twold) < 0.02) {
      twet3 = twk;
      return(twet3);
    }
  }
  twet3 = tak;
  twet3 = twk;
  
  return(twet3);
}