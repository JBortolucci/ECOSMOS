#include <Rcpp.h>
#include <../../Lsxmain/Comsat.cpp>
using namespace Rcpp;

List twostrCpp(Environment env, double abvegd, double abvegi, double refld, double refli, double fbeldd, double fbeldi, double fbelid, double fbelii, NumericVector asurd, NumericVector asuri, double iv, double coszen, double ib);
List twosetCpp(Environment env, double omega, double betad, double betai, double avmu, double gdir, double coszen, double iv, double ib);

// [[Rcpp::export]]
void solalbCpp(Environment env, double ibb) {
  
  // global
  NumericVector asurd = env["asurd"];
  NumericVector asuri = env["asuri"];
  double ablod = env["ablod"];
  double abloi = env["abloi"];
  double abupd = env["abupd"];
  double abupi = env["abupi"];
  double albsnd = env["albsnd"];
  double albsni = env["albsni"];
  double albsod = env["albsod"];
  double albsoi = env["albsoi"];
  double coszen = env["coszen"];
  double dummy = env["dummy"];
  double fi = env["fi"];
  double fl = env["fl"];
  double flodd = env["flodd"];
  double flodi = env["flodi"];
  double floii = env["floii"];
  double fu = env["fu"];
  double fupdd = env["fupdd"];
  double fupdi = env["fupdi"];
  double fupii = env["fupii"];
  // double indsol = env["indsol"];
  double nsol = env["nsol"];
  double relod = env["relod"];
  double reloi = env["reloi"];
  double reupd = env["reupd"];
  double reupi = env["reupi"];
  // local
  int ib = ibb - 1;
  List retorno;
  // int i;
  
  if (nsol == 0) return;
  
  for (int j = 0;j < nsol; j++) {
    // i = indsol;
    
    asurd[ib] = albsod;
    asuri[ib] = albsoi;
  }
  
  retorno = twostrCpp(env, ablod, abloi, relod, reloi,  flodd,  dummy, flodi, floii,  asurd,  asuri,  1,  coszen, ib);
  ablod = retorno["abvegd"];
  abloi = retorno["abvegi"];
  relod = retorno["refld"];
  reloi = retorno["refli"];
  flodd = retorno["fbeldd"];
  dummy = retorno["fbeldi"];
  flodi = retorno["fbelid"];
  floii = retorno["fbelii"];
  
  for (int j = 0;j < nsol; j++) {
    // i = indsol;
    
    asurd[ib] = fl * (1. - fi) * relod + (1. - fl) * (1. - fi) * albsod + fi * albsnd;
    asuri[ib] = fl * (1. - fi) * reloi + (1. - fl) * (1. - fi) * albsoi + fi * albsni;
  } 
  
  retorno = twostrCpp(env, abupd, abupi,  reupd, reupi,  fupdd,  dummy, fupdi, fupii,  asurd,  asuri,  2, coszen, ib);
  
  abupd = retorno["abvegd"];
  abupi = retorno["abvegi"];
  reupd = retorno["refld"];
  reupi = retorno["refli"];
  fupdd = retorno["fbeldd"];
  dummy = retorno["fbeldi"];
  fupdi = retorno["fbelid"];
  fupii = retorno["fbelii"];
  
  for (int j = 0;j < nsol; j++) {
    // i = indsol;
    
    asurd[ib] = fu * reupd + (1. - fu) * asurd[ib];
    asuri[ib] = fu * reupi + (1. - fu) * asuri[ib];
  }
  
  env.assign("asurd", asurd);
  env.assign("asuri", asuri);
  env.assign("ablod", ablod);
  env.assign("abloi", abloi);
  env.assign("relod", relod);
  env.assign("reloi", reloi);
  env.assign("flodd", flodd);
  env.assign("flodi", flodi);
  env.assign("floii", floii);
  env.assign("abupd", abupd);
  env.assign("abupi", abupi);
  env.assign("reupd", reupd);
  env.assign("reupi", reupi);
  env.assign("fupdd", fupdd);
  env.assign("dummy", dummy);
  env.assign("fupdi", fupdi);
  env.assign("fupii", fupii);
}

// [[Rcpp::export]]
List twostrCpp(Environment env, double abvegd, double abvegi, double refld, double refli, double fbeldd, double fbeldi, double fbelid, double fbelii, NumericVector asurd, NumericVector asuri, double iv, double coszen, double ib) {
  // global
  NumericVector lai = env["lai"];
  NumericVector sai = env["sai"];
  NumericVector terml = env["terml"];
  NumericVector termu = env["termu"];
  double epsilon = env["epsilon"];
  double indsol = env["indsol"];
  double nsol = env["nsol"];
  // local
  List retorno;
  double omega = 0;
  double betad = 0;
  double betai = 0;
  double avmu = 0;
  double gdir = 0;
  double tmp0;
  double i, b, c, q, k, p, c0, d, f, h, sigma, ud1, ui1, ud2, ui2, ud3, xai, s1, s2, p1, p2, p3, p4, rwork, dd1, di1, dd2, di2;
  double h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, absurd, absuri;
  
  // if (nsol == 0) return;
  
  retorno = twosetCpp(env, omega, betad, betai, avmu, gdir, coszen, iv, ib);
  
  omega = retorno["omega"];
  betad = retorno["betad"];
  betai = retorno["betai"];
  avmu  = retorno["avmu"];
  gdir  = retorno["gdir"];
  
  for (int j = 0;j < nsol; j++) {
    
    i = indsol - 1;
    b = 1. - omega * (1. - betai);
    c = omega * betai;
    
    tmp0 = b * b - c * c;
    
    q = sqrt( std::max(0.0, tmp0) );
    k = gdir / std::max(coszen, 0.01);
    p = avmu * k;
    
    if((std::abs(p - q)) < (.001 * p)) {
      p = (1. + (.001 * cpp_sign(p - q))) * p;
    }
    
    c0 = omega * p;
    d = c0 * betad;
    f = c0 * (1. - betad);
    h = q / avmu;
    
    sigma = p * p - tmp0;
    
    ud1 = b - c / asurd[ib];
    ui1 = b - c / asuri[ib];
    ud2 = b - c * asurd[ib];
    ui2 = b - c * asuri[ib];
    ud3 = f + c * asurd[ib];
    
    xai = std::max(lai(i,iv-1) + sai(i,iv-1), epsilon);
    
    s1 = std::exp(-1. * h * xai);
    s2 = std::exp(-1. * k * xai);
    
    p1 = b + q;
    p2 = b - q;
    p3 = b + p;
    p4 = b - p;
    rwork = 1. / s1;
    
    dd1 = p1 * (ud1 - q) * rwork - p2 * (ud1 + q) * s1;
    di1 = p1 * (ui1 - q) * rwork - p2 * (ui1 + q) * s1;
    dd2 = (ud2 + q) * rwork - (ud2 - q) * s1;
    di2 = (ui2 + q) * rwork - (ui2 - q) * s1;
    h1 = -1. * d * p4 - c * f;
    rwork = s2 * (d - c - h1 * (ud1 + p) / sigma);
    h2 = 1. / dd1 * ((d - h1 * p3 / sigma) * (ud1 - q) / s1 - p2 * rwork);
    h3 = -1. / dd1 * ((d - h1 * p3 / sigma) * (ud1 + q) * s1 - p1 * rwork);
    h4 = -1. * f * p3 - c * d;
    rwork = s2 * (ud3 - h4 * (ud2 - p) / sigma);
    h5 = -1. / dd2 * (h4 * (ud2 + q) / (sigma * s1) + rwork);
    h6 = 1. / dd2 * (h4 * s1 * (ud2 - q) / sigma + rwork);
    h7 = c * (ui1 - q) / (di1 * s1);
    h8 = -1. * c * s1 * (ui1 + q) / di1;
    h9 = (ui2 + q) / (di2 * s1);
    h10 = -1. * s1 * (ui2 - q) / di2;
    
    fbeldd = s2;
    fbeldi = 0.;
    fbelid = h4 / sigma * s2 + h5 * s1 + h6 / s1;
    fbelii = h9 * s1 + h10 / s1;
    
    refld = h1 / sigma + h2 + h3;
    
    refli = h7 + h8;
    absurd = (1. - asurd[ib]) * fbeldd + (1. - asuri[ib]) * fbelid;
    absuri = (1. - asuri[ib]) * fbelii;
    
    abvegd = std::max(0., 1 - refld - absurd);
    abvegi = std::max(0., 1 - refli - absuri);
    
    if (xai < epsilon) abvegd = 0.0;
    if (xai < epsilon) abvegi = 0.0;
    
    
    if (ib == 0) {
      if (iv == 1) {
        terml[0] = k * (1. + (h4 - h1) / sigma);
        terml[1] = h * (h5 - h2);
        terml[2] = h * (h9 - h7);
        terml[3] = h * (h3 - h6);
        terml[4] = h * (h8 - h10);
        terml[5] = k;
        terml[6] = h;
        
      }else{
        
        termu[0] = k * (1. + (h4 - h1) / sigma);
        termu[1] = h * (h5 - h2);
        termu[2] = h * (h9 - h7);
        termu[3] = h * (h3 - h6);
        termu[4] = h * (h8 - h10);
        termu[5] = k;
        termu[6] = h;
        
      }
      env.assign("terml", terml);
      env.assign("termu", termu);
    }
  }
  return(List::create(Named("abvegd") = abvegd, Named("abvegi") = abvegi, Named("refld") = refld, Named("refli") = refli, Named("fbeldd") = fbeldd, Named("fbeldi") = fbeldi, Named("fbelid") = fbelid, Named("fbelii") = fbelii));
}

List twosetCpp(Environment env, double omega, double betad, double betai, double avmu, double gdir, double coszen, double iv, double ib) {
  // global
  NumericVector lai = env["lai"];
  NumericVector sai = env["sai"];
  NumericVector orieh = env["orieh"];
  NumericVector oriev = env["oriev"];
  double epsilon = env["epsilon"];
  double fwetl = env["fwetl"];
  double fwets = env["fwets"];
  double fwetu = env["fwetu"];
  double greenfracl = env["greenfracl"];
  double indsol = env["indsol"];
  double nsol = env["nsol"];
  double rliql = env["rliql"];
  double rliqs = env["rliqs"];
  double rliqu = env["rliqu"];
  double tl = env["tl"];
  double tmelt = env["tmelt"];
  double ts = env["ts"];
  double tu = env["tu"];
  double pi = M_PI;
  // local
  double ntmu, betadsno, betaisno, i, o, x, y, rwork;
  double rhovegvlg, rhovegvlb, rhovegvu, rhovegirlg, rhovegirlb, rhovegiru, tauvegvlg, tauvegvlb, tauvegvu, tauvegirlg, tauvegirlb, tauvegiru;
  double zrho, ztau, orand, itab, ztab, otmp;
  NumericVector omegasno, tablemu;
  
  iv = iv - 1;
  ntmu = 100;
  
  double dataAux[101] = {0.5000, 0.4967, 0.4933, 0.4900, 0.4867, 0.4833, 0.4800, 0.4767, 0.4733, 0.4700, 
                         0.4667, 0.4633, 0.4600, 0.4567, 0.4533, 0.4500, 0.4467, 0.4433, 0.4400, 0.4367,
                         0.4333, 0.4300, 0.4267, 0.4233, 0.4200, 0.4167, 0.4133, 0.4100, 0.4067, 0.4033, 
                         0.4000, 0.3967, 0.3933, 0.3900, 0.3867, 0.3833, 0.3800, 0.3767, 0.3733, 0.3700, 
                         0.3667, 0.3633, 0.3600, 0.3567, 0.3533, 0.3500, 0.3467, 0.3433, 0.3400, 0.3367, 
                         0.3333, 0.3300, 0.3267, 0.3233, 0.3200, 0.3167, 0.3133, 0.3100, 0.3067, 0.3033, 
                         0.3000, 0.2967, 0.2933, 0.2900, 0.2867, 0.2833, 0.2800, 0.2767, 0.2733, 0.2700, 
                         0.2667, 0.2633, 0.2600, 0.2567, 0.2533, 0.2500, 0.2467, 0.2433, 0.2400, 0.2367, 
                         0.2333, 0.2300, 0.2267, 0.2233, 0.2200, 0.2167, 0.2133, 0.2100, 0.2067, 0.2033, 
                         0.2000, 0.1967, 0.1933, 0.1900, 0.1867, 0.1833, 0.1800, 0.1767, 0.1733, 0.1700, 0.1667};
  
  tablemu = NumericVector(dataAux,dataAux + sizeof(dataAux)/sizeof(*dataAux));
  
  omegasno = NumericVector::create(0.9, 0.7);
  betadsno = 0.5;
  betaisno = 0.5;
  
  rhovegvlg = 0.10;
  rhovegvlb = 0.36;
  rhovegvu = 0.10;
  
  rhovegirlg = 0.48;
  rhovegirlb = 0.58;
  rhovegiru = 0.40;
  
  tauvegvlg = 0.07;
  tauvegvlb = 0.22;
  tauvegvu = 0.05;
  
  tauvegirlg = 0.25;
  tauvegirlb = 0.38;
  tauvegiru = 0.20;
  
  for (int j = 0;j < nsol; j++) {
    // i = indsol - 1;
    if (iv == 1) {
      if (ib == 0) {
        
        zrho = rhovegvu;
        ztau = tauvegvu;
      }else{
        
        zrho = rhovegiru;
        ztau = tauvegiru;
      }
    }else{
      if (ib == 0) {
        
        zrho = greenfracl * rhovegvlg + rhovegvlb * (1. - greenfracl);
        ztau = greenfracl * tauvegvlg + tauvegvlb * (1. - greenfracl);
        
      }else{
        
        zrho = greenfracl * rhovegirlg + rhovegirlb * (1. - greenfracl);
        ztau = greenfracl * tauvegirlg + tauvegirlb * (1. - greenfracl);
      }
    }
    
    orand = 1. - oriev[iv] - orieh[iv];
    
    omega = zrho + ztau;
    
    itab = round((coszen * ntmu + 1));
    ztab = tablemu[itab - 1];
    rwork = 1. / omega;
    
    betad = (oriev[iv] * 0.5*(zrho + ztau) + orieh[iv] * zrho + orand * ((1. - ztab) * zrho + ztab * ztau)) * rwork;
    
    betai = (oriev[iv] * 0.5*(zrho + ztau) + orieh[iv] * zrho + orand * ((2. / 3.) * zrho + (1. / 3.) * ztau)) * rwork;
    
    gdir = oriev[iv] * (2. / pi) * sqrt (std::max(0., 1. - coszen * coszen)) + orieh[iv] * coszen + orand * 0.5;
    
    avmu = 1.;
    
  }
  
  if (iv == 0) {
    
    for (int j = 0;j < nsol; j++) {
      // i = indsol - 1;
      y = fwetl * (1. - rliql);
      o = omegasno[ib] * (.6 + .4* std::max(0., std::min(1.,(tmelt - tl) / 1.0)));
      otmp  = omega;
      rwork = y * o;
      omega =  (1 - y) * otmp + rwork;
      betad = ((1 - y) * otmp * betad + rwork * betadsno) / omega;
      betai = ((1 - y) * otmp * betai + rwork * betaisno) / omega;
    }
  }else{
    
    for (int j = 0;j < nsol; j++) {
      i = indsol - 1;
      x = lai(i,iv) / std::max(lai(i,iv) + sai(i,iv), epsilon);
      y = x * fwetu * (1. - rliqu) + (1 - x) * fwets * (1. - rliqs);
      o = (x * std::min(1., std::max(.6, (tmelt - tu) / 0.1)) + (1 - x) * std::min(1., std::max(.6, (tmelt - ts) / 0.1))) *  omegasno[ib];
      
      otmp  = omega;
      rwork = y * o;
      
      omega = (1 - y) * otmp + rwork;
      
      betad = ((1 - y) * otmp * betad + rwork * betadsno) / omega;
      betai = ((1 - y) * otmp * betai + rwork * betaisno) / omega;
    }
  }
  return(List::create(Named("omega") = omega, Named("betad") = betad, Named("betai") = betai, Named("avmu") = avmu, Named("gdir") = gdir));
}