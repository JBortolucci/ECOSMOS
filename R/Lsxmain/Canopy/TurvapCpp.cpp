#include <Rcpp.h>
#include <../../Lsxmain/Comsat.cpp>
using namespace Rcpp;

double impexpCpp(Environment env, double wimp, double tveg, double ch, double wliq, double wsno, int iter);
double impexp2Cpp(Environment env, double wimp, double t, double told, int iter);
List linsolveCpp(NumericMatrix arr, NumericVector rhs, NumericVector vec, NumericMatrix mplate, int nd);

// [[Rcpp::export]]
void turvapCpp(Environment env, int iter, int niter) {
  // Global
  NumericVector lai = env["lai"];
  NumericVector sai = env["sai"];
  NumericVector wsoi = env["wsoi"];
  NumericVector wisoi = env["wisoi"];
  NumericVector upsoil = env["upsoil"];
  NumericVector upsoiu = env["upsoiu"];
  NumericVector tsno = env["tsno"];
  NumericVector tsoi = env["tsoi"];
  NumericVector suction = env["suction"];
  NumericVector swilt = env["swilt"];
  NumericVector stressu = env["stressu"];
  NumericVector stressl = env["stressl"];
  NumericVector rhosoi = env["rhosoi"];
  NumericVector poros = env["poros"];
  NumericVector hsno = env["hsno"];
  NumericVector frac = env["frac"];
  NumericVector csoi = env["csoi"];
  NumericVector consoi = env["consoi"];
  NumericVector bex = env["bex"];
  NumericVector totcond = env["totcond"];
  NumericVector hsoi = env["hsoi"];
  double ch2o = env["ch2o"];
  double chl = env["chl"];
  double chs = env["chs"];
  double chu = env["chu"];
  double cice = env["cice"];
  double cl = env["cl"];
  double consno = env["consno"];
  double cp = env["cp"];
  double cu = env["cu"];
  double dtime = env["dtime"];
  double epsilon = env["epsilon"];
  double fi = env["fi"];
  double firb = env["firb"];
  double firg = env["firg"];
  double firi = env["firi"];
  double firl = env["firl"];
  double firs = env["firs"];
  double firu = env["firu"];
  double fl = env["fl"];
  double fsena = env["fsena"];
  double fseng = env["fseng"];
  double fseni = env["fseni"];
  double fsenl = env["fsenl"];
  double fsens = env["fsens"];
  double fsenu = env["fsenu"];
  double fu = env["fu"];
  double fvapa = env["fvapa"];
  double fvapg = env["fvapg"];
  double fvapi = env["fvapi"];
  double fvaplt = env["fvaplt"];
  double fvaplw = env["fvaplw"];
  double fvaps = env["fvaps"];
  double fvaput = env["fvaput"];
  double fvapuw = env["fvapuw"];
  double fwetl = env["fwetl"];
  double fwetlx = env["fwetlx"];
  double fwets = env["fwets"];
  double fwetsx = env["fwetsx"];
  double fwetu = env["fwetu"];
  double fwetux = env["fwetux"];
  double ginvap = env["ginvap"];
  double grav = env["grav"];
  double greenfracl = env["greenfracl"];
  double gsuvap = env["gsuvap"];
  double gtrans = env["gtrans"];
  double gtransl = env["gtransl"];
  double gtransu = env["gtransu"];
  double hsnotop = env["hsnotop"];
  double hvap = env["hvap"];
  double hvasug = env["hvasug"];
  double hvasui = env["hvasui"];
  double nsoilay = env["nsoilay"];
  double pfluxl = env["pfluxl"];
  double pfluxs = env["pfluxs"];
  double pfluxu = env["pfluxu"];
  double psurf = env["psurf"];
  double q12 = env["q12"];
  double q34 = env["q34"];
  double qa = env["qa"];
  double rhow = env["rhow"];
  double rliql = env["rliql"];
  double rliqs = env["rliqs"];
  double rliqu = env["rliqu"];
  double rvap = env["rvap"];
  double sg = env["sg"];
  double si = env["si"];
  double sl = env["sl"];
  double solg = env["solg"];
  double soli = env["soli"];
  double soll = env["soll"];
  double sols = env["sols"];
  double solu = env["solu"];
  double ss = env["ss"];
  double stef = env["stef"];
  double stresstl = env["stresstl"];
  double stresstu = env["stresstu"];
  double su = env["su"];
  double t12 = env["t12"];
  double t34 = env["t34"];
  double ta = env["ta"];
  double tfac = env["tfac"];
  double tg = env["tg"];
  double ti = env["ti"];
  double tl = env["tl"];
  double tmelt = env["tmelt"];
  // double totcondc3 = env["totcondc3"];
  // double totcondc4 = env["totcondc4"];
  // double totcondl3 = env["totcondl3"];
  // double totcondl4 = env["totcondl4"];
  // double totcondls = env["totcondls"];
  // double totcondub = env["totcondub"];
  // double totconduc = env["totconduc"];
  double ts = env["ts"];
  double tu = env["tu"];
  double wipud = env["wipud"];
  double wliql = env["wliql"];
  double wliqs = env["wliqs"];
  double wliqu = env["wliqu"];
  double wpud = env["wpud"];
  double wpudmax = env["wpudmax"];
  double wsnol = env["wsnol"];
  double wsnos = env["wsnos"];
  double wsnou = env["wsnou"];
  double xu = env["xu"];
  double xs = env["xs"];
  double xl = env["xl"];
  double chux = env["chux"];
  double chsx = env["chsx"];
  double chlx = env["chlx"];
  double chgx = env["chgx"];
  double wlgx = env["wlgx"];
  double wigx = env["wigx"];
  double cog = env["cog"];
  double coi = env["coi"];
  double zirg = env["zirg"];
  double ziri = env["ziri"];
  double wu = env["wu"];
  double ws = env["ws"];
  double wl = env["wl"];
  double wg = env["wg"];
  double wi = env["wi"];
  double tuold = env["tuold"];
  double tsold = env["tsold"];
  double tlold = env["tlold"];
  double tgold = env["tgold"];
  double tiold = env["tiold"];
  double upper = env["UPPER"];
  double lower = env["LOWER"];
  List plantList = env["plantList"];
  double npft = env["npft"];
  // local
  double fradu, frads, fradl, qu, qs, ql, qg, qi, dqu, dqs ,dql, dqg ,dqi, tupre, tspre, tlpre, tgpre;
  double tipre, suw, ssw, slw, sut, slt, slt0, suh, ssh, slh, qgfac, qgfac0, rwork, e, zwpud, zwsoi, zwtot;
  double psig, hfac, hfac2, zwopt, zwdry, betaw, qs1, dqs1, xnumer, xdenom, betafac, betas, emisoil, totCondSum, rwork2;
  double tgav, tiav, tuav, tsav, tlav, quav, qlav, qgav, qiav, qsav;
  List plant, res;
  int nqn;
  bool plantActive;
  double plantCanopy;
  
  fradu = 0;
  frads = 0;
  fradl = 0;
  qu = 0;
  qs = 0;
  ql = 0;
  qg = 0;
  qi = 0;
  dqu = 0;
  dqs = 0;
  dql = 0;
  dqg = 0;
  dqi = 0;
  tupre = 0;
  tspre = 0;
  tlpre = 0;
  tgpre = 0;
  tipre = 0;
  suw = 0;
  ssw = 0;
  slw = 0;
  sut = 0;
  slt = 0;
  slt0 = 0;
  suh = 0;
  ssh = 0;
  slh = 0;
  qgfac = 0;
  qgfac0 = 0;
  
  nqn = 9;
  
  NumericMatrix arr(nqn,nqn);
  NumericVector rhs(nqn);
  NumericVector vec(nqn);
  
  NumericMatrix mplate;
  NumericVector aux;
  double dataAux[nqn*nqn] = {1,  0,  0,  1,  0,  1,  0,  0,  0,
                             0,  1,  0,  1,  0,  1,  0,  0,  0,
                             0,  0,  1,  0,  1,  0,  1,  0,  0,
                             1,  1,  0,  1,  1,  0,  0,  0,  0,
                             0,  0,  1,  1,  1,  0,  0,  1,  1,
                             1,  1,  0,  0,  0,  1,  1,  0,  0,
                             0,  0,  1,  0,  0,  1,  1,  1,  1,
                             0,  0,  0,  0,  1,  0,  1,  1,  0,
                             0,  0,  0,  0,  1,  0,  1,  0,  1};
  
  aux = NumericVector(dataAux,dataAux + sizeof(dataAux)/sizeof(*dataAux));
  aux.attr("dim") = Dimension(nqn, nqn);
  
  mplate = as<NumericMatrix>(aux);
  if(iter == 1) {
    
    xu = 2 * lai[1] * fu;
    xs = 2 * sai[1] * fu;
    xl = 2 * (lai[0] + sai[0]) * fl * (1 - fi);
    
    chux = chu + ch2o * wliqu + cice  * wsnou;
    chsx = chs + ch2o * wliqs + cice  * wsnos;
    chlx = chl + ch2o * wliql + cice  * wsnol;
    
    
    rwork = poros[0] * rhow;
    
    chgx = ch2o * wpud + cice  * wipud + ((1 - poros[0]) * csoi[0] * rhosoi[0] + rwork * (1 - wisoi[0]) * wsoi[0] * ch2o + rwork * wisoi[0] * cice) * hsoi[0];
    
    wlgx = wpud + rwork * (1 - wisoi[0]) * wsoi[0] * hsoi[0];
    
    wigx = wipud + rwork * wisoi[0] * hsoi[0];
    
    cog = consoi[0] / (0.5 * hsoi[0]);
    coi = consno / (0.5 * std::max(hsno[0], hsnotop));
    
    rwork = 4 * 0.95 * stef;
    
    zirg = rwork * (pow(tg, 3));
    ziri = rwork * (pow(ti, 3));
    
    tuold = tu;
    tsold = ts;
    tlold = tl;
    tgold = tg;
    tiold = ti;
  }
  
  wu = impexpCpp (env, wu, tu, chux, wliqu, wsnou, iter);
  ws = impexpCpp (env, ws, ts, chsx, wliqs, wsnos, iter);
  wl = impexpCpp (env, wl, tl, chlx, wliql, wsnol, iter);
  wg = impexpCpp (env, wg, tg, chgx, wlgx,  wigx,  iter);
  
  wi = impexp2Cpp (env, wi, ti, tiold, iter);
  
  tu = wu * tu + (1 - wu) * tuold;
  ts = ws * ts + (1 - ws) * tsold;
  tl = wl * tl + (1 - wl) * tlold;
  tg = wg * tg + (1 - wg) * tgold;
  ti = wi * ti + (1 - wi) * tiold;
  
  tupre = tu;
  tspre = ts;
  tlpre = tl;
  tgpre = tg;
  tipre = ti;
  
  e = cpp_esat(tu);
  qu = cpp_qsat(e, psurf);
  dqu = cpp_dqsat(tu, qu);
  dqu = std::min(dqu, qu * 0.1);
  
  e = cpp_esat(ts);
  qs = cpp_qsat(e, psurf);
  dqs = cpp_dqsat(ts, qs);
  dqs = std::min(dqs, qs * 0.1);
  
  e = cpp_esat(tl);
  ql = cpp_qsat(e, psurf);
  dql = cpp_dqsat(tl, ql);
  dql = std::min(dql, ql * 0.1);
  
  e = cpp_esat(tg);
  qg = cpp_qsat(e, psurf);
  dqg = cpp_dqsat(tg, qg);
  dqg = std::min(dqg, qg * 0.1);
  
  e = cpp_esat(ti);
  qi = cpp_qsat(e, psurf);
  dqi = cpp_dqsat(ti, qi);
  dqi = std::min(dqi, qi * 0.1);
  
  zwpud = std::max(0.0, std::min(0.5, 0.5 * (wpud + wipud) / wpudmax) );
  zwsoi = (1 - wisoi[0]) * wsoi[0] + wisoi[0];
  zwtot = zwpud + (1 - zwpud) * zwsoi;
  
  psig =  -grav * suction[0] * (pow(zwtot, (-bex[0])));
  hfac = std::exp(psig / (rvap * tg));
  
  hfac2 = q34 / qg;
  
  zwopt = 1;
  zwdry = swilt[0];
  betaw = std::max(0.0, std::min(1.0, (zwtot - zwdry) / (zwopt - zwdry)) );
  
  if(tg <= 273.16 || fi > 0) {
    betaw = 0.01;
  }
  
  emisoil = 0.95;
  e = cpp_esat(t34);
  qs1 = cpp_qsat(e, psurf);
  dqs1 = cpp_dqsat(t34, qs1);
  xnumer = hvap * dqs1;
  xdenom = cp + (4 * emisoil * stef * pow((t34), 3)) / sg;
  betafac = xnumer / xdenom;
  betas = betaw / (1 + betafac * (1 - betaw));
  
  qgfac0 = betas * hfac + (1 - betas) * hfac2;
  
  fwetux = fwetu;
  
  if (q12 > qu) 
    fwetux = 1;
  
  fwetsx = fwets;
  
  if (q12 > qs) 
    fwetsx = 1;
  
  fwetlx = fwetl;
  
  if (q34 > ql) 
    fwetlx = 1;
  
  qgfac = qgfac0;
  
  if (q34 > qg) 
    qgfac = 1;
  
  fradu = 0;
  
  if (lai[1] > epsilon)
    fradu = (solu + firu) / (2 * lai[1]);
  
  frads = 0;
  
  if (sai[1] > epsilon)
    frads = (sols + firs) / (2 * sai[1]);
  
  fradl = 0;
  
  if ((lai[0] + sai[0]) > epsilon)
    fradl = (soll + firl) / (2 * (lai[0] + sai[0]));
  
  suw = std::min( fwetux * su, 0.8 * (wliqu + wsnou) / std::max(dtime  * (qu - q12), epsilon));
  
  totCondSum = 0;
  for(int i = 0; i < npft; i++) {
    plant = plantList[i];
    plantActive = plant["active"];
    plantCanopy = plant["canopy"];
    if(plantActive){
      if(plantCanopy == upper)
        totCondSum = totCondSum + totcond[i] * frac[i];
    }
  }
  sut = (1 - fwetux) * 0.5 * totCondSum;
  
  sut = std::max(0.0, sut);
  
  suh = suw * (rliqu * cpp_hvapf(env, tu, ta) + (1 - rliqu) * cpp_hsubf(env, tu, ta)) + sut * cpp_hvapf(env, tu, ta);
  
  ssw = std::min(fwetsx * ss,  0.8 * (wliqs + wsnos) / std::max(dtime  * (qs - q12), epsilon));
  
  ssh = ssw * (rliqs * cpp_hvapf(env, ts, ta) + (1 - rliqs) * cpp_hsubf(env, ts, ta));
  
  slw = std::min(fwetlx * sl * std::min(0.1, greenfracl), 0.8 * (wliql + wsnol) / std::max(dtime  * (ql - q34), epsilon));
  
  totCondSum = 0;
  for(int i = 0; i < npft; i++) {
    plant = plantList[i];
    plantActive = plant["active"];
    plantCanopy = plant["canopy"];
    if(plantActive){
      if(plantCanopy == lower)
        totCondSum = totCondSum + totcond[i] * frac[i];
    }
  }
  slt0 = (1 - fwetlx) * 0.5 * totCondSum;
  
  slt0 = std::max(0.0, slt0);
  
  slt = slt0 * lai[0] / std::max(lai[0] + sai[0], epsilon);
  
  slh = slw * (rliql * cpp_hvapf(env, tl, ta) + (1 - rliql) * cpp_hsubf(env, tl, ta)) + slt * cpp_hvapf(env, tl, ta);
  
  rwork = 1 / dtime;
  
  rwork2 = su * cp;
  arr(0,0) = chux * rwork + wu * rwork2 + wu * suh * dqu;
  arr(0,3) = -rwork2;
  arr(0,5) = -suh;
  rhs[0] = tuold * chux * rwork - (1 - wu) * rwork2 * tu - suh * (qu - wu * dqu * tu) + fradu - pfluxu;
  
  rwork2 = ss * cp;
  arr(1,1) = chsx * rwork + ws * rwork2 + ws * ssh * dqs;
  arr(1,3) = -rwork2;
  arr(1,5) = -ssh;
  rhs[1] = tsold * chsx * rwork - (1 - ws) * rwork2 * ts - ssh * (qs - ws * dqs * ts) + frads - pfluxs;
  
  rwork2 = sl * cp;
  arr(2,2) = chlx * rwork + wl * rwork2 + wl * slh * dql;
  arr(2,4) = -rwork2;
  arr(2,6) = -slh;
  rhs[2] = tlold * chlx * rwork - (1 - wl) * rwork2 * tl - slh * (ql - wl * dql * tl) + fradl - pfluxl;
  
  rwork = xu * su;
  rwork2 = xs * ss;
  arr(3,0) =  - wu * rwork;
  arr(3,1) =  - ws * rwork2;
  arr(3,3) = cu + cl + rwork + rwork2;
  arr(3,4) =  - cl;
  rhs[3] = cu * ta * tfac + (1 - wu) * rwork * tu + (1 - ws) * rwork2 * ts;
  
  rwork = xl * sl;
  rwork2 = fi * si;
  arr(4,2) =  - wl * rwork;
  arr(4,3) =  - cl;
  arr(4,4) = cl + rwork +(1 - fi) * sg + rwork2;
  arr(4,7) =  - wg * (1 - fi) * sg;
  arr(4,8) =  - wi * rwork2;
  rhs[4] = (1 - wl) * rwork * tl + (1 - wg) * (1 - fi) * sg * tg + (1 - wi) * rwork2 * ti;
  
  rwork = xu * (suw + sut);
  rwork2 = xs * ssw;
  arr(5,0) = -wu * rwork * dqu;
  arr(5,1) = -ws * rwork2 * dqs;
  arr(5,5) = cu + cl + rwork + rwork2;
  arr(5,6) =  - cl;
  rhs[5] = cu * qa + rwork * (qu - wu * dqu * tu) +rwork2 * (qs - ws * dqs * ts);
  
  rwork = xl * (slw + slt);
  rwork2 = (1 - fi) * sg;
  arr(6,2) = -wl * rwork * dql;
  arr(6,5) = -cl;
  arr(6,6) = cl + rwork + rwork2 + fi * si;
  arr(6,7) = -wg * rwork2 * qgfac * dqg;
  arr(6,8) = -wi * fi * si * dqi;
  rhs[6] = rwork * (ql -wl * dql * tl) + rwork2 * qgfac * (qg - wg * dqg * tg) + fi * si * (qi -wi * dqi * ti);
  
  rwork = sg * cp;
  rwork2 = sg * hvasug;
  arr(7,4) = -rwork;
  arr(7,6) = -rwork2;
  arr(7,7) = rwork + rwork2 * qgfac * dqg + cog + zirg;
  rhs[7] = -rwork2 * qgfac * (qg - dqg * tg) +  cog * tsoi[0] + solg + firg + zirg * tgold;
  
  rwork = si * cp;
  rwork2 = si * hvasui;
  arr(8,4) =  - rwork;
  arr(8,6) =  - rwork2;
  arr(8,8) = rwork + rwork2 * dqi + coi + ziri;
  rhs[8] =  - rwork2 * (qi - dqi * ti) + coi * tsno[0] +  soli + firi + ziri * tiold;
  
  res = linsolveCpp(arr, rhs, vec, mplate, nqn);
  vec = res["vec"];
  rhs = res["rhs"];
  
  tu = vec[0];
  ts = vec[1];
  tl = vec[2];
  t12 = vec[3];
  t34 = vec[4];
  q12 = vec[5];
  q34 = vec[6];
  tg = vec[7];
  ti = vec[8];
  
  if (iter < niter) {
    env.assign("xu", xu);
    env.assign("xs", xs);
    env.assign("xl", xl);
    env.assign("chux", chux);
    env.assign("chsx", chsx);
    env.assign("chlx", chlx);
    env.assign("chgx", chgx);
    env.assign("wlgx", wlgx);
    env.assign("wigx", wigx);
    env.assign("cog", cog);
    env.assign("coi", coi);
    env.assign("zirg", zirg);
    env.assign("ziri", ziri);
    env.assign("wu", wu);
    env.assign("ws", ws);
    env.assign("wl", wl);
    env.assign("wg", wg);
    env.assign("wi", wi);
    env.assign("tuold", tuold);
    env.assign("tsold", tsold);
    env.assign("tlold", tlold);
    env.assign("tgold", tgold);
    env.assign("tiold", tiold);
    env.assign("tu", tu);
    env.assign("ts", ts);
    env.assign("tl", tl);
    env.assign("tg", tg);
    env.assign("ti", ti);
    env.assign("fwetux", fwetux);
    env.assign("fwetsx", fwetsx);
    env.assign("fwetlx", fwetlx);
    env.assign("t12", t12);
    env.assign("t34", t34);
    env.assign("q12", q12);
    env.assign("q34", q34);
    env.assign("fsena", fsena);
    env.assign("fseng", fseng);
    env.assign("fseni", fseni);
    env.assign("fsenu", fsenu);
    env.assign("fsens", fsens);
    env.assign("fsenl", fsenl);
    env.assign("fvapa", fvapa);
    env.assign("fvapuw", fvapuw);
    env.assign("fvaput", fvaput);
    env.assign("fvaps", fvaps);
    env.assign("fvaplw", fvaplw);
    env.assign("fvaplt", fvaplt);
    env.assign("fvapg", fvapg);
    env.assign("fvapi", fvapi);
    env.assign("firg", firg);
    env.assign("firi", firi);
    env.assign("firb", firb);
    env.assign("upsoiu", upsoiu);
    env.assign("upsoil", upsoil);
    env.assign("ginvap", ginvap);
    env.assign("gsuvap", gsuvap);
    env.assign("gtrans", gtrans);
    env.assign("gtransu", gtransu);
    env.assign("gtransl", gtransl);
    return;
  }
  
  fsena = cp * cu * (ta * tfac - t12);
  
  tgav = wg * tg + (1 - wg) * tgpre;
  fseng = cp * sg * (tgav - t34);
  
  tiav = wi * ti + (1 - wi) * tipre;
  fseni = cp * si * (tiav - t34);
  
  tuav = wu * tu + (1 - wu) * tupre;
  fsenu = cp * su * (tuav - t12);
  
  tsav = ws * ts + (1 - ws) * tspre;
  fsens = cp * ss * (tsav - t12);
  
  tlav = wl * tl + (1 - wl) * tlpre;
  fsenl = cp * sl * (tlav - t12);
  
  fvapa = cu * (qa - q12);
  
  quav = qu + wu * dqu * (tu - tupre);
  fvapuw = suw * (quav - q12);
  fvaput = std::max(0.0, sut * (quav - q12));
  
  qsav = qs + ws * dqs * (ts - tspre);
  fvaps = ssw * (qsav - q12);
  
  qlav = ql + wl * dql * (tl - tlpre);
  fvaplw = slw * (qlav - q34);
  fvaplt = std::max(0.0, slt0 * (qlav - q34));
  
  qgav = qg + wg * dqg * (tg - tgpre);
  fvapg = sg * (qgfac * qgav - q34);
  
  qiav = qi + wi * dqi * (ti - tipre);
  fvapi = si * (qiav - q34);
  
  firg = firg - wg * zirg * (tg - tgold);
  firi = firi - wi * ziri * (ti - tiold);
  firb = firb + (1 - fi) * wg * zirg * (tg - tgold) + fi * wi * ziri * (ti - tiold);
  
  ti = std::min(ti, tmelt);
  
  for(int k= 0;k < nsoilay; k++) {
    upsoiu[k] = fvaput * 2 * lai[1] * fu * stressu[k] / std::max(stresstu, epsilon);
    upsoil[k] = fvaplt * 2 * lai[0] * fl * (1 - fi) * stressl[k] / std::max(stresstl, epsilon);
  }
  
  ginvap = fvapuw * 2 * lai[1] * fu + fvaps * 2 * sai[1] * fu + fvaplw * 2 * (lai[0] + sai[0]) * fl * (1 - fi);
  
  gsuvap = fvapg * (1 - fi) + fvapi * fi;
  
  gtrans = fvaput * 2 * lai[1] * fu + fvaplt * 2 * lai[0] * fl * (1 - fi);
  gtransu = fvaput * 2 * lai[1] * fu;
  gtransl = fvaplt * 2 * lai[0] * fl * (1 - fi);
  
  env.assign("xu", xu);
  env.assign("xs", xs);
  env.assign("xl", xl);
  env.assign("chux", chux);
  env.assign("chsx", chsx);
  env.assign("chlx", chlx);
  env.assign("chgx", chgx);
  env.assign("wlgx", wlgx);
  env.assign("wigx", wigx);
  env.assign("cog", cog);
  env.assign("coi", coi);
  env.assign("zirg", zirg);
  env.assign("ziri", ziri);
  env.assign("wu", wu);
  env.assign("ws", ws);
  env.assign("wl", wl);
  env.assign("wg", wg);
  env.assign("wi", wi);
  env.assign("tuold", tuold);
  env.assign("tsold", tsold);
  env.assign("tlold", tlold);
  env.assign("tgold", tgold);
  env.assign("tiold", tiold);
  env.assign("tu", tu);
  env.assign("ts", ts);
  env.assign("tl", tl);
  env.assign("tg", tg);
  env.assign("ti", ti);
  env.assign("fwetux", fwetux);
  env.assign("fwetsx", fwetsx);
  env.assign("fwetlx", fwetlx);
  env.assign("t12", t12);
  env.assign("t34", t34);
  env.assign("q12", q12);
  env.assign("q34", q34);
  env.assign("fsena", fsena);
  env.assign("fseng", fseng);
  env.assign("fseni", fseni);
  env.assign("fsenu", fsenu);
  env.assign("fsens", fsens);
  env.assign("fsenl", fsenl);
  env.assign("fvapa", fvapa);
  env.assign("fvapuw", fvapuw);
  env.assign("fvaput", fvaput);
  env.assign("fvaps", fvaps);
  env.assign("fvaplw", fvaplw);
  env.assign("fvaplt", fvaplt);
  env.assign("fvapg", fvapg);
  env.assign("fvapi", fvapi);
  env.assign("firg", firg);
  env.assign("firi", firi);
  env.assign("firb", firb);
  env.assign("upsoiu", upsoiu);
  env.assign("upsoil", upsoil);
  env.assign("ginvap", ginvap);
  env.assign("gsuvap", gsuvap);
  env.assign("gtrans", gtrans);
  env.assign("gtransu", gtransu);
  env.assign("gtransl", gtransl);
  return;
}

// [[Rcpp::export]]
double impexpCpp(Environment env, double wimp, double tveg, double ch, double wliq, double wsno, int iter) {
  // global
  double epsilon = env["epsilon"];
  double hfus = env["hfus"];
  double tmelt = env["tmelt"];
  // local
  double h, z, winew;
  
  if(iter == 1) {
    wimp = 1;
    return(wimp);
  }
  
  h = ch * (tveg - tmelt);
  z = std::max(std::abs(h), epsilon);
  winew = 1;
  
  if (h > epsilon)  
    winew = 1 - std::min(1.0, hfus * wsno / z);
  
  if (h <  -epsilon) 
    winew = 1 - std::min(1.0, hfus * wliq / z);
  
  wimp = 0.5 * (wimp + winew);
  return(wimp);
}

// [[Rcpp::export]]
double impexp2Cpp(Environment env, double wimp, double t, double told, int iter) {
  double epsilon = env["epsilon"];
  double tmelt = env["tmelt"];
  
  if(iter == 1) {
    wimp = 1;
    return(wimp);
  }
  
  if ((t - told) > epsilon) 
    wimp = (tmelt - told) / (t - told);
  
  wimp = std::max(0.0, std::min(1.0, wimp));
  
  return(wimp);
}

// [[Rcpp::export]]
List linsolveCpp(NumericMatrix arr, NumericVector rhs, NumericVector vec, NumericMatrix mplate, int nd) {
  
  int ndx = 9;
  double f = 0;
  
  NumericMatrix mplatex(ndx,ndx);
  
  if(nd > ndx) {
    Rprintf("Number of linsolve eqns %i exceeds limit %i", nd, ndx);
    stop("Number of linsolve eqns %i exceeds limit %i", nd, ndx);
  }
  
  for(int j = 0; j < nd; j++) {
    for(int i = 0; i < nd; i++) {
      mplatex(i,j) = mplate(i,j);
    }
  }
  
  for(int id = 0; id < (nd - 1); id++) {
    for(int i = (id + 1);i < nd; i++) {
      if(mplatex(i,id) != 0) {
        f = arr(i,id) / arr(id,id);
        for(int j = id; j < nd; j++) {
          if(mplatex(id,j) != 0) {
            arr(i,j) = arr(i,j) - f * arr(id,j);
            mplatex(i,j) = 1;
          }
        }
        rhs[i] = rhs[i] - f * rhs[id];
      }
    }
  }
  
  nd = nd - 1;
  for(int id = nd; id >= 0; id--) {
    f = 0;
    if(id < nd) {
      for(int j = id + 1; j <= nd; j++) {
        if(mplatex(id,j) != 0) {
          f = f + arr(id,j) * vec[j];
        }
      }
    }
    vec[id] = (rhs[id] - f) / arr(id,id);
  }
  
  return(List::create(Named("vec") = vec, Named("rhs") = rhs));
}
