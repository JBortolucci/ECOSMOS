#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void solarfCpp(Environment env, int ibb) {
  // global 
  NumericVector lai = env["lai"];
  NumericVector sai = env["sai"];
  NumericVector scalcoefl = env["scalcoefl"];
  NumericVector scalcoefu = env["scalcoefu"];
  NumericVector solad = env["solad"];
  NumericVector solai = env["solai"];
  NumericVector terml = env["terml"];
  NumericVector termu = env["termu"];
  double ablod = env["ablod"];
  double abloi = env["abloi"];
  double abupd = env["abupd"];
  double abupi = env["abupi"];
  double albsnd = env["albsnd"];
  double albsni = env["albsni"];
  double albsod = env["albsod"];
  double albsoi = env["albsoi"];
  double epsilon = env["epsilon"];
  double fl = env["fl"];
  double flodd = env["flodd"];
  double flodi = env["flodi"];
  double floii = env["floii"];
  double fu = env["fu"];
  double fupdd = env["fupdd"];
  double fupdi = env["fupdi"];
  double fupii = env["fupii"];
  double indsol = env["indsol"];
  double nsol = env["nsol"];
  double sol2d = env["sol2d"];
  double sol2i = env["sol2i"];
  double sol3d = env["sol3d"];
  double sol3i = env["sol3i"];
  double solg = env["solg"];
  double soli = env["soli"];
  double soll = env["soll"];
  double sols = env["sols"];
  double solsoi = env["solsoi"];
  double solu = env["solu"];
  double topparl = env["topparl"];
  double topparu = env["topparu"];
  // local 
  int ib = ibb - 1;
  double i, x, y, xd, xi, xaiu, xail;
  
  if (nsol == 0) return;
  
  for (int j = 0;j < nsol; j++) {
    i = indsol;
    x = solad[ib] * abupd + solai[ib] * abupi;
    y = lai(i-1,1) / std::max(lai(i-1,1) + sai(i-1,1), epsilon);
    solu = solu + x * y;
    sols = sols + x * (1. - y);
    sol2d = solad[ib] * fupdd;
    sol2i = solad[ib] * fupdi + solai[ib] * fupii;
  }
  
  for (int j = 0;j < nsol; j++) {
    i = indsol;
    sol3d = fu * sol2d + (1. - fu) * solad[ib];
    sol3i = fu * sol2i + (1. - fu) * solai[ib];
  }
  
  for (int j = 0;j < nsol; j++) {
    i = indsol;
    
    soll = soll + sol3d * ablod + sol3i * abloi;
    
    xd = (fl * flodd + 1.-fl) * sol3d;
    
    xi = fl * (sol3d * flodi + sol3i * floii) + (1. - fl) * sol3i;
    
    solg = solg + (1. - albsod) * xd + (1. - albsoi) * xi;
    
    solsoi = solsoi + xd + xi;
    
    soli = soli + (1. - albsnd) * sol3d + (1. - albsni) * sol3i;
  }
  
  if (ib == 0) {
    
    for (int j = 0;j < nsol; j++) {
      
      i = indsol;
      
      xaiu = std::max(lai(i-1,1) + sai(i-1,1), epsilon);
      
      scalcoefu[0] = termu[0] * solad[ib];
      
      scalcoefu[1] = termu[1] * solad[ib] + termu[2] * solai[ib];
      
      scalcoefu[2] = termu[3] * solad[ib] + termu[4] * solai[ib];
      
      scalcoefu[3] = scalcoefu[0] + scalcoefu[1] + scalcoefu[2];
      
      topparu = scalcoefu[3] * lai(i-1,1) / xaiu;
      
      xail = std::max(lai(i-1,0) + sai(i-1,0), epsilon);
      
      scalcoefl[0] = terml[0] * sol3d;
      
      scalcoefl[1] = terml[1] * sol3d + terml[2] * sol3i;
      
      scalcoefl[2] = terml[3] * sol3d + terml[4] * sol3i;
      
      scalcoefl[3] = scalcoefl[0] + scalcoefl[1] + scalcoefl[2];
      
      topparl = scalcoefl[3] * lai(i-1,0) / xail;
    }
  }
  
  env.assign("solu", solu);
  env.assign("sols", sols);
  env.assign("sol2d", sol2d);
  env.assign("sol2i", sol2i);
  env.assign("sol3d", sol3d);
  env.assign("sol3i", sol3i);
  env.assign("soll", soll);
  env.assign("solg", solg);
  env.assign("solsoi", solsoi);
  env.assign("soli", soli);
  env.assign("scalcoefu", scalcoefu);
  env.assign("topparu", topparu);
  env.assign("scalcoefl", scalcoefl);
  env.assign("topparl", topparl);
}