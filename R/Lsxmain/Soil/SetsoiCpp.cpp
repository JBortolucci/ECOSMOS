#include <Rcpp.h>
#include <../../Lsxmain/Comsat.cpp>
using namespace Rcpp;

// [[Rcpp::export]]
void setsoiCpp(Environment env) {
  // global
  NumericVector consoi = env["consoi"];
  NumericVector wsoi = env["wsoi"];
  NumericVector wisoi = env["wisoi"];
  NumericVector tsno = env["tsno"];
  NumericVector tsoi = env["tsoi"];
  NumericVector poros = env["poros"];
  NumericVector qglif = env["qglif"];
  NumericVector fracclay = env["fracclay"];
  NumericVector fracsand = env["fracsand"];
  NumericVector fracsilt = env["fracsilt"];
  double epsilon = env["epsilon"];
  double hvasug = env["hvasug"];
  double hvasui = env["hvasui"];
  double nsoilay = env["nsoilay"];
  double ta = env["ta"];
  double tmelt = env["tmelt"];
  double wipud = env["wipud"];
  double wpud = env["wpud"];
  double wpudmax = env["wpudmax"];
  double zwpmax = env["zwpmax"];
  // local
  double powliq, powice, zcondry, zwpud, zwsoi, rwork1, rwork2, zvap, zsub;
  
  for(int k = 0; k < nsoilay; k++) { 
    powliq = poros[k] * wsoi[k] * (1 - wisoi[k]);
    powice = poros[k] * wisoi[k];
    zcondry = fracsand[k] * 0.300 + fracsilt[k] * 0.265 + fracclay[k] * 0.250; 
    consoi[k] = zcondry * pow((0.56 * 100) , powliq) *  pow((2.24 * 100) , powice);
  }
  zwpud = std::max( 0., std::min(zwpmax, (zwpmax * (wpud + wipud) / wpudmax)) );
  zwsoi = (1 - wisoi[0]) * wsoi[0] + wisoi[0];
  
  if(zwsoi >= epsilon) {
    
    rwork1 = 1 / zwsoi;
    
    if(zwpud >= epsilon) {
      rwork2 = 1 / (wpud + wipud);
      qglif[0] = (1 - zwpud) * (1 - wisoi[0]) * wsoi[0] * rwork1;
      qglif[1] = (1 - zwpud) * wisoi[0] * rwork1;
      qglif[2] = zwpud * wpud * rwork2;
      qglif[3] = zwpud * wipud * rwork2;
    } else {
      qglif[0] = (1 - wisoi[0]) * wsoi[0] * rwork1;
      qglif[1] = wisoi[0] * rwork1;
      qglif[2] = 0;
      qglif[3] = 0;
    }
  } else {
    if(zwpud >= epsilon) {
      rwork2 = 1 / (wpud + wipud);
      qglif[0] = 0;
      qglif[1] = 0;
      qglif[2] = zwpud * wpud * rwork2;
      qglif[3] = zwpud * wipud * rwork2;
    } else {
      if(tsoi[0] >= tmelt) {
        qglif[0] = 0;
        qglif[1] = 0;
        qglif[2] = 1;
        qglif[3] = 0;
      } else {
        qglif[0] = 0;
        qglif[1] = 0;
        qglif[2] = 0;
        qglif[3] = 1;
      }
    }
  }
  zvap = cpp_hvapf(env, tsoi[0], ta);
  zsub = cpp_hsubf(env, tsoi[0], ta);
  
  hvasug = (qglif[0] + qglif[2]) * zvap + (qglif[1] + qglif[3]) * zsub ;
  
  hvasui = cpp_hsubf(env, tsno[0],ta);
  
  env.assign("consoi", consoi);
  env.assign("qglif", qglif);
  env.assign("hvasug", hvasug);
  env.assign("hvasui", hvasui);
}
