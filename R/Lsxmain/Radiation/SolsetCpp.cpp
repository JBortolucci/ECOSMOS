#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void solsetCpp(Environment env) {
  NumericVector asurd = env["asurd"];
  NumericVector asuri = env["asuri"];
  NumericVector scalcoefl = env["scalcoefl"];
  NumericVector scalcoefu = env["scalcoefu"];
  double coszen = env["coszen"];
  double indsol = env["indsol"];
  double nsol = env["nsol"];
  double solg = env["solg"];
  double soli = env["soli"];
  double soll = env["soll"];
  double sols = env["sols"];
  double solsoi = env["solsoi"];
  double solu = env["solu"];
  double topparl = env["topparl"];
  double topparu = env["topparu"];
  
  for(int i=0; i<asurd.length(); ++i){
    asurd[i] = 0;
  }
  for(int i=0; i<asuri.length(); ++i){
    asuri[i] = 0;
  }
  
  solu = 0;
  sols = 0;
  soll = 0;
  solg = 0;
  soli = 0;
  solsoi = 0;
  
  topparu = 0;
  topparl = 0;
  
  for(int i=0; i<scalcoefl.length(); ++i){
    scalcoefl[i] = 0;
  }
  for(int i=0; i<scalcoefu.length(); ++i){
    scalcoefu[i] = 0;
  }
  
  nsol = 0;
  
  
  if(coszen > 0) {
    nsol = nsol + 1;
    indsol = 1;
  }
  
  env.assign("nsol", nsol);
  env.assign("asurd", asurd);
  env.assign("asuri", asuri);
  env.assign("solu", solu);
  env.assign("sols", sols);
  env.assign("soll", soll);
  env.assign("solg", solg);
  env.assign("soli", soli);
  env.assign("solsoi", solsoi);
  env.assign("topparu", topparu);
  env.assign("topparl", topparl);
  env.assign("scalcoefl", scalcoefl);
  env.assign("scalcoefu", scalcoefu);
  env.assign("indsol", indsol);
  
}
