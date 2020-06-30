#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void irradCpp(Environment env) {
  // global 
  NumericVector sai = env["sai"];
  NumericVector lai = env["lai"];
  double fi = env["fi"];
  double fira = env["fira"];
  double firb = env["firb"];
  double firg = env["firg"];
  double firi = env["firi"];
  double firl = env["firl"];
  double firs = env["firs"];
  double firu = env["firu"];
  double fl = env["fl"];
  double fu = env["fu"];
  double stef = env["stef"];
  double tg = env["tg"];
  double ti = env["ti"];
  double tl = env["tl"];
  double ts = env["ts"];
  double tu = env["tu"];
  // local
  double avmuir;
  double emu;
  double ems;
  double eml;
  double emg;
  double emi;
  double fdown;
  double fdowng;
  double fupg;
  double fupgb;
  double fupi;
  double fup;
  
  emg = 0.95;
  emi = 0.95;
  
  avmuir = 1.0;
  
  emu = 1.0 - std::exp( (-lai[1]) / avmuir );
  ems = 1.0 - std::exp( (-sai[1]) / avmuir );
  eml = 1.0 - std::exp( (-(lai[0] + sai[0])) / avmuir );
  
  fdown =  (1.0 - fu) * fira + fu * ((1.0 - emu) * (1.0 - ems) * fira + emu * (1.0 - ems) * stef * (pow(tu , 4.0)) + ems * stef * (pow(ts , 4.0)));
  
  fdowng = (1.0 - eml) * fdown + eml * stef * (pow(tl , 4.0));
  
  fupg = (1.0 - emg) * fdowng + emg * stef * (pow(tg , 4.0));
  
  fupgb = (1.0 - emg) * fdown + emg * stef * (pow(tg , 4.0));
  
  fupi = (1.0 - emi) * fdown + emi * stef * (pow(ti , 4.0));
  
  fup = (1.0 - fi) * (fl * (eml * stef * (pow(tl , 4.0)) + (1.0 - eml) * fupg) + (1.0 - fl) * fupgb) + fi * fupi;
  
  firb = (1.0 - fu) * fup + fu * ((1.0 - emu) * (1.0 - ems) * fup + emu * stef * (pow(tu , 4.0)) + ems * (1.0 - emu) * stef * (pow(ts , 4.0)));
  
  firu = emu * ems * stef * (pow(ts , 4.0)) + emu * (1.0 - ems) * fup + emu * fira - 2.0 * emu * stef * (pow(tu , 4.0));
  
  firs = ems * emu * stef * (pow(tu , 4.0)) + ems * fup + ems * (1.0 - emu) * fira - 2.0 * ems * stef * (pow(ts , 4.0));
  
  firl = eml * fdown + eml * fupg - 2.0 * eml * stef * (pow(tl , 4.0));
  
  firg = fl * (fdowng - fupg) + (1.0 - fl) * (fdown  - fupgb);
  
  firi = fdown - fupi;
  
  env.assign("firb", firb); 
  env.assign("firu", firu);
  env.assign("firs", firs);
  env.assign("firl", firl);
  env.assign("firg", firg);
  env.assign("firi", firi);
}