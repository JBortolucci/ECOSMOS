#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void outputC(Environment env, String currentDate, int type) {
  StringVector outputNameVars;
  String var;
  double iteratoVal;
  NumericVector vectorValores;
  int oHour = env["oHour"];
  StringVector linhaOutput;
  List out;
  std::string mean = "Mean";
  
  //Horario
  if(type == 1){
    if(oHour == 25){
      oHour = 1;
    }
    outputNameVars = env["outputHourlyList"];
    linhaOutput.push_back(currentDate);
    linhaOutput.push_back(std::to_string(oHour));
    oHour = oHour + 1;
    
    for(int a = 0; a < outputNameVars.length() ; a++) {
      var = outputNameVars[a];
      vectorValores = env[var.get_cstring()];
      
      for(NumericVector::iterator i = vectorValores.begin(); i != vectorValores.end(); ++i) {
        iteratoVal = *i;
        linhaOutput.push_back(std::to_string(iteratoVal));
      }
    }
    
    out = env["outputCHourly"];
    out.push_back(linhaOutput);
    env["outputCHourly"] = out;
    env["oHour"] = oHour;
  }else{
    //Nao Horario
    outputNameVars = env["outputDailyList"];
    linhaOutput.push_back(currentDate);
    
    for(int a = 0; a < outputNameVars.length() ; a++) {
      var = outputNameVars[a];
      mean = "Mean";
      mean = var.get_cstring() + mean;
      vectorValores = env[mean];
      for(NumericVector::iterator i = vectorValores.begin(); i != vectorValores.end(); ++i) {
        iteratoVal = *i;
        linhaOutput.push_back(std::to_string(iteratoVal));
      }
    }
    
    out = env["outputCDaily"];
    out.push_back(linhaOutput);
    env["outputCDaily"] = out;
  }
  
  return;
}