source("tools/debug/out.R")
 fl_nm <- paste0("/home/jair/Área de Trabalho/agro_ibis_Xavier_MG/F_log.txt")
#fl_nm <- paste0("D:/Bruno/Documentos/Git/Macro_Ecosystem_Simulator/F_log.txt")

con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)
str <- paste(str, collapse = "\n")

a <- unlist(strsplit(str, "[ \t\n]+", perl = T))[-1]

con <- file("tools/debug/F_log.txt", open = "w")

noNewLine <- T
for(x in a) {
  b <- type.convert(x, numerals = "allow.loss" ,as.is = T)
  
  if(is.character(b)) {
    if(noNewLine) {
      noNewLine <- F
      str <- paste0(b, "\n")
    } else {
      str <- paste0("\n", b, "\n")
    }
  } else {
    str <- sprintf("%.3f     ", b)
  }
  
  writeLines(str, con, sep = "")
}

close(con)

