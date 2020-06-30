source("tools/getGlobalVars.R")
fl_nm <- paste0("R/rd_param.R")
con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)
str <- paste(str, collapse = "\n")

vars <- list()

vars <- getGlobalVars()
# remove unnused large globals:
vars <- vars[-which(vars %in% c("lonscale", "latscale", "work", "iimonth", "iiday","iiyear", "cdummy", "iihour", "var1", "var2", "var3", "var4", "var5", "var6"))]
# dont print large globals:
vars <- vars[-which(vars %in% c("workdir", "lmask", "corndop", "gddcorn", "gddsgc", "gddsoy", "gddwht", "sgcdop", "soydop", "whtdop", "cropout", "cfertmaize", "cfertsgc", "cfertsoy", "cfertwheat", "daydrn", "daygddc", "daygdds", "daygddsgc", "daygddw", "daynconc", "ldiag"))]

# vars <- stringr::str_match_all(str, "(?<=assign\\(\")([a-zA-Z0-9_]+)(?=\")")[[1]][,1]

vars <- unique(vars)

# vars <- globals

fortran_code <- unlist(lapply(vars, function(x) {
  paste0("      write(666,*) \"", x, "\"\n      write(666,*) ", x)
}))

fortran_code <- paste(fortran_code, collapse = "\n")

fortran_code <- paste(
  "      open(unit=666,file=\"F_log.txt\",action=\"write\",status=\"replace\")",
  fortran_code,
  "      close(666)",
  "      stop 9999",
  sep = "\n")
cat(fortran_code)

R_code <- unlist(lapply(vars, function(x) {
  paste0("writeLines(\"", x ,"\", con)\nwriteLines(paste(sprintf(\"%.3f\",", x, "), collapse = \"     \"), con)")
}))

R_code <- paste(R_code, collapse = "\n")

R_code <- paste(
  "con <- file(\"tools/debug/R_log.txt\", open = \"w\")",
  R_code,
  "close(con)",
  sep = "\n")

con <- file("tools/debug/out.f", open = "w")
writeLines(fortran_code, con)
close(con)

con <- file("tools/debug/out.R", open = "w")
writeLines(R_code, con)
close(con)
