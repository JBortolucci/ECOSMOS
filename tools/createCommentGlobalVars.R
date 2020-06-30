fl_nm <- "tools/out.R"
fortran_dir <- "D:/Bruno/Documentos/Git/Macro_Ecosystem_Simulator/"

source("tools/getGlobalVars.R")
globals <- getGlobalVars()

con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)
str <- paste(str, collapse = "\n")
str <- gsub("#[^\n]*", "", str)

# Find global variables used in file
matched <- lapply(globals, function(x) {
  regex <- paste0("[^a-zA-Z]", x, "[^a-zA-Z0-9_]")
  grepl(regex, str)
})
matched <- unlist(matched)
global_in_file <- globals[which(matched)]

common_b_files <- list.files(fortran_dir, pattern = "\\.h$", recursive = F)
common_b_files <- common_b_files

variables <- NULL
for(x in common_b_files) {
  nm <- paste0(fortran_dir, x)
  con <- file(nm, open = "r")
  str <- readLines(con = con)
  close(con)
  str <- paste(str, collapse = "\n")
  
  matched <- lapply(global_in_file, function(x){
    # x <- "precip"
    regex <- paste0("(?<![a-zA-Z0-9_])", x, "\\h*(\\([^)]*\\)){0,1}\\h*,{0,1}\\h*!\\h*([^\n]*)")
    matched <- stringr::str_match(str, regex)[[3]]
    if(is.na(matched))
      NULL
    else
      c(x, matched)
  })
  
  variables <- c(variables, matched)
}

variables <- variables[which(!sapply(variables, is.null))]

v <- lapply(variables, `[[`, 1)
g <- global_in_file[which(!global_in_file %in% v)]


variables <- c(variables, g)

maxlen <- max(unlist(lapply(v, nchar)))

R_Comm <- lapply(variables, function(x) {
  spaces <- paste(rep(" ", 2 + maxlen - nchar(x[1])), collapse = "")
  if(!is.na(x[2]))
    paste0("# ", x[1], spaces,"# ", x[2])
  else 
    paste0("# ", x[1])
})

R_Comm <- unlist(R_Comm)
R_Comm <- sort(R_Comm)
R_Comm <- paste(R_Comm, collapse = "\n")
R_Comm <- paste0("##### Comment Variables ", fl_nm, " #####\n\n# Global Vars:\n", R_Comm)

source("tools/createGlobalAssigns.R")

out <- paste(R_Comm, global_assigns, sep = "\n\n")

con <- file("tools/comm.R", open = "w")
writeLines(out, con)
close(con)
