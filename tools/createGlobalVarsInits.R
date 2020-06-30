fortran_dir <- "D:/Bruno/Documentos/Git/Macro_Ecosystem_Simulator/"

###### Get all global variables ######
common_b_files <- list.files(fortran_dir, pattern = "\\.h$", recursive = F)
common_b_files <- common_b_files[7]
print(common_b_files)

global_vars <- lapply(common_b_files, function(x) {
  fl_nm <- paste0(fortran_dir, x)
  con <- file(fl_nm, open = "r")
  str <- readLines(con = con)
  close(con)
  str <- paste(str, collapse = "\n")
  
  str <- gsub("![^\n]*", "", str)
  str <- gsub("(^|\n)(c|\\*)[^\n]*", "", str)
  
  # Replace >
  str <- gsub("\n\\h*>\\h*", "", str, perl = T)
  str <- gsub("[ \t]*", "", str)
  matches <- stringr::str_match_all(str, "common\\h*(?:\\/[^\\/]*\\/)*((?:\\h*,{0,1}[a-zA-Z0-9_]*)*)")[[1]][,2]
  matches <- paste0(matches, collapse = ",")
  matches <- unlist(strsplit(matches, ","))

  if(length(matches) == 0)
    return(NULL)
  else
    return(matches)
})

global_vars <- unlist(global_vars)

fl_nm <- paste0(fortran_dir, common_b_files)
con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)
str <- paste(str, collapse = "\n")

# Remove comments
str <- gsub("![^\n]*", "", str)
str <- gsub("(^|\n)(c|\\*)[^\n]*", "", str)

var_declare <- lapply(global_vars, function(x) {
  regex <- paste0("(?<=", x, ")\\h*\\(([^\\)]*)\\)")
  matched <- unlist(stringr::str_match(str, regex))[[2]]
  if(is.na(matched))
    matched <- ""
  # paste0(x, matched)
  c(x, matched)
})

fl_nm <- paste0(fortran_dir, common_b_files)
con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)
str <- paste(str, collapse = "\n")

# Get comments
variables <- lapply(var_declare, function(x){
  if(x[[2]] == "")
    regex <- paste0(x[[1]], "\\h*,{0,1}\\h*!\\h*([^\n]*)")
  else
    regex <- paste0(x[[1]], "\\h*\\(", x[[2]], "\\)\\h*,{0,1}\\h*!\\h*([^\n]*)")
  matched <- stringr::str_match(str, regex)[[2]]
  if(is.na(matched))
    matched <- ""
  c(x[[1]], x[[2]], matched)
})

R_vars <- lapply(variables, function(x) {
  # Count parameters
  if(x[[2]] == "")
    npar <- 0
  else {
    x[[2]] <- gsub("\\h*,\\h*", ", ", x[[2]])
    npar <- stringr::str_count(x[[2]], ",") + 1
  }
  
  if(npar == 0) {
    paste0(x[[1]], " <- 0 # ", x[[3]])
  } else if(npar == 1) {
    paste0(x[[1]], " <- array(0, ", x[[2]], ") # ", x[[3]])
  } else if(npar == 2) {
    paste0(x[[1]], " <- matrix(0, ", x[[2]], ") #", x[[3]])
  } else if(npar >= 3) {
    print("3 DIMENSOES")
    paste0(x[[1]], " <- array(0, dim = c(", x[[2]], ")) # ", x[[3]])
  }
})

R_vars <- unlist(R_vars)
R_vars <- paste(R_vars, collapse = "\n")
R_vars <- paste0("##### ", common_b_files, " #####\n\n", R_vars)
R_vars <- gsub("npoi", "nlpoints", R_vars)

con <- file("inits.R", open = "w")
writeLines(R_vars, con)
close(con)
