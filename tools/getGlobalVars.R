getGlobalVars <- function(fortran_dir = "D:/Bruno/Documentos/Git/Macro_Ecosystem_Simulator/") {
  # fortran_dir <- "D:/Bruno/Documentos/Git/Macro_Ecosystem_Simulator/"
  
  ###### Get all global variables ######
  
  common_b_files <- list.files(fortran_dir, pattern = "\\.h$", recursive = F)
  
  global_vars <- lapply(common_b_files, function(x) {
    fl_nm <- paste0(fortran_dir, x)
    con <- file(fl_nm, open = "r")
    str <- readLines(con = con)
    close(con)
    str <- paste(str, collapse = "\n")
    
    # Remove comments
    str <- gsub("![^\n]*", "", str)
    str <- gsub("(^|\n)(c|\\*)[^\n]*", "", str)
    
    # Replace >
    str <- gsub("\n\\h*>\\h*", "\\1", str, perl = T)
    str <- gsub("[ \t]*", "", str)
    matches <- stringr::str_match_all(str, "common\\h*(?:\\/[^\\/]*\\/)*((?:\\h*,{0,1}[a-zA-Z0-9_]*)*)")[[1]][,2]
    matches <- paste0(matches, collapse = ",")
    matches <- unlist(strsplit(matches, ","))
    
    if(length(matches) == 0)
      return(NULL)
    else
      return(matches)
  })
  
  globals <- unlist(global_vars)
  print("finding globals...")
  
  return(globals)
}
