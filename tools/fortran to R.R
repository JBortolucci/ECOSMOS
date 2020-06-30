fortran_dir <- "D:/Bruno/Documentos/Git/Macro_Ecosystem_Simulator/"
fortran_file_name <- paste0(fortran_dir, "initial.f")

source("tools/getGlobalVars.R")
global_vars <- getGlobalVars(fortran_dir)

###### Open .f file ######
con <- file(fortran_file_name, open="r")
str <- readLines(con = con)
close(con)

str <- paste(str, collapse = "\n")

###### Get local variables ######
local_vars_str <- stringr::str_match_all(str, "(?<=\n)\\h*(?:real|integer|character)(?!\\s*(?:\\(|function))(?:\\s|>)*(?:[^\n,]*)(?:\\s*,(?:(?:\\s|>)*![^\n]*)*\\s*[^\n,]*)*")
local_vars_str <- paste0(unlist(local_vars_str), collapse = "\n")
local_vars_str <- gsub("![^\n]*", "", local_vars_str)
local_vars_str <- gsub("(^|\n)(c|\\*)[^\n]*", "", local_vars_str)

# Replace >
local_vars_str <- gsub("\n\\h*>\\h*", " \\1", local_vars_str, perl = T)
local_vars_str <- gsub("[ \t]+", " ", local_vars_str)
local_vars_str <- gsub("[ \t]*,[ \t]*", ",", local_vars_str)
local_vars_str <- gsub("(^|\n)[ \t]*", "\n", local_vars_str)
local_vars_str <- gsub("[ \t]*(^|\n)", "\n", local_vars_str)

matches <- stringr::str_match_all(local_vars_str, "(?:character|integer|real)\\h*((?:\\h*,{0,1}[a-zA-Z0-9_()]*)*)")[[1]][,2]
matches <- gsub("\\(.*?\\)", "", matches)
matches <- paste0(matches, collapse = ",")
local_vars <- unlist(strsplit(matches, ","))

###### Concatenate all variables ######
all_variables <- c(local_vars, global_vars)
if(length(which(grepl("^[ \t]*$", all_variables))) > 0)
  all_variables <- all_variables[-which(grepl("^[ \t]*$", all_variables))]

##### Temporary replacements #####
# Temporary replace ! from strings by \1
str <- gsub("!(?=[^'\n]*'(?:[^'\n]*'[^'\n]*')*[^'\n]*\n)", "\1", str, perl = T)
str <- gsub('!(?=[^"\n]*"(?:[^"\n]*"[^"\n]*")*[^"\n]*\n)', "\1", str, perl = T)

# Temporary replace commented ! by \1
while(length(grep("(\nc|!)([^\n!]*?)!", str))!=0) 
  str <- gsub("(\nc|!)([^\n!]*)!", "\\1\\2\1", str)

# Temporary replace commented ( by \2
while(length(grep("(\nc|!)([^\n\\(]*?)(\\()", str))!=0)
  str <- gsub("(\nc|!)([^\n\\(]*?)(\\()", "\\1\\2\2", str)

# Temporary Replace commented ) by \3
while(length(grep("(\nc|!)([^\n\\)]*?)(\\))", str))!=0)
  str <- gsub("(\nc|!)([^\n\\)]*?)(\\))", "\\1\\2\3", str)

# Replace local variable declarations
str_local_ <- gsub("(?<=\n)\\h*(?:real|integer|character)(?!\\s*(?:\\(|function))\\s*(?:[^\n,]*)(?:(?:\\s|>)*,(?:(?:\\s|>)*![^\n]*)*\\s*[^\n,]*)*", "", str, perl = T)

# Replace end
str <- gsub("\n[ \t]*end[ \t]*(\n|$)", "\n}\n", str)

# Replace includes
str <- gsub("\n[\t ]*include '.*?'", "", str, perl = T)

# Replace DO's
str <- gsub("do\\h+(?:[0-9]+\\h+){0,1}([a-z]*)\\h*=\\h*(.*?),([^\n!]*)",
            "for(\\1 in \\2:\\3) { ",
            str, perl = T)

# Replace elseif
str <- gsub("else[ \t]*if[ \t]*(\\(([^()]|(?1))*\\))[ \t]*then", "} else if\\1 {", str, perl = T)

# Replace IF's
str <- gsub("if[ \t]*(\\(([^()]|(?1))*\\))[ \t]*then", "if\\1 {", str, perl = T)

# Replace else
str <- gsub("else(?!\\h*if)", "} else {", str, perl = T)

# Replace endif
str <- gsub("(endif|end[ \t]*if)", "}", str)

# TODO check multiple lines
# Replace print*,...
str <- gsub("print\\s*\\*\\s*,([^\n]*)", "print(paste0(\\1))", str)

# Replace c commentaries
str <- gsub("(\n|^)[cC]", "\n#", str)

# Replace * commentaries
str <- gsub("(\n|^)\\*", "\n#", str)

# Replace ! commentaries
str <- gsub("!", "#", str)

# Replace enddo's and continues
str <- gsub("((\n)[^#\n]*continue|enddo|end[ \t]do)", "\\2}", str)

# Replace >
str <- gsub("\n\\h*>\\h*", "\\1\n", str, perl = T)

# Replace subroutine
str <- gsub("(\n\\h*)subroutine[ \t]*([a-zA-Z_][a-zA-Z0-9_]*)\\h*(\\(([^()]|(?3))*\\))", "\\1\\2 <- function \\3 {", str, perl = T)

# Replace = by <-
str <- gsub("([^=<>])[ \t]*=[ \t]*([^=])", "\\1 <- \\2", str)

# Replace .eq.
str <- gsub("\\h*(\n)*\\.\\h*(\n)*eq\\h*(\n)*\\.\\h*(\n)*", "\\1 == \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*ne\\h*(\n)*\\.\\h*(\n)*", "\\1 \1= \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*ge\\h*(\n)*\\.\\h*(\n)*", "\\1 >= \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*le\\h*(\n)*\\.\\h*(\n)*", "\\1 <= \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*lt\\h*(\n)*\\.\\h*(\n)*", "\\1 < \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*gt\\h*(\n)*\\.\\h*(\n)*", "\\1 > \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*or\\h*(\n)*\\.\\h*(\n)*", "\\1 || \\2\\3\\4", str, perl = T)
str <- gsub("\\h*(\n)*\\.\\h*(\n)*and\\h*(\n)*\\.\\h*(\n)*", "\\1 && \\2\\3\\4", str, perl = T)

# Fix operations after newline
str <- gsub("((\n\\h*)*)((#[^\n]*)(\n\\h*#[^\n]*|\n\\h*)*){0,1}\n\\h*(\\+|\\-|\\*{1,2}|\\/|\1=|={1,2}|<={0,1}|>={0,1}|\\|\\||&&)",
            "\\6\\1 \\3\n", str, perl = T)

# Fix operator spacing
str <- gsub("([^e*+<\\/-])\\h*(\\+|\\-|\\*{1,2}|\\/|\1=|={1,2}|<={0,1}|>={0,1}|\\|\\||&&)\\h*([^*+\\/\n-])", "\\1 \\2 \\3", str, perl = T)

##### Restore temporary replacements
str <- gsub("\1", "!", str)
str <- gsub("\2", "(", str)
str <- gsub("\3", ")", str)

lapply(all_variables, function(x) {
  regex <- paste0("((?<![a-zA-Z0-9_])", x, ")\\h*(\\(((?:[^()]|(?2))*)\\))")
  str <<- gsub(regex, "\\1[\\3]", str, perl = T)
})

# Replace # without comments
# str <- gsub("#(\\h*|#)\n", "\n", str, perl = T)

# Replace returns
str <- gsub("(?<=\n)\\h*return", "return()", str, perl = T)

# Replace [0-9].0*
str <- gsub("([0-9])\\.0*([^0-9])", "\\1\\2", str)

# Remove empty comments
str <- gsub("\\h*#\\h*\n", "\n", str)

con <- file("tools/out.R", open = "w")
writeLines(str, con)
close(con)





