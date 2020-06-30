fl_nm <- paste0("tools/debug/F_log.txt")
con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)

str <- paste(str, collapse = "\n")

str <- gsub("\\h+", ", ", str, perl = T)

str <- gsub("([a-zA-Z_][a-zA-Z_0-9]*)", "\\1 <- matrix( data = c(", str, perl = T)

str <- gsub(",\\h*(\n|$)", "),\n nrow = , ncol = )\n", str, perl = T)

# cat(str)

con <- file("tools/debug/out.R", open = "w")
writeLines(str, con)
close(con)
