# fl_nm <- "R/Weather/diurnal.R"

# source("tools/getGlobalVars.R")
# globals <- getGlobalVars()

con <- file(fl_nm, open = "r")
str <- readLines(con = con)
close(con)
str <- paste(str, collapse = "\n")
str <- gsub("#[^\n]*", "", str)

# assigns <- stringr::str_match_all(str, "([a-zA-Z0-9_]+)\\h*(\\[([^]]|(?2))*\\]){0,1}\\h*<-")
assigns <- regmatches(str, gregexpr("([a-zA-Z0-9_]+)(?=\\h*(\\[([^]]|(?2))*\\]){0,1}\\h*<-)", str, perl = TRUE))
assigns <- unique(unlist(assigns))

global_assigns <- assigns[which(assigns %in% globals)]
print(assigns[which(!assigns %in% globals)])

global_assigns <- lapply(global_assigns, function(x) {
  paste0("assign(\"", x, "\", ", x, ", envir = globalenv())")
})

global_assigns <- unlist(global_assigns)
global_assigns <- c(paste("# Global Assigns", fl_nm, "\n"), global_assigns)
global_assigns <- paste(global_assigns, collapse = "\n")

# con <- file("test.R", open = "w")
# writeLines(global_assigns, con)
# close(con)