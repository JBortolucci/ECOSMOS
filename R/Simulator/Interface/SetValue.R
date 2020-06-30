# 
# library("pryr")
# 
# 
# SetValue <- function(varName = "", varValue = 0) {
#   
#   simInstances[[1]][[varName]] <- varValue
# 
#   assign("simInstances", simInstances, env = globalenv())
#   
# }
# 
# EXEMPLO

# vars <- new.env(parent = globalenv())
# vars$x <- 0
# vars$y <- 1
# vars$env <- vars
# 
# a <- function() {
# 
#   environment(b) <- vars
# 
#   vars$x <- 10
# 
#   b()
# 
# }
# 
# b <- function() {
# 
#   environment(c) <- env
#   c()
# 
# }
# 
# c <- function() {
#   
#   print(environment(env))
#   d()
# 
# }
# 
# d <- function() {
#   
#   print(parent.frame())
#   
# }
# 
# a()

