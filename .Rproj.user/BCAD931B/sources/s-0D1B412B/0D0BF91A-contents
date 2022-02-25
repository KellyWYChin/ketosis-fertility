library(roxygen2)
roxygen2::roxygenise()
install.packages("renv")
renv::init()
renv::snapshot()
renv::restore()
install.packages("conda")
library(conda)
library(docstring)
square <- function(x){
  #' Computes the square of the input
  return(x^2)
}
square
?square

square <- function(x){
  #' Squares a number
  #'
  #' Provides the square of the input
  #' @param x The value to be squared
  return(x^2)
}

