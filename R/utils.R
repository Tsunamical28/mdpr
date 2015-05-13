# Utility functions -------------------------------------------------------

#' Coalesce to Return First Non-NA
#' 
#' Evaluates the given argument in the order in which they were passed.
#' This function will return the first argument that does NOT evaluate to NA.
#' 
#' @param ... Arguments to evaluate
#' @return  The first non NA argument 
#' @examples
#' coalesce(NA, "second","third",4, NA,"sixth")
#' @export
coalesce <- function(...){
  Reduce(function(x,y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}


#' Vectorized \code{ifelse} with Class Coercion
#' 
#' The normal behavior for the \code{ifelse} function is to return the resulting vector
#' with a class type coerced to be the same as the \code{yes} argument. However, when 
#' a Date type vector is passed as an argument, Dates lose their class when the 
#' condition is evaluated. The resulting vector is of type numeric. 
#' This function \code{safe_ifelse} modifies the normal behavior for Date vectors.
#' Users can specify whether to coerce to any class or the default will be used
#' which is \code{class(no)}.
#' 
#' @param cond the test to determine if yes/no
#' @param yes the value or operation to return if \code{cond} evaluates to \code{TRUE}
#' @param no the value or operation to return if \code{cond} evaluates to \code{FALSE} 
#' @return the resulting vector coerced to class type \code{class_to_use}
#' @examples
#' d = as.Date(c('2015-01-01','2015-08-08'))
#' safe_ifelse(d=='2015-01-01',d-10,d)
#' @export
safe_ifelse <- function(cond, yes, no, class_to_use = class(no)){
  structure(ifelse(cond, yes, no), class = class_to_use)
}


#' Vectorized function for rounding to multiple of any number
#' 
#' Vectorized version of \code{plyr::round_any}, which is a function that rounds 
#' to multiple of any number. 
#' 
#' @param x vector of numbers to round
#' @param accuracy vector of multiples to round the vector \code{x}
#' @return vector with \code{x} rounded to the nearest \code{accuracy}
#' @examples
#' round_any_vec(c(52,199,14),10)
#' @export
round_any_vec <- Vectorize(round_any, c("x","accuracy"))
