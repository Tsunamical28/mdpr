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
#' d = as.Date(c("2015-01-01","2015-08-08"))
#' safe_ifelse(d=="2015-01-01",d-10,d)
#' @export
safe_ifelse <- function(cond, yes, no, class_to_use = class(no)){
  structure(ifelse(cond, yes, no), class = class_to_use)
}
# safe_ifelse <- function(cond, yes, no){  
#   if(cond) yes 
#   else no
# }

#' Vectorized Function for Rounding to a Multiple of any Number
#' 
#' Vectorized version of \code{plyr::round_any}, which is a function that rounds 
#' to multiple of any number. 
#' 
#' @param x vector of numbers to round
#' @param accuracy vector of multiples to round the vector \code{x}
#' @return vector with \code{x} rounded to the nearest \code{accuracy}
#' @examples
#' round_any_v(c(52,199,14),10)
#' @export
round_any_v <- Vectorize(round_any, c("x","accuracy"))


#' Query Database and Return Results
#' 
#' Wrapper for RODBC odbcDriverConnect with default values for cetain
#' parameters. The connection defaults to the production server and
#' the Muni DB. The result is returned as a \code{tbl_df} with strings
#' represented as characters instead of factors, unless it is only one
#' column, in which case it is returned as a vector.
#' 
#' @param query Character string database query in MS SQL SERVER T-SQL format.
#' @param stringsAsFactors Boolean value as in base R
#' @param server Server name as a character string
#' @param database Database name as a character string
#' @param uid User ID as a character string
#' @param pwd Password as a character string
#' @return \code{tbl_df} or vector containing the results of the query
#' @examples
#' qry <- paste("R_LoadMuniDeskPositions '",
#'            as.character(c_close_date),"','", as.character(c_as_of_datetime),"','",
#'            paste(c_exempt_accounts, c_taxable_accounts,
#'            c_tsy_accounts,sep = ",",collapse=","),"'")
#' dbQuery(qry)
#' @export
dbQuery <- function(query, stringsAsFactors = FALSE, 
                    server = c_server, database = c_database, 
                    uid = c_uid, pwd = c_pwd){  
  channel <- odbcDriverConnect(paste0("driver=SQL Server; server=", server, 
                                      "; database=", database ,"; uid=",
                                      uid, ";pwd=", pwd))
  on.exit(odbcClose(channel))
  results <- tbl_df(sqlQuery(channel, query, stringsAsFactors = stringsAsFactors))
#   odbcClose(channel)
  if(dim(results)[2] == 1 | is.na(dim(results)[2])){
    results <- structure(simplify2array(results)[[1]], class = class(results[[1]]))
  }
  results 
}


#' Wrap Text in Apostrophes
#' 
#' Returns its argument wrapped in apostrophes. This can be used
#' when passing dates to SQL SERVER or for other similar 
#' applications.
#' 
#' @param text Text to be wrapped in apostrophes
#' @return The text wrapped in apostrophes
#' @examples
#' sqt("2015-05-01")
#' @export
sqt <- function(text){
  paste0("'", text, "'")
}
