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
#' @return \code{tbl_df} containing the results of the query or a vector
#' @examples
#' close_date <- as.Date("2015-05-14")
#' as_of_datetime <- Sys.time()
#' lubridate::hour(as_of_datetime) <- 23;
#' lubridate::minute(as_of_datetime) <- 59;
#' lubridate::second(as_of_datetime) <- 59
#' qry <- paste("R_LoadMuniDeskPositions '",
#'            as.character(close_date),"','", as.character(as_of_datetime),"','",
#'            paste(c_exempt_accounts, c_taxable_accounts,
#'            c_tsy_accounts,sep = ",",collapse=","),"'")
#' dbQuery(qry)
#' @export
dbQuery <- function(query, stringsAsFactors = FALSE, 
                    server = mdpr_globals$c_server, database = c_database, 
                    uid = c_uid, pwd = c_pwd){
  channel <- odbcDriverConnect(paste0("driver=SQL Server; server=", server, 
                                      "; database=", database ,"; uid=",
                                      uid, ";pwd=", pwd))
  results <- tbl_df(sqlQuery(channel, query, stringsAsFactors = stringsAsFactors))
  odbcClose(channel)
  if(length(results) == 1){
    results <- results[[1]]
  }
  results 
}


#' Load Configurations from a File
#' 
#' Given a .txt filename, this will parse out key value pairs separated
#' by "=" and assign the value to the key in a new environment.
#' The file should end in a new line character to avoid any warnings. Users
#' can access the variables with the base R command \code{get()}
#' 
#' @param filename Path and filename to be loaded
#' @return Environment containing all the assigned variables
#' @export
load_config <- function (filename) {
  conf.vars <- new.env()
  tmp <- read.table(filename, sep = "=", stringsAsFactors = FALSE)
  for(i in 1:nrow(tmp)){
    assign(tmp[i, 1], tmp[i, 2], envir = conf.vars)
  }
  conf.vars
}

# Startup defaults --------------------------------------------------------
mdpr_globals <- new.env()
# Database Defaults -----------------------------------------------------
db_configs <- load_config("S:/Old Orchard/Development/R/mdpr_package/db_config.txt")

assign("c_server", db_configs$c_server, mdpr_globals)
assign("c_database", db_configs$c_database, mdpr_globals)
assign("c_uid", db_configs$c_uid, mdpr_globals)
assign("c_pwd", db_configs$c_pwd, mdpr_globals)


# Default dates based on date that package is attached
# c_close_date <- Sys.Date()
# c_close_date <- as.Date(dbQuery(paste0(
#   "SELECT CalendarDate
#   FROM [Architect].[dbo].[CAL_BusDay]
#   WHERE NextBusinessDate = '", c_close_date, "'")))  
# c_as_of_datetime <- Sys.time()
# lubridate::hour(c_as_of_datetime) <- 23;
# lubridate::minute(c_as_of_datetime) <- 59;
# lubridate::second(c_as_of_datetime) <- 59


# Traders and Accounts ----------------------------------------------------

c_trader_inits <- c("RJ", "LF", "MB", "EM", "MXA", "OOC", "GHC")
c_trader_inits <- factor(c_trader_inits, levels = c_trader_inits)
assign("c_trader_inits", c_trader_inits, mdpr_globals)

c_exempt_accounts <- paste0(c_trader_inits, "-EX")
c_exempt_accounts <- factor(c_exempt_accounts, levels = c_exempt_accounts)
assign("c_exempt_accounts", c_exempt_accounts, mdpr_globals)

c_taxable_accounts <- paste0(c_trader_inits, "-TAX")
c_taxable_accounts <- factor(c_taxable_accounts, levels = c_taxable_accounts)
assign("c_taxable_accounts", c_taxable_accounts, mdpr_globals)

c_tsy_accounts <- paste0(c_trader_inits, "-TSY")
c_tsy_accounts <- factor(c_tsy_accounts, levels = c_tsy_accounts)
assign("c_tsy_accounts", c_tsy_accounts, mdpr_globals)


# Bloomberg Fields --------------------------------------------------------

c_muni_bbg_fields <- c("ID_CUSIP","MARKET_SECTOR_DES", "ISSUER_BULK", 
                       "ISSUER_DESCRIPTION_2ND_LINE_BULK", "CPN",
                       "MATURITY","WORKOUT_DT_BID", "NXT_CALL_DT", 
                       "NXT_CALL_PX", "SETTLE_DT", "STATE_CODE",
                       "CPN_TYP","MUNI_TAX_PROV")
assign("c_muni_bbg_fields", c_muni_bbg_fields, mdpr_globals)

c_bma_tenors <- c(1:5, 7, 10, 12, 15, 20, 25, 30)
assign("c_bma_tenors", c_bma_tenors, mdpr_globals)

c_bma_securities <- paste0("USSMSB", c_bma_tenors, " Curncy")
c_bma_securities <- factor(c_bma_securities, levels = c_bma_securities)
assign("c_bma_securities", c_bma_securities, mdpr_globals)

c_bma_bbg_fields <- c("PX_LAST")
assign("c_bma_bbg_fields", c_bma_bbg_fields, mdpr_globals)


c_tsy_bbg_fields <- c("ID_CUSIP", "CPN", "MATURITY","ISSUE_DT", 
                      "SETTLE_DT", "PX_LAST" )
assign("c_tsy_bbg_fields", c_tsy_bbg_fields, mdpr_globals)

c_tsy_hedge_maturities <- c(5, 7, 10, 30)
c_tsy_hedge_letters <- c("","B")
c_tsy_hedges <- paste(mapply(paste0, paste0("CT", c_tsy_hedge_maturities), 
                             rep(c_tsy_hedge_letters, 
                                 each = length(c_tsy_hedge_maturities))), "Govt")
c_tsy_hedges <- factor(c_tsy_hedges, levels = c_tsy_hedges)
assign("c_tsy_hedges", c_tsy_hedges, mdpr_globals)


# Analysis Defaults -------------------------------------------------------

c_curve_shocks <- seq(-1.00, 1.00, by = 0.10)
assign("c_curve_shocks", c_curve_shocks, mdpr_globals)


# Lookup Tables -----------------------------------------------------------
tax_status_lookup <- data.frame(muni_tax_prov = 
                                  c("AMT/ST TAX-EXEMPT",
                                    "AMT/ST TAXABLE",
                                    "AMT/ST&CMWLTH TAX-EXMPT",
                                    "FED & ST TAX-EXEMPT",
                                    "FED AMT FOR INDIVIDUALS",
                                    "FED BF/CMWLTH TAX-EXMPT",
                                    "FED BF/ST & PR TAX-EXMPT",
                                    "FED BQ",
                                    "FED BQ/ST TAX-EXEMPT",
                                    "FED BQ/ST TAXABLE",
                                    "FED TAX-EXEMPT",
                                    "FED TAX-EXEMPT/ST TAXABLE",
                                    "FED TAXABLE",
                                    "FED TAXABLE/CMWLTH EXMPT",
                                    "FED TAXABLE/ST TAX-EXEMPT",
                                    "FED TAXABLE/ST TAXABLE",
                                    "FED TAXABLE/ST&PR EXMPT",
                                    "FED/ST/CMWLTH TAX-EXMPT"),
                                tax_status =
                                  c("EXEMPT",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "TAXABLE",
                                    "TAXABLE",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "EXEMPT",
                                    "TAXABLE",
                                    "TAXABLE",
                                    "TAXABLE",
                                    "TAXABLE",
                                    "TAXABLE",
                                    "EXEMPT")
)

assign("tax_status_lookup", tax_status_lookup, mdpr_globals)

attach(mdpr_globals)

# .onAttach <- function(libname, pkgname){
#   attach(mdpr_globals)
# }



