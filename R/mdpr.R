#' mdpr: Municipal Data and Pricing
#' 
#' mdpr will be used by traders of fixed income instruments, mainly municipal and US    
#' government bonds. Functionality will include pricing (clean and dirty price, yield to
#' worst, and assorted other functions), risk analysis (dv01, convexity, etc.), and relative
#' value metrics.
#'
#'
#'
#' @docType package
#' @name mdpr
#' @importFrom plyr round_any
#' @importFrom NMOF DEopt
#' @importFrom zoo as.Date
#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df filter mutate dense_rank bind_rows
#' @importFrom lubridate floor_date parse_date_time day days month year years is.Date
NULL