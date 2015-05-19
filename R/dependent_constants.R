# Constants Built after Utils ---------------------------------------------

#' Get Previous Close Date
#' 
#' Uses the Architect DB to get the business date before
#' the argument's business date.
#' 
#' @param date The date to use
#' @return  The previous business date using the SIFMA Calednar
#' @examples
#' get_close_date()
#' @export
get_close_date <- function(date = Sys.Date()){  
    date <- as.Date(dbQuery(paste0(
              "SELECT PriorBusinessDate AS CalendarDate 
                    FROM [Architect].[dbo].[CAL_BusDay]
                    WHERE CalendarDate = ", sqt(date), " AND
                    CalendarTypeID = 2")))
    date
}

#' Get End of Day
#' 
#' Used to get a date with hours and minutes for the last
#' second of the day. This guarantees that all trades on a given
#' date will be captured.
#' 
#' @param date The date to use
#' @return  POSIX variable for the date with hours, minutes, 
#' and seconds equal to 23:59:59
#' @examples
#' get_eod()
#' @export
get_eod <- function(date = Sys.time()){
  date <- try_parse_date(date)
  lubridate::hour(date) <- 23;
  lubridate::minute(date) <- 59;
  lubridate::second(date) <- 59
  date
}

# Default dates based on date that package is attached
c_close_date <- get_close_date()
assign("c_close_date", c_close_date, mdpr_globals)

c_as_of_datetime <- get_eod()
assign("c_as_of_datetime", c_as_of_datetime, mdpr_globals)
attach(mdpr_globals)

