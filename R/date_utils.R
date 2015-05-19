# Date Utility Functions --------------------------------------------------


#' Parse Date If Necessary
#' 
#' Since atomic objects can only have one type, it is unnecessary to check the type of
#' each element in the vector. This function checks the type on the vector level to 
#' determine whether it needs to operate \code{parse_date_time} for \code{d} on a 
#' vectorized basis.
#' 
#' @param d vector of elements that may or may not be type Date
#' @return Date type vector with NA for a particular element in \code{d} if it were 
#' not parsable
#' @examples
#' try_parse_date(as.Date(c("2015-01-01","2015-08-08")))
#' try_parse_date(c("2015-01-01","blah"))
#' @export
try_parse_date <- function (d) {
  #Test on the vector level if the type is Date  
  if(!is.Date(d)){
    if(!lubridate::is.POSIXt(d)){
      d <- parse_date_time(d, c("ymd","mdy"))
    }
    d <- as.Date(d)
  }
  d
}

#' Count Days in Year
#' 
#' Counts the number of days in year via the provided day count convention.
#' 
#' @param vector of dates 
#' @param conv the day count convention can be either "30/360", "Act/365", or 
#' "Act/Act" (default)
#' @return number of days in year
#' @examples
#' d <- as.Date(c("2015-01-01","2015-08-08"))
#' days_in_year(d, conv = "30/360")
#' @export
days_in_year <- function(date, conv = "Act/Act"){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  if(conv == "30/360") return(rep.int(360, length(date)))
  else if(conv == "Act/365") return(rep.int(365, length(date)))
  else{
    date <- try_parse_date(date)
    return(as.numeric(as.Date(ISOdate(year(date) + 1, 1, 1)) - floor_date(date,"year")))
  }
}


dc_act_act <- function(date1, date2){
  date1 <- try_parse_date(date1)
  date2 <- try_parse_date(date2)
  
  return(as.numeric(date2 - date1))
}


dc_30_360 <- function(date1, date2, eom = FALSE){
  date1 <- try_parse_date(date1)
  date2 <- try_parse_date(date2)
  
  d1 <- day(date1)
  d2 <- day(date2)
  m1 <- month(date1)
  m2 <- month(date2)
  y1 <- year(date1)
  y2 <- year(date2)
  
  if(eom){
    #If both date1 and date2 is February EOM, then Make d2 = 30
    d2 <- mapply(function(m1, m2, date1, date2, d2){
                  if((m1 == 2) & (m2 == 2)){
                    if((month(date1 + days(1)) == 3) & 
                         (month(date2 + days(1)) == 3)){
                      return(30)
                    }    
                  }
                  d2
                }, m1, m2, date1, date2, d2)
    
    #If date1 is February EOM, then make d1 = 30
    d1 <- mapply(function(m1, date1, d1){
                  if(m1 == 2){
                    if(month(date1 + days(1)) == 3){
                      return(30)
                    }
                  } 
                  d1
                }, m1, date1, d1)
  }
  
  d2 <- mapply(function(d1, d2){
                if(d2 == 31 & ((d1 == 30) | (d1 == 31))){
                  return(30)
                }
                d2
              }, d1, d2)
  
  d1 <- vapply(d1, function(d1){
                    if(d1 == 31){
                      return(30)
                    }
                    d1
                  }, numeric(length(1))
              )
  
  dc <- (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)
  
  dc
}

#' Day Count
#' 
#' Counts the number of days between \code{date1} and \code{date2} via the provided day 
#' count convention. For the 30/360 day count convention, it is assumed that there are
#' 30 days in a month. If the bond is EOM, which is available only for the 30/360 day count 
#' convention, in a regular year the 3 days between February 28 and March 1 is assigned 
#' to February, otherwise to March. In a leap year, only 2 days is assigned to February 
#' for EOM. If the \code{eom} parameter is set to \code{TRUE}, then two additional rules 
#' will apply to EOM February dates to achieve the aforementioned behavior.
#' 
#' @param date1 starting date to begin counting
#' @param date2 ending date to stop counting
#' @param conv the day count convention can be either "30/360" (default), "Act/365", 
#' or "Act/Act" 
#' @param eom option to assign additional days to February for the 30/360 day count 
#' convention
#' @return number of days between \code{date1} and \code{date2}
#' @examples
#' d1 <- as.Date(c("2004-02-25","2003-02-28")
#' d2 <- as.Date(c("2015-01-01","2015-08-08")
#' dc(d1, d2, conv = "30/360")
#' dc(d1, d2, conv = "30/360", eom = TRUE)
#' 
#' @aliases dc_act_act dc_30_360
#' @export
dc <- function(date1, date2, conv = "30/360", eom = FALSE){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  switch(conv, 
         "30/360" = mapply(dc_30_360, date1, date2, eom, USE.NAMES = FALSE),
         "Act/Act" = mapply(dc_act_act, date1, date2, USE.NAMES = FALSE),
         "Act/365" = mapply(dc_act_act, date1, date2, USE.NAMES = FALSE))  
}



#' Duration Between Two Dates in Fractional Year Terms
#' 
#' Evaluates the duration between \code{date1} and \code{date2} in fractional year terms
#' via the provided day count convention. For the 30/360 day count convention, it is assumed that there are
#' 30 days in a month. If the bond is EOM, which is available only for the 30/360 day count 
#' convention, in a regular year the 3 days between February 28 and March 1 is assigned 
#' to February, otherwise to March. In a leap year, only 2 days is assigned to February 
#' for EOM. If the \code{eom} parameter is set to \code{TRUE}, then two additional rules 
#' will apply to EOM February dates to achieve the aforementioned behavior.
#' 
#' @param date1 starting date
#' @param date2 ending date
#' @param conv the day count convention can be either "30/360", "Act/365", or "Act/Act" (default)
#' @param eom option to assign additional days to February for the 30/360 day count convention
#' @return fractional years between \code{date1} and \code{date2}
#' @examples
#' d1 <- as.Date(c("2004-02-25","2003-02-28")
#' d2 <- as.Date(c("2015-01-01","2015-08-08")
#' yearfrac(d1, d2, conv = "30/360")
#' yearfrac(d1, d2, conv = "30/360", eom = TRUE)
#' @export
yearfrac <- function(date1, date2, conv = "Act/Act", eom = FALSE){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  if(conv == "30/360") return(dc(date1, date2, conv, eom) / 360)
  else if(conv == "Act/365") return(dc(date1, date2, conv, eom) / 365)
  else{
    date1 <- try_parse_date(date1)
    date2 <- try_parse_date(date2)
    
    reverse_dates <- ifelse(date1 > date2, TRUE, FALSE)
    tmp_dates <- date1[reverse_dates]
    date1[reverse_dates] <- date2[reverse_dates]
    date2[reverse_dates] <- tmp_dates
    
    y1 <- year(date1)
    y2 <- year(date2)
    
    
    dib1 <- days_in_year(date1)
    dib2 <- days_in_year(date2)
    
    sm <- y2 - y1 -1    
    sm <- sm + dc(date1, as.Date(ISOdate(y1 + 1, 1, 1)), "Act/Act")/dib1
    sm <- sm + dc(as.Date(ISOdate(y2, 1, 1)), date2, "Act/Act")/dib2
    sm <- as.numeric(sm) * ifelse(reverse_dates, -1, 1)
    sm
  }

}



