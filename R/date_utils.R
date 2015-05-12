days_in_year <- function(date, conv = "Act/Act"){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  if(conv == "30/360") return(360)
  else if(conv == "Act/365") return(365)
  else{
    date <- safe_ifelse(is.Date(date), date,
                        as.Date(parse_date_time(date, c("ymd","mdy"))))
    return(as.numeric(as.Date(ymd(paste(year(date) + 1, "01", "01", sep = "-"))) - 
                        floor_date(date,"year")))
  }
}



dc_act_act <- function(date1, date2){
  date1 <- safe_ifelse(is.Date(date1), date1,
                       as.Date(parse_date_time(date1, c("ymd","mdy"))))
  date2 <- safe_ifelse(is.Date(date2), date2,
                       as.Date(parse_date_time(date2, c("ymd","mdy"))))
  
  return(as.numeric(date2 - date1))
}


dc_30_360 <- function(date1, date2, eom = FALSE){
  date1 <- safe_ifelse(is.Date(date1), date1,
                       as.Date(parse_date_time(date1, c("ymd","mdy"))))
  date2 <- safe_ifelse(is.Date(date2), date2,
                       as.Date(parse_date_time(date2, c("ymd","mdy"))))
  d1 <- day(date1)
  d2 <- day(date2)
  m1 <- month(date1)
  m2 <- month(date2)
  y1 <- year(date1)
  y2 <- year(date2)
  
  if(eom){
    
    if((m1 == 2) & (m2 == 2)){
      if((month(date1 + days(1)) == 3) & (month(date2 + days(1)) == 3)){d2 <- 30}
    }
    
    if(m1 == 2){
      if(month(date1 + days(1)) == 3){d1 <- 30}
    }    
    
  }
  
  if(d2 == 31 & ((d1 == 30) | (d1 == 31))){d2 <- 30}
  
  if(d1 == 31){d1 <- 30}
  
  dc <- (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)
  
  return(dc)
}

dc <- function(date1, date2, conv = "30/360", eom = FALSE){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  switch(conv, 
         "30/360" = dc_30_360(date1, date2, eom),
         "Act/Act" = dc_act_act(date1, date2),
         "Act/365" = dc_act_act(date1, date2))  
}

yearfrac <- function(date1, date2, conv = "Act/Act", eom = FALSE){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  if(conv == "30/360") return(dc(date1, date2, conv, eom) / 360)
  else if(conv == "Act/365") return(dc(date1, date2, conv, eom) / 365)
  else{
    date1 <- safe_ifelse(is.Date(date1), date1,
                         as.Date(parse_date_time(date1, c("ymd","mdy"))))
    date2 <- safe_ifelse(is.Date(date2), date2,
                         as.Date(parse_date_time(date2, c("ymd","mdy"))))
    
    if(date1 > date2) return(-yearfrac(date2, date1, conv, eom))    
    
    y1 <- year(date1)
    y2 <- year(date2)
    
    
    dib1 <- days_in_year(date1)
    dib2 <- days_in_year(date2)
    
    sm <- y2 - y1 -1    
    sm <- sm + dc(date1, as.Date(ymd(paste(y1 + 1, 01, 01, sep = "-"))), "Act/Act")/dib1
    sm <- sm + dc(as.Date(ymd(paste(y2,01,01, sep = "-"))), date2, "Act/Act")/dib2
    return(as.numeric(sm))
  }
}

dc_vec <- Vectorize(dc, c("date1", "date2"))
yearfrac_vec <- Vectorize(yearfrac, c("date1", "date2"))