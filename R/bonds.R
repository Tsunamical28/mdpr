# Cashflow Functions ------------------------------------------------------

#' Create vector of cashflow amounts
#' 
#' @param num Number of cashflows
#' @param coupon  Constant cashflow amount
#' @param freq  Cashflow frequency (number of periods per year)
#' @param redemption  Redemption value or par amount
#' @return  Vector of cashflow amounts of length \code{num} whose last value is
#'    \eqn{redemption + \frac{coupon}{freq}}{redemption + coupon / freq}.
#' @examples
#' create_cf_amounts(20, 5, 2, 100)
#' @export
create_cf_amounts <- function(num, coupon, freq, redemption = 100){
  cfs <- vector("numeric", length = (num + 1))
  cfs[1] <- 0
  cfs[2:(num + 1)] <- rep(coupon / freq, num)
  cfs[length(cfs)] <- cfs[length(cfs)] + redemption
  cfs
}


#Test
create_cashflows <- function(maturity, coupon, settle, conv = "30/360", 
                             freq = 2, redemption = 100, id = NULL){
  maturity <- safe_ifelse(is.Date(maturity), maturity,
                          as.Date(parse_date_time(maturity, c("ymd","mdy"))))
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  coupon <- as.numeric(coupon)
  
  daysinyear <- 365.25
  maturity_in_years <- as.numeric(maturity - settle) / daysinyear
  coup_num <- round_any_vec(maturity_in_years, 1 / freq, ceiling) * freq
  coupon_dates <- mapply(seq, from = maturity,
                         by = paste0("-", as.integer(12 / freq)," months"), 
                         length.out = (coup_num + 1), SIMPLIFY = F)
  coupon_dates <- lapply(coupon_dates, sort)
  coupon_dates <- lapply(coupon_dates, as.Date)
  cf_amounts <- mapply(create_cf_amounts, num = coup_num, 
                       coupon = coupon, freq = freq,
                       redemption = redemption, SIMPLIFY = F)
  bond_ids <- lapply(
    lapply(seq_along(coupon_dates), 
           function(d){list(len = length(coupon_dates[[d]]),
                            idx = ifelse(is.null(id[[d]]),d,id[[d]]))}),
    function(x){rep(x$idx, x$len)})
  
  cf <- bind_rows(lapply(1:length(coupon_dates), 
                         function(i){
                           tbl_df(data.frame(id = as.character(unlist(bond_ids[i])),
                                             cf_date = do.call("c", coupon_dates[i]), 
                                             cf_amount = unlist(cf_amounts[i]),
                                             stringsAsFactors = FALSE
                           )
                           )
                         }
        )
  ) 
  
  
  return(cf)
  
  
}


bond <- function(maturity, coupon, dated_date, conv = "30/360", freq = 2, 
                 cfs = NULL, cusip = NULL, call_date = NULL, call_px = NULL){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  maturity <- safe_ifelse(is.Date(maturity), maturity,
                          as.Date(parse_date_time(maturity, c("ymd","mdy"))))
  dated_date <- safe_ifelse(is.Date(dated_date), dated_date,
                            as.Date(parse_date_time(dated_date, c("ymd","mdy"))))
  
  if(!is.null(call_date) && !is.na(call_date))
  {
    call_date <- safe_ifelse(is.Date(call_date), call_date,
                             as.Date(parse_date_time(call_date, c("ymd","mdy"))))
    call_px <- ifelse((is.null(call_px) || is.na(call_px)), 100, call_px)
  } else {    
    call_date <- NULL
    call_px <- NULL
  }
  
  
  if(is.null(cfs)){
    cfs_maturity <- create_cashflows(maturity, coupon, dated_date, conv, freq,
                                     id = cusip)
    if(!is.null(call_date))
    {
      cfs_call <- create_cashflows(call_date, coupon, dated_date, conv, freq, 
                                   redemption = call_px, id = cusip)    
    } else{
      cfs_call <- NULL
    }
    
    cfs <- list(maturity = cfs_maturity)
    
    if(!is.null(cfs_call))
    {
      cfs$call <- cfs_call
    }
    
  }
  
  bond <- list(cusip = cusip, maturity = maturity, coupon = coupon, 
               dated_date = dated_date, conv = conv,
               freq = freq, cfs = cfs, call_date = call_date, 
               call_px = call_px)
  
  class(bond) <- "bond"
  return(bond)
}

acc_int <- function(accrual_start, settle, next_coupon, coupon,
                    conv = "30/360", freq = 2){    
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  daysinyear <- days_in_year(settle, conv)
  
  if(conv == "Act/Act"){
    daysinyear <- freq * dc_act_act(accrual_start, next_coupon)
  }
  
  ai_days <- dc(accrual_start, settle, conv)
  
  ai <- (ai_days / daysinyear) * (coupon)
  
  return(ai)
}


discount_cfs_single <- function(cfs, settle, yield = NULL, conv = "30/360", freq = 2, 
                                include_dfPV = TRUE, include_dur = FALSE){
  
  if(!is.null(yield)){
    yield <- yield / 100
  }
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  next_coupon <- (cfs %>%  filter(cf_date > settle) %>% summarise(min(cf_date)))[[1]]
  prev_coupon <- (cfs %>%  filter(cf_date <= settle) %>% summarise(max(cf_date)))[[1]]
  E <- dc(prev_coupon, next_coupon, conv)
  A <- dc(prev_coupon, settle, conv)
  coupon_offset <- (E - A)
  DSC  <- dc(settle, next_coupon, conv)
  daysinyear <- days_in_year(settle, conv)
  
  cfs <- cfs %>% filter(cf_date >= next_coupon)  
  if(include_dfPV &&!is.null(yield))
  {
    if(conv == "Act/Act"){
      cfs <- cfs %>% mutate(index = (dense_rank(cf_date) - 1),
                            days_diff = dc_vec(settle, cf_date, conv),
                            year_frac = (index + (DSC / E))/freq,
                            df = 1 / (1 + yield / freq)^(freq *year_frac),
                            PV = cf_amount * df)
    }
    else{
      cfs <- cfs %>% mutate(index = (dense_rank(cf_date) - 1),
                            days_diff = (daysinyear / freq) * (index) + coupon_offset,
                            year_frac = days_diff / daysinyear,
                            df = 1 / (1 + yield / freq)^(freq * year_frac),
                            PV = cf_amount * df)
    }
    
    if(include_dur){
      dirty_px <- sum(cfs$PV)
      n <- length(cfs)
      cfs <- cfs %>% mutate(macdur = year_frac * PV / dirty_px/100,
                            moddur = macdur / (1 + yield / freq),
                            dv01 = moddur * dirty_px/100,
                            convexity = (1 / dirty_px) * 1 / (1 + yield / freq)^2 *
                              PV * ((year_frac * freq)^2 +
                                      year_frac * freq) / freq^2 /10000)
    }
  }
  else{
    if(conv == "Act/Act"){
      cfs <- cfs %>% mutate(index = (dense_rank(cf_date) - 1),
                            days_diff = dc_vec(settle, cf_date, conv),
                            year_frac = (index + (DSC / E))/freq)
    }
    else{
      cfs <- cfs %>% mutate(index = (dense_rank(cf_date) - 1),
                            days_diff = (daysinyear / freq) * (index) + coupon_offset,
                            year_frac = days_diff / daysinyear)
    }
  }
  return(cfs)
}


calc_dirty_px <- function(x, ...){
  UseMethod("calc_dirty_px")
}


calc_dirty_px.default <- function(maturity, settle, coupon, yield,
                                  conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  tmp_dated_date <- settle - months(6)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  dirty_px <- calc_dirty_px.bond(b, settle, yield)
  return(dirty_px)
  
}


calc_dirty_px.bond <- function(b, settle, yield){
  
  conv <- b$conv
  cfs <-  b$cfs
  freq <- b$freq
  
  
  cfs <- lapply(cfs, discount_cfs_single, settle, yield, conv, freq)
  
  
  if(length(cfs)==1)
  {
    dirty_px <- sum(mapply(`[[`, cfs, "PV"))
    names(dirty_px) <- "maturity"
  }else{
    dirty_px <- lapply(mapply(`[[`, cfs, "PV"), sum)
    worst_index <- which.min(dirty_px)
    worst_type <- names(dirty_px)[worst_index]
    dirty_px <- dirty_px[[worst_index]]
    names(dirty_px) <- worst_type
    attr(dirty_px, "workout_date") <- max(cfs[[worst_index]]$cf_date)
    
  }
  
  return(dirty_px)
}

calc_clean_px <- function(x, ...){
  UseMethod("calc_clean_px")
}

calc_clean_px.default <- function(maturity, settle, coupon, yield,
                                  conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  tmp_dated_date <- settle - months(6)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  
  clean_px <- calc_clean_px.bond(b, settle, yield)
  return(clean_px)  
}

calc_clean_px.bond <- function(b, settle, yield){
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  
  coupon <- b$coupon
  freq <- b$freq
  conv <- b$conv
  cfs <- b$cfs
  
  prev_coupon <- (cfs$mat %>%  filter(cf_date <= settle) %>% summarise(max(cf_date)))[[1]]  
  next_coupon <- (cfs$mat %>%  filter(cf_date > settle) %>% summarise(min(cf_date)))[[1]]
  
  dirty_px <- calc_dirty_px.bond(b, settle, yield)
  ai <- acc_int(prev_coupon, settle, next_coupon, coupon, conv)
  clean_px <- dirty_px - ai
  
  
  names(clean_px) <- names(dirty_px)
  attr(clean_px, "workout_date") <- max(cfs[[names(clean_px)]]$cf_date)
  
  return(clean_px)
  
}

ytm <- function(cf, times, y0 = 0.05,
                tol = 1e-05, h = 1e-05, maxit = 1000L) {        
  dr <- 1
  for (i in seq_len(maxit)) {
    y1 <- 1 + y0
    g <- cf / y1 ^ times
    g <- sum(g)
    t1 <- times - 1
    dg <- times * cf * 1/y1 ^ t1
    dg <- sum(dg)
    dr <- g/dg
    y0 <- y0 + dr
    if (abs(dr) < tol)
      break
  }
  y0
}

yield_cfs_single <- function(cfs, settle, coupon, conv = "30/360", freq = 2, clean_px){
  
  prev_coupon <- (cfs %>%  filter(cf_date <= settle) %>% summarise(max(cf_date)))[[1]]  
  next_coupon <- (cfs %>%  filter(cf_date > settle) %>% summarise(min(cf_date)))[[1]]
  
  cfs <- discount_cfs_single(cfs, settle, yield = NULL, conv, freq, include_dfPV = FALSE)
  
  ai <- acc_int(prev_coupon, settle, next_coupon, coupon, conv)
  dirty_px <- clean_px + ai
  
  amt <- c(-dirty_px , cfs$cf_amount)
  tm <- c(0, cfs$year_frac)
  yield <- ytm(amt, tm, y0 = 0.05, tol = 1e-06, h = 1e-05, maxit = 1000L)
  yield <- 100 * freq * ((1 + yield)^(1 / freq) - 1)
  
  return(yield)
}



calc_yield  <- function(x, ...){
  UseMethod("calc_yield")
}

calc_yield.default <- function(maturity, settle, coupon, clean_px,
                               conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  tmp_dated_date <- settle - months(6)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  
  yield <- calc_yield.bond(b, settle, clean_px)
  return(yield)  
}

calc_yield.bond <- function(b, settle, clean_px){
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  
  coupon <- b$coupon
  freq <- b$freq
  conv <- b$conv
  cfs <- b$cfs
  
  
  if(length(cfs)==1)
  {
    cfs <- cfs$maturity
    ytw <- yield_cfs_single(cfs, settle, coupon, conv, freq, clean_px)
    names(ytw) <- "maturity"
    attr(ytw, "workout_date") <- max(cfs$cf_date)
  }
  else
  {
    yields <- lapply(cfs, yield_cfs_single, settle, coupon, conv, freq, clean_px)
    worst_index <- which.min(yields)
    worst_type <- names(yields)[worst_index]
    ytw <- yields[[worst_index]]
    names(ytw) <- worst_type
    attr(ytw, "workout_date") <- max(cfs[[worst_index]]$cf_date)    
  }
  
  return(ytw)  
}

dv01_cfs_single <- function(cfs, settle, yield, conv = "30/360",
                            freq = 2, returnCFs = TRUE){
  cfs <- discount_cfs_single(cfs, settle, yield, conv, freq,
                             include_dfPV = TRUE, include_dur = TRUE)
  if(returnCFs){
    result <- cfs
  }
  else
  {
    result <- sum(cfs$dv01)
    
  }
  return(result)
  
}


calc_dv01  <- function(x, ...){
  UseMethod("calc_dv01")
}

calc_dv01.default <- function(maturity, settle, coupon, price_yield, 
                              input_type = "Y", returnCFs = FALSE,
                              conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  tmp_dated_date <- settle - months(6)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  
  if(input_type == "P"){
    yield <- calc_yield(b, settle, price_yield)
  }
  else{yield <- price_yield}
  
  dv01 <- calc_dv01.bond(b, settle, yield, returnCFs = returnCFs)
  return(dv01)  
}

calc_dv01.bond <- function(b, settle, price_yield,
                           input_type = "Y", returnCFs = FALSE){
  settle <- safe_ifelse(is.Date(settle), settle,
                        as.Date(parse_date_time(settle, c("ymd","mdy"))))
  
  if(input_type == "P"){
    yield <- calc_yield(b, settle, price_yield)
  }
  else{yield <- price_yield}
  
  freq <- b$freq
  conv <- b$conv
  cfs <- b$cfs
  
  if(length(cfs)==1)
  {    
    cfs <- cfs$maturity
    dv01 <- dv01_cfs_single(cfs, settle, yield, conv, freq, returnCFs)
    if(!returnCFs)
    {
      names(dv01) <- "maturity"
      attr(dv01, "workout_date") <- max(cfs$cf_date)
    }
  }
  else
  {
    dv01 <- lapply(cfs, dv01_cfs_single, settle, yield, conv, freq, TRUE)
    dirty_px <- lapply(mapply(`[[`, dv01, "PV"), sum)
    worst_index <- which.min(dirty_px)
    worst_type <- names(dirty_px)[worst_index]
    dv01 <- dv01[[worst_index]]
    if(!returnCFs){
      dv01 <- sum(dv01$dv01)
      names(dv01) <- worst_type
      attr(dv01, "workout_date") <- max(cfs[[worst_index]]$cf_date)   
    }
  }
  
  return(dv01)  
}


