# Cashflow Functions ------------------------------------------------------

#' Create vector of cashflow amounts
#' 
#' @family cashflows
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

#' Create \code{tbl_df} of Cashflows for a Given Workout/Maturity Date
#' 
#' If a bond has a list of potential workout dates such as a call schedule, this
#' procedure will be called for each workout date. Dates can be passed as R date
#' types or can be in \code{ymd} or \code{mdy} form with any of the common 
#' delimiters used in lubridate. The function is vectorized and if you don't pass
#' in an "id," normally the CUSIP, the integers 1 to N will be used where N is
#' the number of workout dates. 
#' 
#' @family cashflows
#' 
#' @param maturity Maturity/Workout date
#' @param coupon  Annnual coupon amount
#' @param settle Settlement date
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @param redemption  Redemption value or par amount
#' @param id CUSIP or other id to be used as an identifier in the returned tbl_df
#' @return  tbl_df with cashflows counting back to the coupon date before settle.
#'          Columns are (id, cashflow date, cashflow amount)
#' @examples
#' create_cashflows("2045-02-15", 2.5, "2015-05-07", "Act/Act", 2, 100,"ABCD")
#' @export
create_cashflows <- function(maturity, coupon, settle, conv = "30/360", 
                             freq = 2, redemption = 100, id = NULL){
  maturity <- try_parse_date(maturity)
  settle <- try_parse_date(settle)
  coupon <- as.numeric(coupon)
  
  daysinyear <- 365.25
  maturity_in_years <- as.numeric(maturity - settle) / daysinyear
  coup_num <- round_any_v(maturity_in_years, 1 / freq, ceiling) * freq
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
  
  cf
  
}


# Bond Constructor --------------------------------------------------------

#' Create \code{bond} Object
#' 
#' Given basic bond details like maturity date, call date and price (if callable),
#' coupon, frequency, settle, daycount convention, and id (e.g. cusip),
#' this will create an object of type \code{bond}. The constructor can take a 
#' \code{cfs} object, which, if passed, should be a list of \code{tbl_df}s, where each
#' one has a name that means something. e.g. \code{cfs$maturity}, \code{cfs$call},
#' \code{cfs$put} etc. \code{bond} is the basic object in this package. It is used
#' to calculate prices, yields, risk metrics, and do any other kind of fixed 
#' income analysis.
#' 
#' @param maturity Maturity/Workout date
#' @param coupon  Annnual coupon amount
#' @param dated_date Dated date, usually the issue date and the day from which
#'    interest accrues.
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @param redemption  Redemption value or par amount
#' @param id CUSIP or other id
#' @param cfs List of cashflow tbl_dfs (id, cf_date, cf_amount)
#' @return  \code{bond} object with cashflows
#' @examples
#' bond("2045-02-15", 2.5, "2015-02-17", "Act/Act", 2, 100, id = "ABCD")
#' bond("2045-02-15", 2.5, "2015-02-17", "Act/Act", 2, 100, id = "ABCD",
#'      call_date = "2025-02-15", call_px = 101)
#' @export
bond <- function(maturity, coupon, dated_date, conv = "30/360", freq = 2, 
                 redemption = 100, id = NULL, cfs = NULL,
                 call_date = NULL, call_px = NULL){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  maturity <- try_parse_date(maturity)
  dated_date <- try_parse_date(dated_date)
  
  if(!is.null(call_date) && !is.na(call_date))
  {
    call_date <- try_parse_date(call_date)
    call_px <- ifelse((is.null(call_px) || is.na(call_px)), 100, call_px)
  } else {    
    call_date <- NULL
    call_px <- NULL
  }
  
  
  if(is.null(cfs)){
    cfs_maturity <- create_cashflows(maturity, coupon, dated_date, conv, freq,
                                     redemption, id)
    if(!is.null(call_date))
    {
      cfs_call <- create_cashflows(call_date, coupon, dated_date, conv, freq, 
                                   redemption = call_px, id)    
    } else{
      cfs_call <- NULL
    }
    
    cfs <- list(maturity = cfs_maturity)
    
    if(!is.null(cfs_call))
    {
      cfs$call <- cfs_call
    }
    
  }
  
  bond <- list(id = id, maturity = maturity, coupon = coupon, 
               dated_date = dated_date, conv = conv, freq = freq, 
               cfs = cfs, call_date = call_date, 
               call_px = call_px)
  
  class(bond) <- "bond"
  bond
}

# Pricing Functions --------------------------------------------------------

#' Calculate Accrued Interest
#' 
#' Bonds typically trade based on a "clean price," which does not include
#' accrued interest. To calculate yields etc. "dirty price" must first be 
#' calculated by including accrued interest. This function assumes that the
#' dates passed in confrom to the convention that \code{settle} is between
#' \code{accrual_start} and \code{next_coupon}.
#' 
#' @family pricing functions
#' 
#' @param accrual_start Accrual start date
#' @param settle  Settlement Date
#' @param next_coupon  Next coupon date
#' @param coupon Annual coupon amount
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @return Numeric value of the accrued interest for a par amount of 100
#' @examples
#' acc_int("2015-01-15", "2015-03-07", "2015-07-15", "30/360", 2)
#' @export
acc_int <- function(accrual_start, settle, next_coupon, coupon,
                    conv = "30/360", freq = 2){    
  accrual_start <- try_parse_date(accrual_start)
  settle <- try_parse_date(settle)
  next_coupon <- try_parse_date(next_coupon)
  
  daysinyear <- days_in_year(settle, conv)
  
  if(conv == "Act/Act"){
    daysinyear <- freq * dc_act_act(accrual_start, next_coupon)
  }
  
  ai_days <- dc(accrual_start, settle, conv)  
  ai <- (ai_days / daysinyear) * (coupon)
  ai
}

#' Discount Single Set of Cashflows
#' 
#' Used as the backbone for a number of pricing functions. The function filters
#' for relevant cashflow dates given the settle date. It allows the user to 
#' specify which columns should be in the results via the last two parameters.
#' 
#' @family pricing functions
#' 
#' @param cfs Cashflows \code{tbl_df}
#' @param settle  Settlement Date
#' @param yield  Yield in percent (e.g. 5.0)
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @param include_dfPV Boolean indicating whether or not to include 
#' discount factor and present value columns in the result
#' @param include_dur Boolean indicating whether or not to include macaulay 
#' duration, modified duration, dv01, and convexity columns in result
#' @return \code{tbl_df} containing cashflows and other calculated columns
#' given the \code{yield} parameter.
#' @examples
#' cfs <- create_cashflows("2045-02-15", 2.5, "2015-05-07", "Act/Act", 2, 100, "ABCD")
#' discount_cfs_single(cfs, "2015-05-07", 4, "30/360", 2, TRUE, TRUE)
#' @export
discount_cfs_single <- function(cfs, settle, yield = NULL, conv = "30/360", freq = 2, 
                                include_dfPV = TRUE, include_dur = FALSE){
  
  if(!is.null(yield)){
    yield <- yield / 100
  }
  settle <- try_parse_date(settle)
#   next_coupon <- (cfs %>%  filter(cf_date > settle) %>% summarise(min(cf_date)))[[1]]
#   prev_coupon <- (cfs %>%  filter(cf_date <= settle) %>% summarise(max(cf_date)))[[1]]
  next_coupon <- with(cfs, cf_date[min(which(cf_date > settle))])
  prev_coupon <- with(cfs, cf_date[max(which(cf_date <= settle))])
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
                            days_diff = dc(settle, cf_date, conv),
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
                                      year_frac * freq) /  freq^2 /10000)
    }
  }
  else{
    if(conv == "Act/Act"){
      cfs <- cfs %>% mutate(index = (dense_rank(cf_date) - 1),
                            days_diff = dc(settle, cf_date, conv),
                            year_frac = (index + (DSC / E))/freq)
    }
    else{
      cfs <- cfs %>% mutate(index = (dense_rank(cf_date) - 1),
                            days_diff = (daysinyear / freq) * (index) + coupon_offset,
                            year_frac = days_diff / daysinyear)
    }
  }
  cfs
}


#' Calculate Dirty Price of a Bond
#' 
#' Given a \code{bond} or the necessary parameters to construct one, this
#' will calculate the price of the \code{bond} including accrued interest. This
#' price is used in yield calculations and other functions that require the 
#' "full" or "dirty" price.
#' 
#' @family pricing functions
#' 
#' @param b A \code{bond} object
#' @param maturity Maturity/Workout date
#' @param settle Settlement Date
#' @param coupon  Annnual coupon amount
#' @param yield Yield in percent (e.g. 5.0)
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @return Numeric value of the clean price and corresponding worst workout date
#' if there are multiple sets of cashflows in the bond
#' @examples
#' calc_dirty_px("2045-02-15", "2015-02-15", 4, 3, "Act/Act", 2)
#' b <- bond("2045-02-15", 2.5, "2015-02-17", "Act/Act", 2, 100, id = "ABCD")
#' calc_dirty_px(b, "2015-05-15", 3)
#' @export
calc_dirty_px <- function(x, ...){
  UseMethod("calc_dirty_px")
}

#' @rdname calc_dirty_px
#' @export
calc_dirty_px.default <- function(maturity, settle, coupon, yield,
                                  conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- try_parse_date(settle)
  tmp_dated_date <- settle - months(12 / freq)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  dirty_px <- calc_dirty_px.bond(b, settle, yield)
  dirty_px
  
}

#' @rdname calc_dirty_px
#' @export
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
  
  dirty_px
}


#' Calculate Clean Price of a Bond
#' 
#' Given a \code{bond} or the necessary parameters to construct one, this
#' will calculate the price of the \code{bond} excluding accrued interest.
#' This is the price used in normal market quotes.
#' 
#' @family pricing functions
#' 
#' @inheritParams calc_dirty_px
#' 
#' @return Numeric value of the clean price and corresponding worst workout date
#' if there are multiple sets of cashflows in the bond
#' @examples
#' calc_clean_px("2045-02-15", "2015-02-15", 4, 3, "Act/Act", 2)
#' b <- bond("2045-02-15", 2.5, "2015-02-17", "Act/Act", 2, 100, id = "ABCD")
#' calc_clean_px(b, "2015-05-15", 3)
#' @export
calc_clean_px <- function(x, ...){
  UseMethod("calc_clean_px")
}

#' @rdname calc_clean_px
#' @export
calc_clean_px.default <- function(maturity, settle, coupon, yield,
                                  conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- try_parse_date(settle)
  tmp_dated_date <- settle - months(12 / freq)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  
  clean_px <- calc_clean_px.bond(b, settle, yield)
  clean_px
}

#' @rdname calc_clean_px
#' @export
calc_clean_px.bond <- function(b, settle, yield){
  settle <- try_parse_date(settle)
  
  coupon <- b$coupon
  freq <- b$freq
  conv <- b$conv
  cfs <- b$cfs
  
#   prev_coupon <- (cfs$mat %>%  filter(cf_date <= settle) %>% summarise(max(cf_date)))[[1]]  
#   next_coupon <- (cfs$mat %>%  filter(cf_date > settle) %>% summarise(min(cf_date)))[[1]]
  next_coupon <- with(cfs$mat, cf_date[min(which(cf_date > settle))])
  prev_coupon <- with(cfs$mat, cf_date[max(which(cf_date <= settle))])  

  dirty_px <- calc_dirty_px.bond(b, settle, yield)
  ai <- acc_int(prev_coupon, settle, next_coupon, coupon, conv)
  clean_px <- dirty_px - ai
  
  
  names(clean_px) <- names(dirty_px)
  attr(clean_px, "workout_date") <- max(cfs[[names(clean_px)]]$cf_date)
  
  clean_px
  
}

#' Yield or IRR for a Single Set of Cashflows with the Outflow
#' 
#' Used in \code{yield_cfs_single} to do the actual yield calc. 
#' The \code{ytm} function from the NMOF manual is used to calculate
#' the yield, with a 5\% initial guess. It assumes that the first
#' cashflow in the \code{cf} vector is negative as it is an outflow equal
#' to the present value of the cashflows. The corresponding element
#' in the \code{times} vector should be 0.
#' 
#' @family pricing functions
#' 
#' @param cf Vector of cashflow amounts (first cashflow is an outflow)
#' @param times Vector of yearfracs (times to cashflow, first should be 0)
#' @param y0 Initial yield guess (5\% default)
#' @param tol Tolerance in the yield calculation
#' @param maxit Maximum number of iterations
#' @return Numeric value of the yield/IRR for the given cashflow
#' amounts and yearfracs in decimal form (not percentage) (e.g. .05)
#' @examples
#' cf <- c(-101, 3, 3, 3, 103)
#' times <- c(0, 1, 2, 3, 4)
#' ytm(cf, times)
#' @export
ytm <- function(cf, times, y0 = 0.05,
                tol = 1e-05, maxit = 1000L) {        
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


#' Yield or IRR for a Single Set of Cashflows
#' 
#' Used as the backbone for all yield calculations. This function uses
#' \code{discount_cfs_single} to generate the cashflows' yearfracs
#' from the settlement date. The \code{ytm} function from the NMOF manual is
#' used to calculate the yield, with a 5\% initial guess.
#' 
#' @family pricing functions
#' 
#' @param cfs Cashflows \code{tbl_df}
#' @param settle  Settlement Date
#' @param coupon  Annnual coupon amount
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @param clean_px The clean price of the bond
#' @return Numeric value of the yield/IRR for the given cashflows and price
#' @examples
#' cfs <- create_cashflows("2045-02-15", 2.5, "2015-05-07", "Act/Act", 2, 100, "ABCD")
#' yield_cfs_single(cfs, "2015-05-07", 4, "30/360", 2, 101)
#' @export
yield_cfs_single <- function(cfs, settle, coupon, conv = "30/360", freq = 2, clean_px){
  
#   prev_coupon <- (cfs %>%  filter(cf_date <= settle) %>% summarise(max(cf_date)))[[1]]  
#   next_coupon <- (cfs %>%  filter(cf_date > settle) %>% summarise(min(cf_date)))[[1]]
  next_coupon <- with(cfs, cf_date[min(which(cf_date > settle))])
  prev_coupon <- with(cfs, cf_date[max(which(cf_date <= settle))])
  
  cfs <- discount_cfs_single(cfs, settle, yield = NULL, conv, freq, include_dfPV = FALSE)
  
  ai <- acc_int(prev_coupon, settle, next_coupon, coupon, conv)
  dirty_px <- clean_px + ai
  
  amt <- c(-dirty_px , cfs$cf_amount)
  tm <- c(0, cfs$year_frac)
  yield <- ytm(amt, tm, y0 = 0.05, tol = 1e-06, maxit = 1000L)
  yield <- 100 * freq * ((1 + yield)^(1 / freq) - 1)
  
  yield
}

#' Calculate Yield to Worst of a Bond
#' 
#' Given a \code{bond} or the necessary parameters to construct one, this
#' will calculate the yield to worst of the \code{bond}. The workout dates
#' used are the based on the \code{cfs} object's last cashflow dates in
#' the \code{bond}. This function uses \code{yield_cfs_single}, which in 
#' turn uses the imported function \code{NMOF::ytm} to do the actual
#' calculations.
#' 
#' 
#' @family pricing functions
#' 
#' @param b A \code{bond} object
#' @param maturity Maturity/Workout date
#' @param settle Settlement Date
#' @param coupon  Annnual coupon amount
#' @param clean_px The clean price of the bond
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @return Numeric value of the yield to worst and corresponding worst workout date
#' if there are multiple sets of cashflows in the bond
#' @examples
#' calc_yield("2045-02-15", "2015-02-15", 4, 101, "Act/Act", 2)
#' b <- bond("2045-02-15", 2.5, "2015-02-17", "Act/Act", 2, 100, id = "ABCD")
#' calc_yield(b, "2015-05-15", 101)
#' @export
calc_yield  <- function(x, ...){
  UseMethod("calc_yield")
}

#' @rdname calc_yield
#' @export
calc_yield.default <- function(maturity, settle, coupon, clean_px,
                               conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- try_parse_date(settle)
  tmp_dated_date <- settle - months(12 / freq)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  
  yield <- calc_yield.bond(b, settle, clean_px)
  yield
}

#' @rdname calc_yield
#' @export
calc_yield.bond <- function(b, settle, clean_px){
  settle <- try_parse_date(settle)
  
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
  
  ytw 
}


#' DV01 and Other Risks for a Single Set of Cashflows
#' 
#' Used as the backbone for all DV01 and risk calculations. This function uses
#' \code{discount_cfs_single} to calculate the important risk metrics.
#' 
#' 
#' 
#' @family pricing functions
#' 
#' @param cfs Cashflows \code{tbl_df}
#' @param settle  Settlement Date
#' @param yield Yield or IRR used to discount the cashflows
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @param returnCFs Boolean flag indicating whether or not to return the
#' calculated set of cashflows and risk metrics
#' @return Numeric value of the DV01 and other risks for the given cashflows 
#' and yield or the full calculated set of risks by cashflow
#' @examples
#' cfs <- create_cashflows("2045-02-15", 2.5, "2015-05-07", "Act/Act", 2, 100, "ABCD")
#' risk_cfs_single(cfs, "2015-05-07", 4, "30/360", 2, TRUE)
#' @export
risk_cfs_single <- function(cfs, settle, yield, conv = "30/360",
                            freq = 2, returnCFs = TRUE){
  cfs <- discount_cfs_single(cfs, settle, yield, conv, freq,
                             include_dfPV = TRUE, include_dur = TRUE)
  if(returnCFs){
    result <- cfs
  }
  else
  {
    result <- colSums(cfs[-1:-7])
    
  }
  result
  
}

#' Calculate the Risks of a Bond
#' 
#' Given a \code{bond} or the necessary parameters to construct one, this
#' will calculate the DV01 and other risks of the \code{bond} for the worst
#' workout date. The workout dates used are the based on the \code{cfs} 
#' object's last cashflow dates in the \code{bond}. This function uses 
#' \code{discount_cfs_single} to do the actual calculations.
#' 
#' 
#' @family pricing functions
#' 
#' @param b A \code{bond} object
#' @param maturity Maturity/Workout date
#' @param settle Settlement Date
#' @param coupon  Annnual coupon amount
#' @param price_yield The clean price of the bond or the yield to worst
#' @param input_type \code{"Y"} for Yield to Worst (default) or \code{"P"}
#' for clean price
#' @param returnCFs Boolean flag indicating whether or not to return the
#' calculated set of cashflows and risk metrics
#' @param conv Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
#'  or \code{"Act/365"})
#' @param freq  Coupon frequency (number of periods per year)
#' @return Numeric value of the DV01 and other risks and corresponding worst 
#' workout date and workout type if there are multiple sets of cashflows
#' in the bond or a full set of calculated cashflows and risk metrics 
#' as a \code{tbl_df}
#' @examples
#' calc_risk("2045-02-15", "2015-02-15", 4, 101, "P", TRUE, "Act/Act", 2)
#' b <- bond("2045-02-15", 2.5, "2015-02-17", "Act/Act", 2, 100, id = "ABCD")
#' calc_risk(b, "2015-05-15", 101, "P", TRUE)
#' @export
calc_risk  <- function(x, ...){
  UseMethod("calc_risk")
}

#' @rdname calc_risk
#' @export
calc_risk.default <- function(maturity, settle, coupon, price_yield, 
                              input_type = "Y", returnCFs = FALSE,
                              conv = "30/360", freq = 2){
  if(!(conv %in% c("30/360", "Act/Act", "Act/365")))
    stop("Day count convention not supported.")
  settle <- try_parse_date(settle)
  tmp_dated_date <- settle - months(12 / freq)
  b <- bond(maturity, coupon, tmp_dated_date, conv, freq)
  
  if(input_type == "P"){
    yield <- calc_yield(b, settle, price_yield)
  }
  else{yield <- price_yield}
  
  risk <- calc_risk.bond(b, settle, yield, returnCFs = returnCFs)
  risk
}

#' @rdname calc_risk
#' @export
calc_risk.bond <- function(b, settle, price_yield,
                           input_type = "Y", returnCFs = FALSE){
  settle <- try_parse_date(settle)
  
  if(input_type == "P"){
    yield <- calc_yield(b, settle, price_yield)
  }
  else{yield <- price_yield}
  
  freq <- b$freq
  conv <- b$conv
  cfs <- b$cfs
  
  if(length(cfs) == 1)
  {    
    cfs <- cfs$maturity
    risk <- dv01_cfs_single(cfs, settle, yield, conv, freq, TRUE)
    
    if(!returnCFs)
    {
      workout_date <- max(cfs$cf_date)
      risk <- colSums(risk[-1:-7])
      attr(risk, "workout_type") <- "maturity"
      attr(risk, "workout_date") <- workout_date
    }
  }
  else
  {
    risk <- lapply(cfs, risk_cfs_single, settle, yield, conv, freq, TRUE)
    dirty_px <- lapply(mapply(`[[`, risk, "PV"), sum)
    worst_index <- which.min(dirty_px)
    worst_type <- names(dirty_px)[worst_index]
    risk <- risk[[worst_index]]
    workout_date <- max(cfs[[worst_index]]$cf_date)   
    
    if(!returnCFs){
      risk <- colSums(risk[-1:-7])
    }
    attr(risk, "workout_type") <- "maturity"
    attr(risk, "workout_date") <- max(cfs[[worst_index]]$cf_date)   
  }
  
  round(risk, 8)
}