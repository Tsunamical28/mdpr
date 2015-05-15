# 
# # Things to Do ------------------------------------------------------------
# 

# Issue: dirty price is not accurate for bonds with settlement dates that land before the
# first coupon date. When settle is before first coupon, the start accrual date for this coupon
# period will be before the first_int_accrual date (sometimes interchangeable with dated date).
# When this happens, phantom accrued interest is generated, which skews the dirty price.
#
# Solution: Replace the first CF with the first int accrual date if it occurs before first 
# int accrual date.
#
# create_cashflows <- function(maturity, coupon, settle, conv = "30/360", 
#                              freq = 2, redemption = 100, id = NULL, 
#                              int_accrual = NULL){
#   int_accrual <- ifelse(is.null(int_accrual), settle, int_accrual)
#   
#   maturity <- try_parse_date(maturity)
#   settle <- try_parse_date(settle)
#   int_accrual <- try_parse_date(int_accrual)
#   coupon <- as.numeric(coupon)
#   
#   daysinyear <- 365.25
#   maturity_in_years <- as.numeric(maturity - int_accrual) / daysinyear
#   coup_num <- round_any_v(maturity_in_years, 1 / freq, ceiling) * freq
#   coupon_dates <- mapply(seq, from = maturity,
#                          by = paste0("-", as.integer(12 / freq)," months"), 
#                          length.out = (coup_num + 1), SIMPLIFY = F)
#   coupon_dates <- lapply(coupon_dates, sort)
#   coupon_dates <- lapply(coupon_dates, as.Date)
#   lapply(seq_along(coupon_dates), 
#          function(x){
#            coupon_dates[[x]][1] <<- 
#              safe_ifelse(coupon_dates[[x]][1] < int_accrual[x], int_accrual[x], 
#                          coupon_dates[[x]][1])
#          })
#   
#   cf_amounts <- mapply(create_cf_amounts, num = coup_num, 
#                        coupon = coupon, freq = freq,
#                        redemption = redemption, SIMPLIFY = F)
#   bond_ids <- lapply(
#     lapply(seq_along(coupon_dates), 
#            function(d){list(len = length(coupon_dates[[d]]),
#                             idx = ifelse(is.null(id[[d]]),d,id[[d]]))}),
#     function(x){rep(x$idx, x$len)})
#   
#   cf <- bind_rows(lapply(1:length(coupon_dates), 
#                          function(i){
#                            tbl_df(data.frame(id = as.character(unlist(bond_ids[i])),
#                                              cf_date = do.call("c", coupon_dates[i]), 
#                                              cf_amount = unlist(cf_amounts[i]),
#                                              stringsAsFactors = FALSE
#                            )
#                            )
#                          }
#   )
#   ) 
#   
#   cf
#   
# }
