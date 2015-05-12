
# Cashflow Functions ------------------------------------------------------

#' Create vector of cashflow amounts
#' 
#' @param num Number of cashflows
#' @param coupon  Constant cashflow amount
#' @param freq  Cashflow frequency (number of periods per year)
#' @param redemption  Redemption value or par amount
#' @return  Vector of cashflow amounts of length \code{num} whose last value is
#'    \eqn{redemption + \frac{coupon}{freq}}{redemption + coupon / freq}
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