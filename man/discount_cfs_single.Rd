% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/bonds.R
\name{discount_cfs_single}
\alias{discount_cfs_single}
\title{Discount Single Set of Cashflows}
\usage{
discount_cfs_single(cfs, settle, yield = NULL, conv = "30/360", freq = 2,
  include_dfPV = TRUE, include_dur = FALSE)
}
\arguments{
\item{cfs}{Cashflows \code{tbl_df}}

\item{settle}{Settlement Date}

\item{yield}{Yield in percent (e.g. 5.0)}

\item{conv}{Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
or \code{"Act/365"})}

\item{freq}{Coupon frequency (number of periods per year)}

\item{include_dfPV}{Boolean indicating whether or not to include
discount factor and present value columns in the result}

\item{include_dur}{Boolean indicating whether or not to include macaulay
duration, modified duration, dv01, and convexity columns in result}
}
\value{
\code{tbl_df} containing cashflows and other calculated columns
given the \code{yield} parameter.
}
\description{
Used as the backbone for a number of pricing functions. The function filters
for relevant cashflow dates given the settle date. It allows the user to
specify which columns should be in the results via the last two parameters.
}
\examples{
cfs <- create_cashflows("2045-02-15", 2.5, "2015-05-07", "Act/Act", 2, 100, "ABCD")
discount_cfs_single(cfs, "2015-05-07", 4, "30/360", 2, TRUE, TRUE)
}
\seealso{
Other pricing functions: \code{\link{acc_int}};
  \code{\link{calc_clean_px}},
  \code{\link{calc_clean_px.bond}},
  \code{\link{calc_clean_px.default}};
  \code{\link{calc_dirty_px}},
  \code{\link{calc_dirty_px.bond}},
  \code{\link{calc_dirty_px.default}};
  \code{\link{calc_dv01}}, \code{\link{calc_dv01.bond}},
  \code{\link{calc_dv01.default}}, \code{\link{calc_risk}},
  \code{\link{calc_risk.bond}},
  \code{\link{calc_risk.default}};
  \code{\link{calc_yield}}, \code{\link{calc_yield.bond}},
  \code{\link{calc_yield.default}}; \code{\link{calc_ytm}};
  \code{\link{risk_cfs_single}};
  \code{\link{yield_cfs_single}}
}

