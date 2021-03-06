% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/bonds.R
\name{create_cashflows}
\alias{create_cashflows}
\title{Create \code{tbl_df} of Cashflows for a Given Workout/Maturity Date}
\usage{
create_cashflows(maturity, coupon, settle, conv = "30/360", freq = 2,
  redemption = 100, id = NULL)
}
\arguments{
\item{maturity}{Maturity/Workout date}

\item{coupon}{Annnual coupon amount}

\item{settle}{Settlement date}

\item{conv}{Daycount convention (one of \code{"30/360"}, \code{"Act/Act"},
or \code{"Act/365"})}

\item{freq}{Coupon frequency (number of periods per year)}

\item{redemption}{Redemption value or par amount}

\item{id}{CUSIP or other id to be used as an identifier in the returned tbl_df}
}
\value{
tbl_df with cashflows counting back to the coupon date before settle.
         Columns are (id, cashflow date, cashflow amount)
}
\description{
If a bond has a list of potential workout dates such as a call schedule, this
procedure will be called for each workout date. Dates can be passed as R date
types or can be in \code{ymd} or \code{mdy} form with any of the common
delimiters used in lubridate. The function is vectorized and if you don't pass
in an "id," normally the CUSIP, the integers 1 to N will be used where N is
the number of workout dates.
}
\examples{
create_cashflows("2045-02-15", 2.5, "2015-05-07", "Act/Act", 2, 100,"ABCD")
}
\seealso{
Other cashflows: \code{\link{create_cf_amounts}}
}

