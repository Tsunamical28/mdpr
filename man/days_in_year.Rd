% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/date_utils.R
\name{days_in_year}
\alias{days_in_year}
\title{Count Days in Year}
\usage{
days_in_year(date, conv = "Act/Act")
}
\arguments{
\item{conv}{the day count convention can be either "30/360", "Act/365", or
"Act/Act" (default)}

\item{vector}{of dates}
}
\value{
number of days in year
}
\description{
Counts the number of days in year via the provided day count convention.
}
\examples{
d <- as.Date(c("2015-01-01","2015-08-08"))
days_in_year(d, conv = "30/360")
}

