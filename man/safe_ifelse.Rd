% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.R
\name{safe_ifelse}
\alias{safe_ifelse}
\title{Vectorized \code{ifelse} with Class Coercion}
\usage{
safe_ifelse(cond, yes, no, class_to_use = class(no))
}
\arguments{
\item{cond}{the test to determine if yes/no}

\item{yes}{the value or operation to return if \code{cond} evaluates to \code{TRUE}}

\item{no}{the value or operation to return if \code{cond} evaluates to \code{FALSE}}
}
\value{
the resulting vector coerced to class type \code{class_to_use}
}
\description{
The normal behavior for the \code{ifelse} function is to return the resulting vector
with a class type coerced to be the same as the \code{yes} argument. However, when
a Date type vector is passed as an argument, Dates lose their class when the
condition is evaluated. The resulting vector is of type numeric.
This function \code{safe_ifelse} modifies the normal behavior for Date vectors.
Users can specify whether to coerce to any class or the default will be used
which is \code{class(no)}.
}
\examples{
d = as.Date(c("2015-01-01","2015-08-08"))
safe_ifelse(d=="2015-01-01",d-10,d)
}

