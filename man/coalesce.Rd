% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.R
\name{coalesce}
\alias{coalesce}
\title{Coalesce to Return First Non-NA}
\usage{
coalesce(...)
}
\arguments{
\item{...}{Arguments to evaluate}
}
\value{
The first non NA argument
}
\description{
Evaluates the given argument in the order in which they were passed.
This function will return the first argument that does NOT evaluate to NA.
}
\examples{
coalesce(NA, "second","third",4, NA,"sixth")
}

