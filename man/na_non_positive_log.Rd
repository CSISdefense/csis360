% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Standardization.R
\name{na_non_positive_log}
\alias{na_non_positive_log}
\title{Set non-positive values to na and then log.}
\usage{
na_non_positive_log(x)
}
\arguments{
\item{x}{A list of numbers}
}
\value{
The list of number logs, with 0 and negative set to NA
}
\description{
Set non-positive values to na and then log.
}
\details{
This is a function to use when the data should never
be 0s or negatives. It saves the step of setting them to na.
}
\examples{
x<-c(0,2,3,-4); transform_contract(x)

}
