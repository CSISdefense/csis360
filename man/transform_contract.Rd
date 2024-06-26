% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Standardization.R
\name{transform_contract}
\alias{transform_contract}
\title{Transform contract names}
\usage{
transform_contract(contract)
}
\arguments{
\item{contract}{A contract dataset}
}
\value{
contract dataset ready for statistical analysis.
}
\description{
Transform contract names
}
\details{
This function is designed to prepare CSIS data files for lookup
application. It primarily smooths out variation between different ways we've
written SQL statements. It relies on a pre-existing table of variant names.
The var names are matched against that table in a case insensitive manner,
though no other procedural standardization is applied at this time.
}
\examples{
transform_contract(def)

}
