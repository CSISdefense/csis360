% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cross-Footing.R
\name{append_contract_fixes}
\alias{append_contract_fixes}
\title{Apply know fixes to contract files.}
\usage{
append_contract_fixes(
  Path = "https://github.com/CSISdefense/R-scripts-and-data/blob/master/Lookups/",
  df
)
}
\arguments{
\item{Path}{File path}

\item{df}{data frame}
}
\value{
updata datframe
}
\description{
Apply know fixes to contract files.
}
\details{
There are a few adjustments we make to the main contract.fpds dataset. This function applies this to our fpds.gov data as well.
}
