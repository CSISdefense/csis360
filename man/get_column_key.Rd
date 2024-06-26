% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_Labels.R
\name{get_column_key}
\alias{get_column_key}
\title{Get Column Key based on the names in a data frame}
\usage{
get_column_key(
  data,
  path = "https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/"
)
}
\arguments{
\item{data}{A data frame}

\item{path}{The path or url for the column key.  By default, checks
the CSISdefense Github lookups repository at CSISdefense/Lookup-Tables/master/style/}
}
\value{
A data frame of the column names from data joined up to the column key
}
\description{
Get Column Key based on the names in a data frame
}
\details{
Warning: This function should be used in data processing only,
not in a live app.  It reads an external file from GitHub,
which will slow down an app substantially if done repeatedly. Works best
when standardize_names has already been run on the data frame in question.
}
\examples{

FullData <- read_csv("2017_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))
PrepareLabelsAndColors(Coloration,FullData,"Customer")

}
