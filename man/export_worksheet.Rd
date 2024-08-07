% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Standardization.R
\name{export_worksheet}
\alias{export_worksheet}
\title{Save a copy of the plot, a current dollars csv, and an excel copy}
\usage{
export_worksheet(
  df,
  xlsx,
  sheet,
  path = "..\\\\output",
  second_path = NA,
  startRow = 1,
  startCol = 1
)
}
\arguments{
\item{df}{the underlying data}

\item{xlsx}{the excel file to output to}

\item{sheet}{the sheet to use in excel, typically shorter than the name}

\item{path="..\\output\\"}{what directory for the output}

\item{second_path=NA}{for saving to a second location to automatically}

\item{startRow=1}{Start row for excel output}

\item{startCol=NA}{Start column for excel output}
}
\value{
no value
}
\description{
Save a copy of the plot, a current dollars csv, and an excel copy
}
