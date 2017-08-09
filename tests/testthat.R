library(testthat)
library(csis360)

test_check("csis360")


Path<-"C:\\Users\\gsand_000.ALPHONSE\\Documents\\Development\\R-scripts-and-data\\"



# read in data
FullData <- read.csv(
  "data\\2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  na.strings=c("NA","NULL"))

FullData<-standardize_variable_names(FullData)

test_that("standardize_variable_names fixes ï..", {
  expect_equal(colnames(FullData)[1], "Fiscal.Year")
})
