library(testthat)
library(csis360)



# read in data
FullData <- read.csv(
  "data\\2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  na.strings=c("NA","NULL"))


context("remove_bom")

FullData<-remove_bom(FullData)

test_that("remove_bom fixes ï..", {
  expect_equal(colnames(FullData)[1], "Fiscal.Year")
})



context("standardize_variable_names")


FullData <- read.csv(
  "data\\2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  na.strings=c("NA","NULL"))

FullData<-standardize_variable_names(FullData)

test_that("standardize_variable_names fixes ï..", {
  expect_equal(colnames(FullData)[1], "Fiscal.Year")
})



# coerce Amount to be a numeric variable
FullData$Action.Obligation <- as.numeric(FullData$Action.Obligation)
FullData$SumOfnumberOfActions <- as.numeric(FullData$SumOfnumberOfActions)


# discard pre-2000
FullData <- subset(FullData,Fiscal.Year >= 2000)


context("read_and_join")
FullData<-read_and_join(FullData,
                        "LOOKUP_Deflators.csv",
                        by="Fiscal.Year",
                        na_check_columns="Deflator.2016",
                        only_keep_checked_columns=TRUE
)


FullData$Obligation.2016 <- FullData$Action.Obligation /
  FullData$Deflator.2016
FullData<-FullData[,colnames(FullData)!="Deflator.2016"]


#Consolidate categories for Vendor Size
FullData<-read_and_join(FullData,
                        "LOOKUP_Contractor_Size.csv",
                        by="Vendor.Size",
                        na_check_columns="Shiny.VendorSize",
                        only_keep_checked_columns=TRUE
)



# classify competition
FullData<-read_and_join(FullData,
                        "Lookup_SQL_CompetitionClassification.csv",
                        by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                        replace_na_column_name="ClassifyNumberOfOffers",
                        na_check_columns=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum"),
                        only_keep_checked_columns=TRUE
)


#Classify Product or Service Codes
FullData<-read_and_join(FullData,
                        "LOOKUP_Buckets.csv",
                        by="ProductOrServiceArea",
                        na_check_columns="ProductServiceOrRnDarea.sum",
                        only_keep_checked_columns=TRUE,
                        replace_na_column_name="ProductOrServiceArea"
)

context("replace_nas_with_unlabeled")
FullData<-replace_nas_with_unlabeled(FullData,"SubCustomer","Uncategorized")

FullData<-read_and_join(FullData,
                        "Lookup_SubCustomer.csv",
                        by=c("Customer","SubCustomer"),
                        na_check_columns="SubCustomer.platform",
                        only_keep_checked_columns=TRUE
)

# write output to CleanedVendorSize.csv
save(FullData, file="2016_unaggregated_FPDS.Rda")

LabelsAndColors<-PrepareLabelsAndColors(FullData,"SubCustomer")

FullData<-replace_nas_with_unlabeled(FullData,"PlatformPortfolio")
LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"PlatformPortfolio")
)
# ,"PlatformPortfolio")
# )
#Shiny.VendorSize is the new Vendor.Size
LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"Shiny.VendorSize")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"Competition.sum")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"Competition.multisum")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"Competition.effective.only")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"No.Competition.sum")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"Customer")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"ProductOrServiceArea")
)

LabelsAndColors<-rbind(LabelsAndColors,
  PrepareLabelsAndColors(FullData,"ProductServiceOrRnDarea.sum")
)
