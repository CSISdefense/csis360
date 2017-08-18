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

test_that("Rename the SumOfObligatedAmount Column", {
  expect_equal(colnames(FullData)[9], "Action.Obligation")
})



# coerce Amount to be a numeric variable
FullData$Action.Obligation <- as.numeric(FullData$Action.Obligation)
FullData$SumOfnumberOfActions <- as.numeric(FullData$SumOfnumberOfActions)
FullData$Fiscal.Year <- as.numeric(FullData$Fiscal.Year)

# discard pre-2000
FullData <- subset(FullData,Fiscal.Year >= 2000)


context("deflate")
FullData<-deflate(FullData,
  money_var = "Action.Obligation",
  deflator_var="Deflator.2016"
)


context("read_and_join")

#Consolidate categories for Vendor Size
FullData<-read_and_join(FullData,
                        "LOOKUP_Contractor_Size.csv",
                        by="Vendor.Size",
                        add_var="Shiny.VendorSize"
)



# classify competition
FullData<-read_and_join(FullData,
                        "Lookup_SQL_CompetitionClassification.csv",
                        by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                        replace_na_var="ClassifyNumberOfOffers",
                        add_var=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum")
)


#Classify Product or Service Codes
FullData<-read_and_join(FullData,
                        "LOOKUP_Buckets.csv",
                        by="ProductOrServiceArea",
                        add_var="ProductServiceOrRnDarea.sum",
                        replace_na_var="ProductOrServiceArea"
)

context("replace_nas_with_unlabeled")
FullData<-replace_nas_with_unlabeled(FullData,"SubCustomer","Uncategorized")

FullData<-read_and_join(FullData,
                        "Lookup_SubCustomer.csv",
                        by=c("Customer","SubCustomer"),
                        add_var="SubCustomer.platform",
                        new_var_checked=TRUE
)



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

# write output to CleanedVendorSize.csv
save(FullData,LabelsAndColors, file="2016_unaggregated_FPDS.Rda")
