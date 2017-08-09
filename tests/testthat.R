library(testthat)
library(csis360)



# Path<-"C:\\Users\\gsand_000.ALPHONSE\\Documents\\Development\\R-scripts-and-data\\"
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"


# read in data
FullData <- read.csv(
  "data\\2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  na.strings=c("NA","NULL"))


context("standardize_variable_names")

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
FullData<-read_and_join(Path,
                        "LOOKUP_Deflators.csv",
                        FullData,
                        by="Fiscal.Year",
                        NA.check.columns="Deflator.2016",
                        OnlyKeepCheckedColumns=TRUE
)


FullData$Obligation.2016 <- FullData$Action.Obligation /
  FullData$Deflator.2016
FullData<-FullData[,colnames(FullData)!="Deflator.2016"]


#Consolidate categories for Vendor Size
FullData<-read_and_join(Path,
                        "LOOKUP_Contractor_Size.csv",
                        FullData,
                        by="Vendor.Size",
                        NA.check.columns="Shiny.VendorSize",
                        OnlyKeepCheckedColumns=TRUE
)



# classify competition
FullData<-read_and_join(Path,
                        "Lookup_SQL_CompetitionClassification.csv",
                        FullData,
                        by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                        ReplaceNAsColumns="ClassifyNumberOfOffers",
                        NA.check.columns=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum"),
                        OnlyKeepCheckedColumns=TRUE
)


#Classify Product or Service Codes
FullData<-read_and_join(Path,
                        "LOOKUP_Buckets.csv",
                        FullData,
                        by="ProductOrServiceArea",
                        NA.check.columns="ProductServiceOrRnDarea.sum",
                        OnlyKeepCheckedColumns=TRUE,
                        ReplaceNAsColumns="ProductOrServiceArea"
)

context("replace_nas_with_unlabeled")
FullData<-replace_nas_with_unlabeled(FullData,"SubCustomer","Uncategorized")

FullData<-read_and_join(Path,
                        "Lookup_SubCustomer.csv",
                        FullData,
                        by=c("Customer","SubCustomer"),
                        NA.check.columns="SubCustomer.platform",
                        OnlyKeepCheckedColumns=TRUE
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
