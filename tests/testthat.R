library(testthat)
library(csis360)


# read in data
full_data <- read.csv(
  "data\\2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  na.strings=c("NA","NULL"))

context("remove_bom")
full_data<-remove_bom(full_data)

test_that("remove_bom fixes ï..", {
  expect_equal(colnames(full_data)[1], "Fiscal.Year")
})



context("standardize_variable_names")

full_data <- read.csv(
  "data\\2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
  na.strings=c("NA","NULL"))

full_data<-standardize_variable_names(full_data)

test_that("standardize_variable_names fixes ï..", {
  expect_equal(colnames(full_data)[1], "Fiscal.Year")
})

test_that("Rename the SumOfObligatedAmount Column", {
  expect_equal(colnames(full_data)[9], "Action.Obligation")
})



# coerce Amount to be a numeric variable
full_data$Action.Obligation <- as.numeric(full_data$Action.Obligation)
full_data$SumOfnumberOfActions <- as.numeric(full_data$SumOfnumberOfActions)
full_data$Fiscal.Year <- as.numeric(full_data$Fiscal.Year)

# discard pre-2000
full_data <- subset(full_data,Fiscal.Year >= 2000)


context("deflate")
full_data<-deflate(full_data,
  money_var = "Action.Obligation",
  deflator_var="Deflator.2016"
)


context("read_and_join")

#Consolidate categories for Vendor Size
full_data<-read_and_join(full_data,
                        "LOOKUP_Contractor_Size.csv",
                        by="Vendor.Size",
                        add_var="Shiny.VendorSize"
)



# classify competition
full_data<-read_and_join(full_data,
                        "Lookup_SQL_CompetitionClassification.csv",
                        by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                        replace_na_var="ClassifyNumberOfOffers",
                        add_var=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum")
)


#Classify Product or Service Codes
full_data<-read_and_join(full_data,
                        "LOOKUP_Buckets.csv",
                        by="ProductOrServiceArea",
                        add_var="ProductServiceOrRnDarea.sum",
                        replace_na_var="ProductOrServiceArea"
)

context("replace_nas_with_unlabeled")
full_data<-replace_nas_with_unlabeled(full_data,"SubCustomer","Uncategorized")

full_data<-read_and_join(full_data,
                        "Lookup_SubCustomer.csv",
                        by=c("Customer","SubCustomer"),
                        add_var="SubCustomer.platform",
                        new_var_checked=TRUE
)

labels_and_colors<-prepare_labels_and_colors(full_data,"SubCustomer")

full_data<-replace_nas_with_unlabeled(full_data,"PlatformPortfolio")

labels_and_colors<-prepare_labels_and_colors(full_data)


# write output to CleanedVendorSize.csv
save(full_data,labels_and_colors, file="2016_unaggregated_FPDS.Rda")
