library(testthat)
library(csis360)
library(ggplot2)

# read in data
full_data <- read.csv(system.file("extdata",
                                  "2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
                                  package = "csis360")  ,
  na.strings=c("NA","NULL"))

context("remove_bom")
bom_data<-remove_bom(full_data)
  test_that("remove_bom fixes ï..", {
    expect_equal(colnames(bom_data)[1], "Fiscal.Year")
})



context("standardize_variable_names")

full_data <- read.csv(system.file("extdata",
                                  "2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
                                  package = "csis360"),
  na.strings=c("NA","NULL"))

std_data<-standardize_variable_names(full_data)

test_that("standardize_variable_names fixes ï..", {
  expect_equal(colnames(std_data)[1], "Fiscal.Year")
})

test_that("Rename the SumOfObligatedAmount Column", {
  expect_equal(colnames(std_data)[9], "Action.Obligation")
})



# coerce Amount to be a numeric variable
std_data$Action.Obligation <- as.numeric(std_data$Action.Obligation)
std_data$Number.Of.Actions <- as.numeric(std_data$Number.Of.Actions)
std_data$Fiscal.Year <- as.numeric(std_data$Fiscal.Year)

# discard pre-2000
std_data <- subset(std_data,Fiscal.Year >= 2000)


context("deflate")

def_data<-deflate(std_data,
  money_var = "Action.Obligation",
  deflator_var="Deflator.2016"
)

expect_true("Action.Obligation.2016" %in% colnames(def_data))
expect_true("Action.Obligation.Then.Year" %in% colnames(def_data))
expect_false("Action.Obligationr" %in% colnames(def_data))


context("read_and_join")

#Consolidate categories for Vendor Size
def_data<-read_and_join(def_data,
                        "LOOKUP_Contractor_Size.csv",
                        by="Vendor.Size",
                        add_var="Shiny.VendorSize"
)

# classify competition
def_data<-read_and_join(def_data,
                        "Lookup_SQL_CompetitionClassification.csv",
                        by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                        replace_na_var="ClassifyNumberOfOffers",
                        add_var=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum")
)


#Classify Product or Service Codes
def_data<-read_and_join(def_data,
                        "LOOKUP_Buckets.csv",
                        by="ProductOrServiceArea",
                        add_var="ProductServiceOrRnDarea.sum",
                        replace_na_var="ProductOrServiceArea"
)

context("replace_nas_with_unlabeled")
def_data<-replace_nas_with_unlabeled(def_data,"SubCustomer","Uncategorized")

def_data<-read_and_join(def_data,
                        "Lookup_SubCustomer.csv",
                        by=c("Customer","SubCustomer"),
                        add_var="SubCustomer.platform",
                        new_var_checked=TRUE
)

labels_and_colors<-prepare_labels_and_colors(def_data,"SubCustomer")

def_data<-replace_nas_with_unlabeled(def_data,"PlatformPortfolio")

labels_and_colors<-prepare_labels_and_colors(def_data)


column_key<-get_column_key(def_data)

# write output to CleanedVendorSize.csv

save(def_data,labels_and_colors, file="2016_unaggregated_FPDS.Rda")

ggplot(data = subset(def_data),
  aes(x=Fiscal.Year,
    y=Action.Obligation.2016,
    fill = SubCustomer)) +
  geom_bar(width=.7,stat="identity") +
  ggtitle("Contract Obligations") +
  # facet_wrap(~ Faceting, ncol = 2, scales="free_y",
  #   drop = TRUE) +
  # scale_fill_manual(
  #   values = structure(as.character(Pricing.Mechanism.sum$ColorRGB), names = as.character(Pricing.Mechanism.sum$Label)))+
  # c(
  #     # "AllPrimes" = "#33FF66",
  #     "PrimeNotReportInFSRS" =  "#33FF66",
  #     "PrimeReportInFSRS" =  "#0066FF",
  #     "SubReportInFSRS" = "#FF6699")) +
  get_plot_theme() +
  xlab("Fiscal Year") +
  ylab("DoD Contract Obligated Amount in billion $") +
  theme(plot.caption = element_text(
    size = 12, face = "bold", color = "#554449", family = "Open Sans"
  )) +
  labs(caption = "Source: FPDS; CSIS analysis", size = 30, family= "Open Sans")

load(system.file("extdata",
  "Crisis_Funding.RData",
  package = "csis360"))

  # "tests//testthat//Crisis_Funding.RData")
FullData<-replace_nas_with_unlabeled(FullData,"Theater")

labels_and_colors<-prepare_labels_and_colors(FullData,"Theater")





DLApblScore  <- read.csv(system.file("extdata",
                                     "DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt",
                                     package = "csis360"),
  header = TRUE, sep = "\t", dec = ".", strip.white = TRUE, row.names=NULL,
  na.strings = c("NULL","NA",""),
  stringsAsFactors = TRUE
)
#Error message test for by missing
test_that("read_and_join na special cases", {
expect_error(read_and_join(DLApblScore,
                               "Defense_Major_Command_Codes_and_Offices.csv",
                               by=c("Fiscal.Year",
                                    "ContractingAgencyID",
                                    "ContractingOfficeID"),
                               new_var_checked=FALSE))
})

#Test for the handling of na in the inputs
DLApblScore<-read_and_join(DLApblScore,
                               "Lookup_MajorCommandID.csv",
                               by="MajorCommandID",
                               skip_check_var=c("ContractingOfficeCode",
                                                "ContractingOfficeName"))





# format_data_for_plot(full_data,
#   "Fiscal.Year",
#   "")


load(system.file("extdata",
                 "2016_unaggregated_FPDS.Rda",
                 package = "csis360"))

total_data <- format_data_for_plot(data=full_data,
                                   share=FALSE,
                                   fy_var="Fiscal.Year",
                                   start_fy=2009,
                                   end_fy=2016,
                                   y_var="Action.Obligation.2016",
                                   color_var="SubCustomer",
                                   facet_var="SubCustomer",
                                   labels_and_colors=labels_and_colors)



build_plot(data=total_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action.Obligation.2016",
           color_var="SubCustomer",
           facet_var="SubCustomer",
           labels_and_colors=labels_and_colors,
           column_key=column_key)

pp_data <- format_data_for_plot(data=full_data,
                                   share=FALSE,
                                   fy_var="Fiscal.Year",
                                   start_fy=2009,
                                   end_fy=2016,
                                   y_var="Action.Obligation.2016",
                                   color_var="PlatformPortfolio",
                                   facet_var="PlatformPortfolio",
                                   labels_and_colors=labels_and_colors)



build_plot(data=pp_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action.Obligation.2016",
           color_var="PlatformPortfolio",
           facet_var="PlatformPortfolio",
           labels_and_colors=labels_and_colors,
           column_key=column_key)

build_plot(data=pp_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action.Obligation.2016",
           color_var="PlatformPortfolio",
           # facet_var="PlatformPortfolio",
           labels_and_colors=labels_and_colors,
           column_key=column_key)

colnames(full_data)


area_data <- format_data_for_plot(data=full_data,
                                share=FALSE,
                                fy_var="Fiscal.Year",
                                start_fy=2009,
                                end_fy=2016,
                                y_var="Action.Obligation.2016",
                                color_var="ProductServiceOrRnDarea.sum",
                                facet_var="None",
                                labels_and_colors=labels_and_colors)


build_plot(data=area_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action.Obligation.2016",
           color_var="ProductServiceOrRnDarea.sum",
           facet_var="None",
           labels_and_colors=labels_and_colors,
           column_key=column_key)


area_vendor_data <- format_data_for_plot(data=full_data,
                                  share=FALSE,
                                  fy_var="Fiscal.Year",
                                  start_fy=2009,
                                  end_fy=2016,
                                  y_var="Action.Obligation.2016",
                                  color_var="ProductServiceOrRnDarea.sum",
                                  facet_var="Shiny.VendorSize",
                                  labels_and_colors=labels_and_colors)


build_plot(data=subset(area_vendor_data,ProductServiceOrRnDarea.sum!="Unlabeled"),
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action.Obligation.2016",
           color_var="ProductServiceOrRnDarea.sum",
           facet_var="Shiny.VendorSize",
           labels_and_colors=labels_and_colors,
           column_key=column_key)



build_plot(data=subset(area_vendor_data,ProductServiceOrRnDarea.sum!="Unlabeled"),
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action.Obligation.2016",
           color_var="Shiny.VendorSize",
           facet_var="ProductServiceOrRnDarea.sum",
           labels_and_colors=labels_and_colors,
           column_key=column_key)
