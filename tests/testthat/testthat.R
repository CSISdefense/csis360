library(testthat)
library(csis360)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(magrittr)
#ERRORS: prompts general warnings regarding being built under R version 3.5.3 and replacing previous imports. Warnings are N/A to running of code.

# read in data
full_data <- read.csv(system.file("extdata",
                                  "2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
                                  package = "csis360")  ,
   na.strings=c("NA","NULL"))

#context("remove_bom")
bom_data<-remove_bom(full_data)
  test_that("remove_bom fixes ï..", {
    expect_equal(colnames(bom_data)[1], "Fiscal.Year")
})



#context("standardize_variable_names")

full_data <- read.csv(system.file("extdata",
                                  "2016_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
                                  package = "csis360"),
  na.strings=c("NA","NULL"))

std_data<-standardize_variable_names(full_data)

test_that("standardize_variable_names fixes ï..", {
  expect_equal(colnames(std_data)[1], "Fiscal.Year")
})

test_that("Rename the SumOfObligatedAmount Column", {
  expect_equal(colnames(std_data)[9], "Action_Obligation")
})



# coerce Amount to be a numeric variable
std_data$Action_Obligation <- as.numeric(std_data$Action_Obligation)
std_data$Number.Of.Actions <- as.numeric(std_data$Number.Of.Actions)
std_data$Fiscal.Year <- as.numeric(std_data$Fiscal.Year)

# discard pre-2000
std_data <- subset(std_data, Fiscal.Year >= 2000)

# Reassign Fiscal.Year column name to match deflate() expectations
colnames(std_data)[colnames(std_data)=="Fiscal.Year"] <- "Fiscal_Year"


#context("deflate")
def_data<-deflate(std_data,
  money_var = "Action_Obligation",
  deflator_var="GDPdeflator2016"
)

expect_true("Action_Obligation.GDPdeflator2016" %in% colnames(def_data))
expect_true("Action_Obligation_Then_Year" %in% colnames(def_data))
expect_false("Action_Obligationr" %in% colnames(def_data))


#context("read_and_join")

#Consolidate categories for Vendor Size
def_data<-read_and_join(def_data,
                        "LOOKUP_Contractor_Size.csv",
                        by="Vendor.Size",
                        add_var="Shiny.VendorSize"
)
# ERROR: creating unexpected ',' when listing components

# classify competition and a zip file

# debug()
# https://github.com/CSISdefense/csis360/blob/master/inst/extdata/Lookup_SQL_CompetitionClassification.zip
# https://github.com/CSISdefense/csis360/blob/master/inst/extdata/Lookup_SQL_CompetitionClassification.zip
# https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/Lookup_SQL_CompetitionClassification.zip
# https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt
# https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt
# https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt
#
#
# https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/Lookup_SQL_CompetitionClassification.zip
# file.exists("https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/Lookup_SQL_CompetitionClassification.zip")
# file.exists("https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt")
debug(read_and_join_experiment)
def_data<-read_and_join_experiment(def_data,
                        "DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt",
                        zip_file="DLA_Contract_SP_ContractPBLscoreSubCustomerProdServOffice.txt",
                        by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                        replace_na_var="ClassifyNumberOfOffers",
                        path="https://raw.githubusercontent.com/CSISdefense/csis360/master/inst/extdata/",
                        directory="",
                        add_var=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum")
)


#Classify Product or Service Codes
def_data<-read_and_join(def_data,
                        "LOOKUP_Buckets.csv" ,
                        #by="ProductOrServiceArea" ,
                        add_var="ProductServiceOrRnDarea.sum" ,
                        replace_na_var = "ProductOrServiceArea"
)

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
# WARNING: Produces warning about coercing into character vector. Ignore.

# write output to CleanedVendorSize.csv

save(def_data,labels_and_colors, file="2016_unaggregated_FPDS.Rda")

ggplot(data = subset(def_data),
  aes(x=Fiscal.Year,
    y=Action_Obligation.2016,
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
#ERROR: States that there is no package called 'sysfonts'

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

component_data <- format_data_for_plot(data=full_data,
                                   share=FALSE,
                                   fy_var="Fiscal.Year",
                                   start_fy=2009,
                                   end_fy=2016,
                                   y_var="Action_Obligation.2016",
                                   color_var="SubCustomer.platform",
                                   facet_var="SubCustomer.platform",
                                   labels_and_colors=labels_and_colors)



build_plot(data=component_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="SubCustomer.platform",
           facet_var="None",
           labels_and_colors=labels_and_colors,
           column_key=column_key)+theme(legend.position = "right")

build_plot(data=component_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="SubCustomer.platform",
           facet_var="SubCustomer.platform",
           labels_and_colors=labels_and_colors,
           column_key=column_key,legend=FALSE)


milserv_data <- format_data_for_plot(data=full_data,
                                       share=FALSE,
                                       fy_var="Fiscal.Year",
                                       start_fy=2009,
                                       end_fy=2016,
                                       y_var="Action_Obligation.2016",
                                       color_var="SubCustomer.platform",
                                       facet_var="SubCustomer.platform",
                                       labels_and_colors=labels_and_colors)



build_plot(data=milserv_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="SubCustomer.platform",
           facet_var="None",
           labels_and_colors=labels_and_colors,
           column_key=column_key)+theme(legend.position = "right")

build_plot(data=milserv_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="SubCustomer.platform",
           facet_var="SubCustomer.platform",
           labels_and_colors=labels_and_colors,
           column_key=column_key,legend=FALSE)


pp_data <- format_data_for_plot(data=full_data,
                                   share=FALSE,
                                   fy_var="Fiscal.Year",
                                   start_fy=2009,
                                   end_fy=2016,
                                   y_var="Action_Obligation.2016",
                                   color_var="PlatformPortfolio",
                                   facet_var="PlatformPortfolio",
                                   labels_and_colors=labels_and_colors)



build_plot(data=pp_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="PlatformPortfolio",
           facet_var="PlatformPortfolio",
           labels_and_colors=labels_and_colors,
           column_key=column_key,
           legend=FALSE)

build_plot(data=pp_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
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
                                y_var="Action_Obligation.2016",
                                color_var="ProductServiceOrRnDarea.sum",
                                facet_var="None",
                                labels_and_colors=labels_and_colors)


build_plot(data=area_data,
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="ProductServiceOrRnDarea.sum",
           facet_var="None",
           labels_and_colors=labels_and_colors,
           column_key=column_key)


area_vendor_data <- format_data_for_plot(data=full_data,
                                  share=FALSE,
                                  fy_var="Fiscal.Year",
                                  start_fy=2009,
                                  end_fy=2016,
                                  y_var="Action_Obligation.2016",
                                  color_var="ProductServiceOrRnDarea.sum",
                                  facet_var="Shiny.VendorSize",
                                  labels_and_colors=labels_and_colors)


build_plot(data=subset(area_vendor_data,ProductServiceOrRnDarea.sum!="Unlabeled"),
           chart_geom="Bar Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="ProductServiceOrRnDarea.sum",
           facet_var="Shiny.VendorSize",
           labels_and_colors=labels_and_colors,
           column_key=column_key)



comp_data <- format_data_for_plot(data=full_data,
                                         share=TRUE,
                                         fy_var="Fiscal.Year",
                                         start_fy=2009,
                                         end_fy=2016,
                                         y_var="Action_Obligation.2016",
                                         color_var="Competition.effective.only",
                                         # facet_var="None",
                                         labels_and_colors=labels_and_colors)


build_plot(data=subset(comp_data,Competition.effective.only!="Unlabeled"),
           chart_geom="Line Chart",
           share=FALSE,
           x_var="Fiscal.Year",
           y_var="Action_Obligation.2016",
           color_var="Competition.effective.only",
           # facet_var="None",
           labels_and_colors=labels_and_colors,
           column_key=column_key)

#Defense Contracts
load(file="tests/testthat/defense_CSIScontractID_mini.Rdata")

head(def)
# undebug(transform_contract)
def<-csis360::transform_contract(def)
def<-csis360::transform_contract(def)


load(def_full,file="tests/testthat/def_all_mini.Rdata")


def_full<-csis360::read_and_join_experiment(data=def_full
                                            , "Contract.SP_ContractLocationCustomer.txt"
                                            ,path=""
                                            ,dir="tests/testthat/"
                                            ,by="CSIScontractID"
                                            ,new_var_checked=FALSE
)



def_full<-csis360::read_and_join_experiment(data=def_full
                                            , "Contract.SP_ContractUnmodifiedandOutcomeDetailsCustomer.txt"
                                            ,path=""
                                            ,dir="tests/testthat/"
                                            ,by="CSIScontractID"
                                            ,new_var_checked=FALSE
)
debug(read_and_join_experiment)
def_full<-read_and_join_experiment(data=def_full
                                            , "Contract.SP_ContractUnmodifiedAndOutcomeDetailsCustomer.txt"
                                            ,path=""
                                            ,dir="tests/testthat/"
                                            ,by="CSIScontractID"
                                            ,new_var_checked=FALSE
                                            ,zip_file="Contract.SP_ContractUnmodifiedAndOutcomeDetailsCustomer.zip"
)

def_full<-read_and_join_experiment(data=def_full
                                   , "Contract_SP_ContractDefenseSubCustomer.csv"
                                   ,path=""
                                   ,dir="tests/testthat/"
                                   ,by="CSIScontractID"
                                   ,new_var_checked=FALSE
                                   ,zip_file="Contract_SP_ContractDefenseSubCustomer.zip"
)

readr::read_delim(
  "tests/testthat/Contract_SP_ContractDefenseSubCustomer.zip",
  col_names=TRUE,
  delim=ifelse(substring("Contract.SP_ContractUnmodifiedAndOutcomeDetailsCustomer.txt",
                         nchar("Contract.SP_ContractUnmodifiedAndOutcomeDetailsCustomer.txt")-3)==".csv",",","\t"),
  na=c("NA","NULL"),
  trim_ws=TRUE
)

file.info("tests/testthat/Contract_SP_ContractDefenseSubCustomer.zip")$size


read.table(unz(description="tests/testthat/Contract.SP_ContractUnmodifiedandOutcomeDetailsCustomer.zip",
             filename="Contract.SP_ContractUnmodifiedandOutcomeDetailsCustomer.txt"))

read.table(unz(description="tests/testthat/Defense_Contract_SP_ContractBucketPlatformCustomer.zip",
               filename="Defense_Contract_SP_ContractBucketPlatformCustomer.csv"))

read.table(unz(description="tests/testthat/Contract_SP_ContractDefenseSubCustomer.zip",
               filename="Contract_SP_ContractDefenseSubCustomer.csv"),
           header=TRUE)


read.table(unz(description="tests/testthat/Defense_Contract_SP_ContractBucketPlatformCustomer.zip",
               filename="Defense_Contract_SP_ContractBucketPlatformCustomer.csv"),
           header=TRUE)

