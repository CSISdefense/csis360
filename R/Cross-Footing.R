#Cross-Footing
#These are quality control functions for verify that numbers sum over multiple souces.

#'Returns a dataframe with customer data for quality control check
#'
#' @param Path file path name where Footing files are stored
#' @param Choice.Data
#' @param Which.Data character string determining which data files to call; defaults to ""
#'
#' @return dataframe
#'
#' @details Quality control function
#'
#'
#' @export
load.FPDS.gov.customers.df<-function(
  Path
  ,Choice.Data
  ,Which.Data=""
){


  customers.files<-paste(Path
                         ,"Footing Data\\"
                         ,list.files(path=paste(Path,"Footing Data\\",sep="")
                                     ,pattern=paste("Footing_"
                                                    ,Which.Data
                                                    ,"Customers"
                                                    ,sep=""
                                     )
                         )
                         ,sep=""
  )
  if (file.exists(customers.files)){
    FPDS.gov.customers.df<-read.tables(customers.files,
                                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE,
                                       stringsAsFactors=TRUE
    )

    rm(customers.files)

    FPDS.gov.customers.df<-LimitData(FPDS.gov.customers.df
                                     ,Choice.Data$Customer[Which.Data]
                                     ,Choice.Data$big.ProdServCode[Which.Data]
    )

    FPDS.gov.customers.df
  }
}


#'
#'
#' @param Path A character string of path name
#' @param Choice.Data
#' @param Which.Data
#'
#' @return
#'
#' @details Quality control function
#'
#' @export


load.FPDS.gov.buckets.df<-function(Path,Choice.Data,Which.Data){
  buckets.files<-paste(Path,list.files(path=Path,pattern="Footing_Buckets_"),sep="")

  FPDS.gov.buckets.df<-read.tables(buckets.files,
                                   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE,
                                   stringsAsFactors=FALSE
  )

  FPDS.gov.buckets.df$Contracting.Department.ID<-factor(FPDS.gov.buckets.df$Contracting.Department.ID)
  FPDS.gov.buckets.df$Contracting.Department.Name<-factor(FPDS.gov.buckets.df$Contracting.Department.Name)
  FPDS.gov.buckets.df$Product.or.Service.Code<-factor(FPDS.gov.buckets.df$Product.or.Service.Code)
  FPDS.gov.buckets.df$Product.or.Service.Description<-factor(FPDS.gov.buckets.df$Product.or.Service.Description)

  rm(buckets.files)


  FPDS.gov.buckets.df<-LimitData(FPDS.gov.buckets.df
                                 ,Choice.Data$Customer[Which.Data]
                                 ,Choice.Data$big.ProdServCode[Which.Data]
  )
  FPDS.gov.buckets.df
}

#' Bind multiple csv files into one dataframe
#'
#' @param file.names list each filename of data with same column names
#'
#' @return combined data from multiple files into one data table
#' with an extra column identifying which file the line of data came from
#'
#' @examples data <- read.tables(c("filename1.csv", "filename2.csv"))
#'
#'
#' @export
#Source: http://stackoverflow.com/questions/2104483/how-to-read-table-multiple-files-into-a-single-table-in-r
read.tables <- function(file.names, ...) {
  require(plyr)
  ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)))
}




#'
#'
#' @param FPDS.gov
#' @param customer
#' @param big.ProdServ
#'
#' @return
#'
#' @details Quality control function
#'
#'
#' @export

LimitData <- function(
  FPDS.gov
  ,customer=NULL
  ,big.ProdServ=NULL)
{
  FPDS.gov<-subset(FPDS.gov,select=-c(Filename))
  FPDS.gov<-append_contract_fixes(Path,FPDS.gov)
##  FPDS.gov<-apply_lookups(Path,FPDS.gov)



  #Classify Customers
  if("AgencyID" %in% colnames(FPDS.gov)){
    FPDS.gov<-csis360::read_and_join(FPDS.gov,
                                         "Agency_AgencyID.csv",
                                         by="AgencyID",
                                         replace_na_var="AgencyID",
                                         add_var="Customer"
    )
  }
  else if("Contracting.Agency.ID" %in% colnames(FPDS.gov)){
    FPDS.gov$AgencyID<-FPDS.gov$Contracting.Agency.ID
    FPDS.gov<-csis360::read_and_join(FPDS.gov,
                                         "Agency_AgencyID.csv",
                                         by="AgencyID",
                                         replace_na_var="AgencyID",
                                         add_var=c("Customer","SubCustomer")
    )
  }
  else if("Contracting.Department.ID" %in% colnames(FPDS.gov)){
    FPDS.gov$AgencyID<-FPDS.gov$Contracting.Department.ID
    FPDS.gov<-csis360::read_and_join(FPDS.gov,
                                         "Agency_AgencyID.csv",
                                         by="AgencyID",
                                         replace_na_var="AgencyID",
                                         add_var="Customer"
    )
  }

  #Classify Product or Service Codes
  if("ProductOrServiceCode" %in% colnames(FPDS.gov))
    FPDS.gov<-csis360::read_and_join(FPDS.gov,
                                         "ProductOrServiceCodes.csv",
                                         by="ProductOrServiceCode",
                                         replace_na_var="ProductOrServiceCode",
                                         add_var=c("ServicesCategory","Simple","ProductServiceOrRnDarea","ProductOrServiceArea")
    )


  full_data<-replace_nas_with_unlabeled(FPDS.gov,"SubCustomer","Uncategorized")
  full_data<-csis360::read_and_join(FPDS.gov,
                                    "Lookup_SubCustomer.csv",
                                    by=c("Customer","SubCustomer"),
                                    add_var="SubCustomer.platform"
  )




  if(!(is.null(customer)) && (customer=="") &&!(customer=="") && !(customer=="D3")) {
    FPDS.gov<-subset(FPDS.gov,Customer==customer)
  } else if (!(is.null(customer)) && (customer=="D3")){
    FPDS.gov<-subset(FPDS.gov,Customer  %in% c("Defense","State and IAP"))
  }

  if(!(is.null(big.ProdServ)) && big.ProdServ=="Services" && ("ProductOrServiceArea" %in% names(FPDS.gov))){
    FPDS.gov<-subset(FPDS.gov,ProductOrServiceArea %in% c("PAMS","MED","FRS&C","ICT","ERS","R&D"))
  }
  else if(!(is.null(big.ProdServ)) && big.ProdServ!="" && ("ProductOrServiceArea" %in% names(FPDS.gov))){
    #     stop(paste("In LimitData, big.Prodserv = ",big.ProdServ,"which is not handled."))
    warning(paste("In LimitData, big.Prodserv = ",big.ProdServ,"which is not handled."))
  }
  FPDS.gov
}


#'
#'
#' @param FPDS.gov
#' @param main.DF
#'
#' @return
#'
#' @details Quality control function
#'
#' @export

LimitScope <- function(FPDS.gov, main.DF) {
  FPDS.gov<-subset(FPDS.gov,Fiscal.Year>=min(subset(main.DF$Fiscal.Year
                                                            ,!is.na(main.DF$Fiscal.Year
                                                            ))))
  FPDS.gov<-subset(FPDS.gov,Fiscal.Year<=max(subset(main.DF$Fiscal.Year
                                                            ,!is.na(main.DF$Fiscal.Year)
  )))
  FPDS.gov
}



#'
#'
#' @param Path
#' @param prefix
#' @param file.name
#'
#' @return
#'
#' @details Quality control function
#'
#' @export


import_SQLserver_file <- function(Path
                                  , prefix
                                  , file.name) {
  import.data.file <-read.csv(
    paste(
      #       Path,
      "Data\\",prefix,file.name,sep=""),
    header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=TRUE
  )


  #   import.data.file<-subset(import.data.file,select =-c(Row))

  #   import.data.file<-melt(import.data.file, id.var=c("Fiscal.Year","Section","Total","Checksum"))
  #
  #   import.data.file<- join(
  #     import.data.file,
  #     lookup,
  #     match="first"
  #   )

  #   import.zero.check<-aggregate(abs(import.data.file$value), by=list(import.data.file$variable),FUN = "max")
  #   names(import.zero.check)<-c("variable","value")
  #   import.zero.check<-subset(import.zero.check,import.zero.check$value>0)

  #   import.data.file<-subset(import.data.file,import.data.file$variable %in% import.zero.check$variable)


  #   NA.check.df<-subset(import.data.file, is.na(variable.sum), select=c("variable","variable.detail","variable.sum"))
  #   if(nrow(NA.check.df)>0){
  #     print(unique(NA.check.df))
  #     stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",file.name))
  #   }

  #   NA.check.df<-subset(import.data.file, is.na(variable.detail), select=c("variable","variable.detail","variable.sum"))
  #   if(nrow(NA.check.df)>0){
  #     print(unique(NA.check.df))
  #     stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",file.name))
  #   }


  import.data.file
}

#'
#'
#' @param Path
#' @param df
#'
#' @return
#'
#' @details
#'
#' @export
append_contract_fixes<- function(path,df){
  #   print(nrow(df))


  #Step 1, apply our known fixes to the data
  if("X" %in% names(df))  {
    df<-subset(df,select =-c(X))
  }

  append.fixed.tasks<-read.csv(
    paste(path,"Lookups\\","APPEND_Fixed_Tasks_webtool.csv",sep=""),
    header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE,
    stringsAsFactors=TRUE,
  )

  append.fixed.tasks$Fair.Opportunity.Limited.Sources[is.na(append.fixed.tasks$Fair.Opportunity.Limited.Sources)]<-""
  append.fixed.tasks$IDV.Part.8.Or.Part.13[is.na(append.fixed.tasks$IDV.Part.8.Or.Part.13)]<-""
  append.fixed.tasks$IDV.Multiple.Or.Single.Award.IDV[is.na(append.fixed.tasks$IDV.Multiple.Or.Single.Award.IDV)]<-""
  append.fixed.tasks$IDV.Type[is.na(append.fixed.tasks$IDV.Type)]<-""




  append.fixed.tasks<-subset(
    append.fixed.tasks,
    select=c(
      names(df)
    ))


  #   append.fixed.tasks<-subset(
  #     append.fixed.tasks,
  #     select=c(
  #       Contracting.Agency.Name,
  #       Contracting.Agency.ID,
  #       Contracting.Department.ID,
  #       Award.or.IDV.Type,
  #       IDV.Part.8.Or.Part.13,
  #       IDV.Multiple.Or.Single.Award.IDV,
  #       IDV.Type,
  #       Extent.Competed,
  #       Fair.Opportunity.Limited.Sources,
  #       Number.of.Offers.Received,
  #       Fiscal.Year,
  #       Action.Obligation,
  #       Actions,
  #       Download.Date
  #     ))
  #


  #   if("IDV.Part.8.Or.Part.13" %in% names(df)){
  # #     print(paste("typeof","df",typeof(df$IDV.Part.8.Or.Part.13)))
  # #     print(paste("typeof","append.fixed.tasks",typeof(append.fixed.tasks$IDV.Part.8.Or.Part.13)))
  # #     append.fixed.tasks$IDV.Part.8.Or.Part.13<-as.integer(append.fixed.tasks$IDV.Part.8.Or.Part.13)
  #
  # #     print(paste("typeof","df[1]",typeof(df$IDV.Part.8.Or.Part.13[1])))
  # #     print(paste("typeof","append.fixed.tasks[1]",typeof(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
  #
  #     #    append.fixed.tasks$IDV.Part.8.Or.Part.13<-factor(append.fixed.tasks$IDV.Part.8.Or.Part.13)
  #
  # #     print(paste("is.character","df",is.character(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
  # #     print(paste("is.vector","append.fixed.tasks",is.vector(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
  #
  #     append.fixed.tasks$IDV.Part.8.Or.Part.13<-factor(append.fixed.tasks$IDV.Part.8.Or.Part.13)
  #
  #     print(paste("is.vector","append.fixed.tasks",is.vector(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
  #   }

  #   print(sum(append.fixed.tasks$value))
  #  append.fixed.tasks<-subset(append.fixed.tasks, select=c(Contracting.Agency.Name, Contracting.Agency.ID, Contracting.Department.ID, Award.or.IDV.Type, IDV.Part.8.Or.Part.13, IDV.Multiple.Or.Single.Award.IDV, IDV.Type, Extent.Competed, Fair.Opportunity.Limited.Sources, Number.of.Offers.Received, Fiscal.Year, variable, value, Download.Date))
  #   print(sum(append.fixed.tasks$value))
  #   print(sum(df$value))


  df$Action.Obligation<-FactorToNumber(df$Action.Obligation)

  df$Actions<-FactorToNumber(df$Actions)


  df<-rbind(df, append.fixed.tasks)
  #   print(sum(df$value))
  print("LOOKUP_Fixed_Tasks_webtool.csv")
  print(head(df))
  print(tail(df))

  print(head(append.fixed.tasks))

  df
}
