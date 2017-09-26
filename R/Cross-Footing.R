#Cross-Footing
#These are quality control functions for verify that numbers sum over multiple souces.

load.FPDS.gov.customers.df<-function(
  VAR.Path
  ,VAR.Choice.Data
  ,VAR.Which.Data
){
  
  
  customers.files<-paste(Path
                         ,"Footing Data\\"
                         ,list.files(path=paste(Path,"Footing Data\\",sep="")
                                     ,pattern=paste("Footing_"
                                                    ,as.character(VAR.Choice.Data$ProdServCode.Prefix[VAR.Which.Data])
                                                    ,"Customers.*[.]csv"
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
                                     ,VAR.Choice.Data$Customer[VAR.Which.Data]
                                     ,VAR.Choice.Data$big.ProdServCode[VAR.Which.Data]
    )
    
    FPDS.gov.customers.df
  }
}

load.FPDS.gov.buckets.df<-function(VAR.Path,VAR.Choice.Data,VAR.Which.Data){
  buckets.files<-paste(VAR.Path,"Footing Data\\",list.files(path=paste(VAR.Path,"Footing Data\\",sep=""),pattern="Footing_Buckets.*[.]csv"),sep="")
  
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
                                 ,VAR.Choice.Data$Customer[VAR.Which.Data]
                                 ,VAR.Choice.Data$big.ProdServCode[VAR.Which.Data]
  )
  FPDS.gov.buckets.df
}


#Source: http://stackoverflow.com/questions/2104483/how-to-read-table-multiple-files-into-a-single-table-in-r
read.tables <- function(file.names, ...) {
  require(plyr)
  ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)))
}



LimitData <- function(
  var.FPDS.gov
  ,VAR.customer=NULL
  ,VAR.big.ProdServ=NULL)
{
  var.FPDS.gov<-subset(var.FPDS.gov,select=-c(Filename))
  var.FPDS.gov<-append_contract_fixes(Path,var.FPDS.gov)
  var.FPDS.gov<-apply_lookups(Path,var.FPDS.gov)
  if(!(is.null(VAR.customer)) && (VAR.customer=="") &&!(VAR.customer=="") && !(VAR.customer=="D3")) {
    var.FPDS.gov<-subset(var.FPDS.gov,Customer==VAR.customer)
  } else if (!(is.null(VAR.customer)) && (VAR.customer=="D3")){
    var.FPDS.gov<-subset(var.FPDS.gov,Customer  %in% c("Defense","State and IAP"))
  }
  
  if(!(is.null(VAR.big.ProdServ)) && VAR.big.ProdServ=="Services" && ("ProductOrServiceArea" %in% names(var.FPDS.gov))){
    var.FPDS.gov<-subset(var.FPDS.gov,ProductOrServiceArea %in% c("PAMS","MED","FRS&C","ICT","ERS","R&D"))
  }
  else if(!(is.null(VAR.big.ProdServ))&& VAR.big.ProdServ!=""&& ("ProductOrServiceArea" %in% names(var.FPDS.gov))){
    #     stop(paste("In LimitData, VAR.big.Prodserv = ",VAR.big.ProdServ,"which is not handled."))
    warning(paste("In LimitData, VAR.big.Prodserv = ",VAR.big.ProdServ,"which is not handled."))
  }
  var.FPDS.gov
}

LimitScope <- function(var.FPDS.gov, var.main.DF) {
  var.FPDS.gov<-subset(var.FPDS.gov,Fiscal.Year>=min(subset(var.main.DF$Fiscal.Year
                                                            ,!is.na(var.main.DF$Fiscal.Year
                                                            ))))
  var.FPDS.gov<-subset(var.FPDS.gov,Fiscal.Year<=max(subset(var.main.DF$Fiscal.Year
                                                            ,!is.na(var.main.DF$Fiscal.Year)
  )))
  var.FPDS.gov
}

import_SQLserver_file <- function(VAR.Path
                                  , VAR.prefix
                                  , VAR.file.name) {
  import.data.file <-read.csv(
    paste(
      #       VAR.Path,
      "Data\\",VAR.prefix,VAR.file.name,sep=""),
    header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=TRUE
  )
  
  
  #   import.data.file<-subset(import.data.file,select =-c(Row))
  
  #   import.data.file<-melt(import.data.file, id.var=c("Fiscal.Year","Section","Total","Checksum"))
  #
  #   import.data.file<- join(
  #     import.data.file,
  #     VAR.lookup,
  #     match="first"
  #   )
  
  #   import.zero.check<-aggregate(abs(import.data.file$value), by=list(import.data.file$variable),FUN = "max")
  #   names(import.zero.check)<-c("variable","value")
  #   import.zero.check<-subset(import.zero.check,import.zero.check$value>0)
  
  #   import.data.file<-subset(import.data.file,import.data.file$variable %in% import.zero.check$variable)
  
  
  #   NA.check.df<-subset(import.data.file, is.na(variable.sum), select=c("variable","variable.detail","variable.sum"))
  #   if(nrow(NA.check.df)>0){
  #     print(unique(NA.check.df))
  #     stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
  #   }
  
  #   NA.check.df<-subset(import.data.file, is.na(variable.detail), select=c("variable","variable.detail","variable.sum"))
  #   if(nrow(NA.check.df)>0){
  #     print(unique(NA.check.df))
  #     stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
  #   }
  
  
  import.data.file
}
