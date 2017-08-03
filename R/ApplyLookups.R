# This file contains functions to join data frames to pre-established CSIS
# lookup files
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:       'Ctrl + Shift + E'
#   Test Package:        'Ctrl + Shift + T'




#' Replace NAs in one column of a data frame with a specified valued
#'
#' @param VAR.df A data frame
#' @param VAR.column The column to have NAs removed
#' @param replacement Charater string that will replace NAs, by default "Unlabeled"
#'
#' @return Returns a data frame with the VAR.column values replaced with replacement,
#' otherwise identical to the original data frame
#'
#' @section This function is intended for preparing columns for use.
#' As far as the function creator understands it, NAs do not match to NAs. However,
#' we often wish to include NAs in a graph under a proper name, such as Unlabeled
#' and with a color of our choosing. One critical step in this process is the addition
#' of the replacement term to the levels of the VAR.column factor. Which means we
#' should probably add input protection when non factors are passed. I also haven't
#' tested the handling if multiple columns are passed, I think it may not work.
#'
#' @examples VAR.long.DF<-replace_nas_with_unlabeled(VAR.df=data.DF
#'   ,VAR.column="SubCustomer"),
#'   replacement = "Uncategorized",
#'   )
#'
#' @import
#' @export
replace_nas_with_unlabeled<- function(VAR.df,
                                      VAR.column,
                                      replacement="Unlabeled"){
  VAR.df<-as.data.frame(VAR.df)
  if(any(is.na(VAR.df[VAR.column,]))){
    #Make sure the replacement value is in the is within the list of levels
    if (!(replacement %in% levels(VAR.df[,VAR.column]))){
      VAR.df[,VAR.column]<-addNA(VAR.df[,VAR.column],ifany=TRUE)
    }
    levels(VAR.df[,VAR.column])[is.na(levels(VAR.df[,VAR.column]))] <- replacement
  }
  VAR.df
}



#' An internal function to check for NAs in columns after a join
#'
#' @param VAR.df A data frame
#' @param VAR.input The column(s) that had been used to join
#' @param VAR.output The column(s) that result from the join
#' @param VAR.file The file used in the join
#'
#' @return None. Instead the function raises an error if there are NAs.
#'
#' @section This function is intended to catch gaps in lookup tables
#' and to alert the developer before they can come into use. The core intent
#' is to throw an error message that, if needed, will guide the developer to
#' the file they need to update and the rows they need to add.
#'
#' @examples NA.check(VAR.existing.df,
#'   VAR.input=by,
#'   VAR.output=NA.check.columns,
#'   VAR.file=VAR.file)
#'
#' @import
#' @export
NA.check<-function(VAR.df
  , VAR.input
  , VAR.output
  , VAR.file
){
  #Limit just to relevant columns
  NA.check.df<-subset(VAR.df
    , select=c(VAR.input,VAR.output)
  )
  #Drop all complete rows
  NA.check.df<-NA.check.df[!complete.cases(NA.check.df),]

  if(nrow(NA.check.df)>0){
    print(unique(NA.check.df))
    stop(paste(nrow(NA.check.df)
      ,"rows of NAs generated in "
      ,paste(VAR.output,collapse=", ")
      ,"from "
      ,VAR.file)
    )
  }
}


#' Read in an external file and join it with an existing data frame.
#'
#' @param VAR.path The location of the lookup file
#' @param VAR.file The name of the lookup file
#' @param VAR.existing.df The data frame to be joined
#' @param directory="Lookups\\" The directory within the path that holds the lookup
#' @param by=NULL The columns used to join, if not provided, matching columns will be used
#' @param ReplaceNAsColumns=NULL Before the join, these columns will have NAs values replaced
#' @param LookupTrumps=TRUE Should the function replace for common columns not used to join?
#' @param NA.check.columns=NULL, What new columns should be checked for NA values?
#' @param OnlyKeepCheckedColumns=FALSE Should only checked new columns be kept?
#'
#' @return The data frame plus new columns from the lookup file. If OnlyKeepCheckedColumns is
#' true and only new columns listed in NA.check.columns will be kept. Note to self, should
#' add input protection that throws an error if OnlyKeepCheckedColumns is set to true when
#' NA.check.columns is false.
#'
#' @section This function is an elaborate join with various quality check measures thrown in.
#' At its simplest, it just joins the existing data frame with the passed file. But along the way
#' it will make some fixes to common CSV errors and also take advantage of some known facts about
#' how CSIS data is organized.
#'
#' @examples NA.check(VAR.existing.df,
#'   VAR.input=by,
#'   VAR.output=NA.check.columns,
#'   VAR.file=VAR.file)
#'
#' @import plyr
#' @export
read_and_join<-function(VAR.path,
  VAR.file,
  VAR.existing.df,
  directory="Lookups\\",
  by=NULL,
  ReplaceNAsColumns=NULL,
  LookupTrumps=TRUE,
  NA.check.columns=NULL,
  OnlyKeepCheckedColumns=FALSE){

  if(!is.null(ReplaceNAsColumns)){
    VAR.existing.df<-replace_nas_with_unlabeled(VAR.existing.df,ReplaceNAsColumns)
  }


  lookup.file<-read.csv(
    paste(VAR.path,directory,VAR.file,sep=""),
    header=TRUE, sep=ifelse(substring(VAR.file,nchar(VAR.file)-3)==".csv",",","\t"), na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE  #This can get weird when true, as sometimes it confuses numerical variables and factors
  )

  #Remove nonsense characters sometimes added to start of the input file
  colnames(VAR.existing.df)[substring(colnames(VAR.existing.df),1,3)=="?.."]<-
    substring(colnames(VAR.existing.df)[substring(colnames(VAR.existing.df),1,3)=="?.."],4)

  #Remove nonsense characters sometimes added to start of the lookup file
  colnames(lookup.file)[substring(colnames(lookup.file),1,3)=="?.."]<-
    substring(colnames(lookup.file)[substring(colnames(lookup.file),1,3)=="?.."],4)

  #Clear out any fields held in common not used in the joining
  if(!is.null(by)){
    droplist<-names(lookup.file)[names(lookup.file) %in% names(VAR.existing.df)]
    droplist<-droplist[droplist!=by]
    if(NewColumnsTrump)
      VAR.existing.df<-VAR.existing.df[,!names(VAR.existing.df) %in% droplist]
    else
      lookup.file<-lookup.file[,!names(lookup.file) %in% droplist]
  }


  #Fixes for Excel's penchant to drop leading 0s.
  if("Contracting.Agency.ID" %in% names(lookup.file) & "VAR.existing.df" %in% names(lookup.file)){
    lookup.file$Contracting.Agency.ID<-factor(str_pad(lookup.file$Contracting.Agency.ID,4,side="left",pad="0"))
    VAR.existing.df$Contracting.Agency.ID<-as.character(VAR.existing.df$Contracting.Agency.ID)
    VAR.existing.df$Contracting.Agency.ID[is.na(VAR.existing.df$Contracting.Agency.ID=="")]<-"0000"
    VAR.existing.df$Contracting.Agency.ID<-factor(str_pad(VAR.existing.df$Contracting.Agency.ID,4,side="left",pad="0"))
  }

  if("CSIScontractID" %in% colnames(lookup.file)){
    if(!is.numeric(lookup.file$CSIScontractID)){
      lookup.file$CSIScontractID<-as.numeric(as.character(lookup.file$CSIScontractID))
    }
  }

  if(is.null(by)){
    VAR.existing.df<- plyr::join(
      VAR.existing.df,
      lookup.file,
      match="first"
    )
  }
  else{
    VAR.existing.df<- plyr::join(
      VAR.existing.df,
      lookup.file,
      match="first",
      by=by

    )

  }

  if(!is.null(by)&!is.null(NA.check.columns)){
    NA.check(VAR.existing.df,
      VAR.input=by,
      VAR.output=NA.check.columns,
      VAR.file=VAR.file)
    #Clear out any fields held in common not used in the joining

    if(OnlyKeepCheckedColumns==TRUE){
      droplist<-names(lookup.file)[!names(lookup.file) %in% by
        &!names(lookup.file) %in% NA.check.columns]

      VAR.existing.df<-VAR.existing.df[,!names(VAR.existing.df) %in% droplist]
    }
  }

  VAR.existing.df
}

