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






#' Replace NAs in one column_Name of a data frame with a specified valued
#'
#' @param data A data frame
#' @param column_Name The column_Name to have NAs removed
#' @param replacement Charater string that will replace NAs, by default "Unlabeled"
#'
#' @return Returns a data frame with the column_Name values replaced with replacement,
#' otherwise identical to the original data frame
#'
#' @section This function is intended for preparing columns for use.
#' As far as the function creator understands it, NAs do not match to NAs. However,
#' we often wish to include NAs in a graph under a proper name, such as Unlabeled
#' and with a color of our choosing. One critical step in this process is the addition
#' of the replacement term to the levels of the column_Name factor. Which means we
#' should probably add input protection when non factors are passed. I also haven't
#' tested the handling if multiple columns are passed, I think it may not work.
#'
#' @examples VAR.long.DF<-replace_nas_with_unlabeled(data=data.DF
#'   ,column_Name="SubCustomer"),
#'   replacement = "Uncategorized",
#'   )
#'
#' @import
#' @export
replace_nas_with_unlabeled<- function(data,
                                      column_Name,
                                      replacement="Unlabeled"){
  data<-as.data.frame(data)
  if(any(is.na(data[column_Name,]))){
    #Make sure the replacement value is in the is within the list of levels
    if (!(replacement %in% levels(data[,column_Name]))){
      data[,column_Name]<-addNA(data[,column_Name],ifany=TRUE)
    }
    levels(data[,column_Name])[is.na(levels(data[,column_Name]))] <- replacement
  }
  data
}



#' An internal function to check for NAs in columns after a join
#'
#' @param data A data frame
#' @param input_name The column_Name(s) that had been used to join
#' @param output_name The column_Name(s) that result from the join
#' @param lookup_name The file used in the join
#'
#' @return None. Instead the function raises an error if there are NAs.
#'
#' @section This function is intended to catch gaps in lookup tables
#' and to alert the developer before they can come into use. The core intent
#' is to throw an error message that, if needed, will guide the developer to
#' the file they need to update and the rows they need to add.
#'
#' @examples na_check(data,
#'   input_name=by,
#'   output_name=na_check_columns,
#'   lookup_name=lookup_name)
#'
#' @import
#' @export
na_check<-function(data
  , input_name
  , output_name
  , lookup_name
){
  #Limit just to relevant columns
  na_check.df<-subset(data
    , select=c(input_name,output_name)
  )
  #Drop all complete rows
  na_check.df<-na_check.df[!complete.cases(na_check.df),]

  if(nrow(na_check.df)>0){
    print(unique(na_check.df))
    stop(paste(nrow(na_check.df)
      ,"rows of NAs generated in "
      ,paste(output_name,collapse=", ")
      ,"from "
      ,lookup_name)
    )
  }
}



#' Remove byte order marks, these appear before the first column name
#'
#' @param data A /code(data.frame)
#'
#' @return /code(data) with the first column stripped of a byte order mark, if present.
#'
#' @section Byte order marks are included at the start of UTF
#' text files and if not properly processed on read in, can add
#' nonsense characters to the start of the first column of a the
#' /code(data.frame) read in from a text file.
#'
#' @examples remove_bom(data)
#'
#' @import
#' @export
remove_bom<-function(data
){
  #Remove nonsense characters sometimes added to start of files
  colnames(data)[substring(colnames(data),1,3)=="ï.."]<-
    substring(colnames(data)[substring(colnames(data),1,3)=="ï.."],4)

  colnames(data)[substring(colnames(data),1,3)=="?.."]<-
    substring(colnames(data)[substring(colnames(data),1,3)=="?.."],4)

  data
}


#' Read in an external file and join it with an existing data frame.
#'
#' @param data The data frame to be joined
#' @param file_name The name of the lookup file
#' @param path="K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\", The location of the lookup file
#' @param directory="Lookups\\" The directory within the path that holds the lookup
#' @param by=NULL The columns used to join, if not provided, matching columns will be used
#' @param replace_na_column_name=NULL Before the join, these columns will have NAs values replaced
#' @param new_columns_trump=TRUE Should the function replace for common columns not used to join?
#' @param na_check_columns=NULL, What new columns should be checked for NA values?
#' @param only_keep_checked_columns=FALSE Should only checked new columns be kept?
#'
#' @return The data frame plus new columns from the lookup file. If only_keep_checked_columns is
#' true and only new columns listed in na_check_columns will be kept. Note to self, should
#' add input protection that throws an error if only_keep_checked_columns is set to true when
#' na_check_columns is false.
#'
#' @section This function is an elaborate join with various quality check measures thrown in.
#' At its simplest, it just joins the existing data frame with the passed file. But along the way
#' it will make some fixes to common CSV errors and also take advantage of some known facts about
#' how CSIS data is organized.
#'
#' @examples na_check(data,
#'   input_name=by,
#'   output_name=na_check_columns,
#'   file_name=file_name)
#'
#' @import plyr
#' @export
read_and_join<-function(
  data,
  file_name,
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  directory="Lookups\\",
  by=NULL,
  replace_na_column_name=NULL,
  new_columns_trump=TRUE,
  na_check_columns=NULL,
  only_keep_checked_columns=FALSE){

  if(!is.null(replace_na_column_name)){
    data<-replace_nas_with_unlabeled(data,replace_na_column_name)
  }

  lookup_file<-read.csv(
    paste(path,directory,file_name,sep=""),
    header=TRUE, sep=ifelse(substring(file_name,nchar(file_name)-3)==".csv",",","\t"), na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE  #This can get weird when true, as sometimes it confuses numerical variables and factors
  )

  #Remove nonsense characters sometimes added to start of the input file
  data<-remove_bom(data)
  lookup_file<-remove_bom(lookup_file)

  #Clear out any fields held in common not used in the joining
  if(!is.null(by)){
    droplist<-names(lookup_file)[names(lookup_file) %in% names(data)]
    droplist<-droplist[droplist!=by]
    if(new_columns_trump)
      data<-data[,!names(data) %in% droplist]
    else
      lookup_file<-lookup_file[,!names(lookup_file) %in% droplist]
  }

  #Fixes for Excel's penchant to drop leading 0s.
  if("Contracting.Agency.ID" %in% names(lookup_file) & "data" %in% names(lookup_file)){
    lookup_file$Contracting.Agency.ID<-factor(str_pad(lookup_file$Contracting.Agency.ID,4,side="left",pad="0"))
    data$Contracting.Agency.ID<-as.character(data$Contracting.Agency.ID)
    data$Contracting.Agency.ID[is.na(data$Contracting.Agency.ID=="")]<-"0000"
    data$Contracting.Agency.ID<-factor(str_pad(data$Contracting.Agency.ID,4,side="left",pad="0"))
  }

  if("CSIScontractID" %in% colnames(lookup_file)){
    if(!is.numeric(lookup_file$CSIScontractID)){
      lookup_file$CSIScontractID<-as.numeric(as.character(lookup_file$CSIScontractID))
    }
  }

  if(is.null(by)){
    data<- plyr::join(
      data,
      lookup_file,
      match="first"
    )
  }
  else{
    data<- plyr::join(
      data,
      lookup_file,
      match="first",
      by=by
    )
  }

  if(!is.null(by)&!is.null(na_check_columns)){
    na_check(data,
      input_name=by,
      output_name=na_check_columns,
      lookup_name = file_name)
    #Clear out any fields held in common not used in the joining

    if(only_keep_checked_columns==TRUE){
      droplist<-names(lookup_file)[!names(lookup_file) %in% by
        &!names(lookup_file) %in% na_check_columns]

      data<-data[,!names(data) %in% droplist]
    }
  }

  data
}

