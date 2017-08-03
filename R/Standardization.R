# These are special purpose standardization functions that only make sense when
# used with established CSIS data. They standardize names and colors using
# lookup tables prepared specifically for these variables.
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






source("R\\ApplyLookups.R")


#***********************Standardize Variable Names
#' Standardize variable names
#'
#' @param VAR.path The location of the lookup file
#' @param VAR.existing.df The data frame to be joined
#'
#' @return VAR.existing.df with standardized column names.
#'
#' @section This function is designed to prepare CSIS data files for lookup
#' application. It primarily smooths out variation between different ways we've
#' written SQL statements. It relies on a pre-existing table of variant names.
#' The column names are matched against that table in a case insensitive manner,
#' though no other procedural standardization is applied at this time.
#'
#' @examples FullData<-standardize_variable_names(Path,
#'   FullData)
#'
#' @import
#' @export
standardize_variable_names<- function(VAR.Path,VAR.df){
  #Remove nonsense characters sometimes added to start of files
  colnames(VAR.df)[substring(colnames(VAR.df),1,3)=="?.."]<-
    substring(colnames(VAR.df)[substring(colnames(VAR.df),1,3)=="?.."],4)


  #Consider removing non-alphanumerics _s .s etc.

  #***Standardize variable names
  NameList<-read.csv(
    paste(
      VAR.Path,
      "Lookups\\","Lookup_StandardizeVariableNames.csv",sep=""),
    header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )


  #     NameList<-subset(NameList,toupper(Original) %in% toupper(colnames(VAR.df)))
  for(x in 1:nrow(NameList)){
    #         if(toupper(NameList$Original[[x]]) %in% OldNameListUpper){
    colnames(VAR.df)[toupper(colnames(VAR.df))==toupper(NameList$Original[[x]])]<-
      NameList$Replacement[[x]]
    #         }
  }

  VAR.df
}


#' Prepare Labels And Colors
#'
#' @param VAR.path The location of the lookup file
#' @param VAR.long.DF The data frame to be joined
#' @param VAR.y.series Prepare colors for this columns
#' @param ReplaceNAs If true, replace NAs for column before adding colors
#'
#' @return A new data frame for build on the VAR.y.series column, it will
#' include colors, and order, and proper name labels.
#'
#' @section This function applies standard colors and orders to a single
#' data frame column. Colors and order are drawn from pre-existing lookup tables.
#' When values are missing or wrong, these tables must be manually updated.
#' This function is badly optimized, reading in multiple csvs every time.
#' It is intend for use in data preparation source code and not to be used in a
#' real time web environment.
#'
#' @examples FullData<-standardize_variable_names(Path,
#'   FullData)
#'
#' @import
#' @export
PrepareLabelsAndColors<-function(VAR.path
  ,VAR.long.DF
  ,VAR.y.series
  ,ReplaceNAs=FALSE
  #                                  ,VAR.override.coloration=NA
)
{
  if(ReplaceNAs==TRUE){
    VAR.long.DF<-replace_nas_with_unlabeled(VAR.long.DF,VAR.y.series)
  }

  VAR.long.DF<-as.data.frame(VAR.long.DF)
  #Confirm that the category is even available in the data set.
  if(!VAR.y.series %in% names(VAR.long.DF)){
    stop(paste(VAR.y.series,"is not found in data frame passed to PrepareLabelsAndColors"))
  }



  Coloration<-read.csv(
    paste(VAR.path,"Lookups\\","lookup_coloration.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )

  Coloration<-ddply(Coloration
    , c(.(R), .(G), .(B))
    , transform
    , ColorRGB=as.character(
      if(min(is.na(c(R,G,B)))) {NA}
      else {rgb(max(R),max(G),max(B),max=255)}
    )
  )



  #Translate the category name into the appropriate coloration.key
  #This is used because we have more category names than coloration.key
  Coloration.Key<-read.csv(
    paste(VAR.path,"Lookups\\","lookup_coloration_key.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )
  Coloration.Key<-subset(Coloration.Key, category==VAR.y.series)

  if(nrow(Coloration.Key)==0){
    stop(paste(VAR.y.series,"is missing from Lookup_Coloration.Key.csv"))
  }


  #Limit the lookup table to those series that match the variable
  labels.category.DF<-subset(Coloration, coloration.key==Coloration.Key$coloration.key[1] )

  #Fix oddities involving text
  labels.category.DF$variable <- gsub("\\\\n","\n",labels.category.DF$variable)
  labels.category.DF$Label <- gsub("\\\\n","\n",labels.category.DF$Label)

  if(anyDuplicated(labels.category.DF$variable)>0){
    print(labels.category.DF$variable[
      duplicated(labels.category.DF$variable)])
    stop(paste("Lookup_Coloration.csv has"
      ,sum(duplicated(labels.category.DF$variable))
      ,"duplicate value(s) for category="
      ,Coloration.Key$coloration.key[1], ". See above for a list of missing labels")
    )
  }


  #Check for any values in the VAR.y.series field that are not assigned a color.
  NA.labels<-subset(VAR.long.DF,!(data.frame(VAR.long.DF)[,VAR.y.series] %in% labels.category.DF$variable))

  if (nrow(NA.labels)>0){
    print(unique(NA.labels[,VAR.y.series]))
    stop(paste("Lookup_Coloration.csv is missing"
      ,length(unique(NA.labels[,VAR.y.series]))
      ,"label(s) for category="
      ,Coloration.Key$coloration.key[1], ". See above for a list of missing labels")
    )
  }
  rm(NA.labels,Coloration.Key)

  names.DF<-subset(labels.category.DF
    , variable %in% unique(VAR.long.DF[,VAR.y.series]))

  rm(labels.category.DF)

  #Order the names.DF and then pass on the same order to the actual data in VAR.long.DF
  names.DF<-names.DF[order(names.DF$Display.Order),]


  names.DF
}
