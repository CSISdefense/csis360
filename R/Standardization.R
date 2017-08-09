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






#***********************Standardize Variable Names
#' Standardize variable names
#'
#' @param data.df The data frame to be joined
#' @param path The location of the lookup file
#'
#' @return data.df with standardized ColumnName names.
#'
#' @section This function is designed to prepare CSIS data files for lookup
#' application. It primarily smooths out variation between different ways we've
#' written SQL statements. It relies on a pre-existing table of variant names.
#' The ColumnName names are matched against that table in a case insensitive manner,
#' though no other procedural standardization is applied at this time.
#'
#' @examples FullData<-standardize_variable_names(
#'   FullData,
#'   Path)
#'
#' @import
#' @export
standardize_variable_names<- function(data.df,
                                      path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/blob/master/data/style/"
){
  #Remove nonsense characters sometimes added to start of files
  colnames(data.df)[substring(colnames(data.df),1,3)=="?.."]<-
    substring(colnames(data.df)[substring(colnames(data.df),1,3)=="?.."],4)


  #Consider removing non-alphanumerics _s .s etc.

  #***Standardize variable names
  NameList<-read.csv(
    paste(
      path,
      "Lookup_StandardizeVariableNames.csv",sep=""),
    header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )


  #     NameList<-subset(NameList,toupper(Original) %in% toupper(colnames(data.df)))
  for(x in 1:nrow(NameList)){
    #         if(toupper(NameList$Original[[x]]) %in% OldNameListUpper){
    colnames(data.df)[toupper(colnames(data.df))==toupper(NameList$Original[[x]])]<-
      NameList$Replacement[[x]]
    #         }
  }

  data.df
}


#' Prepare Labels And Colors
#'
#' @param data.df The data frame to be joined
#' @param ColumnName Prepare colors for this columns
#' @param ReplaceNAs If true, replace NAs for ColumnName before adding colors
#' @param path The location of the lookup file
#'
#' @return A new data frame for build on the ColumnName ColumnName, it will
#' include colors, and order, and proper name labels.
#'
#' @section This function applies standard colors and orders to a single
#' data frame ColumnName. Colors and order are drawn from pre-existing lookup tables.
#' When values are missing or wrong, these tables must be manually updated.
#' This function is badly optimized, reading in multiple csvs every time.
#' It is intend for use in data preparation source code and not to be used in a
#' real time web environment.
#'
#' @examples FullData<-standardize_variable_names(Path,
#'   FullData)
#'
#' @import plyr
#' @export
PrepareLabelsAndColors<-function(data.df
  ,ColumnName
  ,ReplaceNAs=FALSE
  ,path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/blob/master/data/style/"
  #                                  ,VAR.override.coloration=NA
)
{
  if(ReplaceNAs==TRUE){
    data.df<-replace_nas_with_unlabeled(data.df,ColumnName)
  }

  data.df<-as.data.frame(data.df)
  #Confirm that the category is even available in the data set.
  if(!ColumnName %in% names(data.df)){
    stop(paste(ColumnName,"is not found in data frame passed to PrepareLabelsAndColors"))
  }



  Coloration<-read.csv(
    paste(path,"data\\style\\","lookup_coloration.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )

  #Translate the category name into the appropriate coloration.key
  #This is used because we have more category names than coloration.key
  Coloration.Key<-read.csv(
    paste(path,"data\\style\\","lookup_coloration_key.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )
  Coloration.Key<-subset(Coloration.Key, ColumnName==ColumnName)

  if(nrow(Coloration.Key)==0){
    stop(paste(ColumnName,"is missing from Lookup_Coloration.Key.csv"))
  }


  #Limit the lookup table to those series that match the variable
  labels.category.data.df<-subset(Coloration, coloration.key==Coloration.Key$coloration.key[1] )

  #Fix oddities involving text
  labels.category.data.df$variable <- gsub("\\\\n","\n",labels.category.data.df$variable)
  labels.category.data.df$Label <- gsub("\\\\n","\n",labels.category.data.df$Label)

  if(anyDuplicated(labels.category.data.df$variable)>0){
    print(labels.category.data.df$variable[
      duplicated(labels.category.data.df$variable)])
    stop(paste("Lookup_Coloration.csv has"
      ,sum(duplicated(labels.category.data.df$variable))
      ,"duplicate value(s) for category="
      ,Coloration.Key$coloration.key[1], ". See above for a list of missing labels")
    )
  }


  #Check for any values in the ColumnName field that are not assigned a color.
  NA.labels<-subset(data.df,!(data.frame(data.df)[,ColumnName] %in% labels.category.data.df$variable))

  if (nrow(NA.labels)>0){
    print(unique(NA.labels[,ColumnName]))
    stop(paste("Lookup_Coloration.csv is missing"
      ,length(unique(NA.labels[,ColumnName]))
      ,"label(s) for category="
      ,Coloration.Key$coloration.key[1], ". See above for a list of missing labels")
    )
  }
  rm(NA.labels,Coloration.Key)

  names.data.df<-subset(labels.category.data.df
    , variable %in% unique(data.df[,ColumnName]))

  rm(labels.category.data.df)

  #Order the names.data.df and then pass on the same order to the actual data in data.df
  names.data.df<-names.data.df[order(names.data.df$Display.Order),]


  names.data.df
}
