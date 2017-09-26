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
#' @param data The data frame to be joined
#' @param path The location of the lookup file
#' @param var The names to standard, by default all will be done
#'
#' @return data with standardized var names.
#'
#' @section This function is designed to prepare CSIS data files for lookup
#' application. It primarily smooths out variation between different ways we've
#' written SQL statements. It relies on a pre-existing table of variant names.
#' The var names are matched against that table in a case insensitive manner,
#' though no other procedural standardization is applied at this time.
#'
#' @examples FullData<-standardize_variable_names(
#'   FullData,
#'   Path)
#'
#' @import
#' @export
standardize_variable_names<- function(data,
                                      path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/",
                                      var=NULL
){
  #V
  if(!is.null(var) & any(!var %in% colnames(data)))
    stop(paste(var," is not present in colnames(data)."))


  #Remove nonsense characters sometimes added to start of the input file
  data<-remove_bom(data)

  #Consider removing non-alphanumerics _s .s etc.

  if(is.null(var)) var<-colnames(data)

  #***Standardize variable names
  NameList<-read.csv(
    paste(
      path,
      "Lookup_StandardizeVariableNames.csv",sep=""),
    header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )


  #     NameList<-subset(NameList,toupper(Original) %in% toupper(colnames(data)))
  for(x in 1:nrow(NameList)){
    #Limits it to names in var, if passed. By default, all will be covered.
    if(toupper(NameList$Original[[x]]) %in% toupper(var)){
      colnames(data)[toupper(colnames(data))==toupper(NameList$Original[[x]])]<-
        NameList$Replacement[[x]]
    }
  }

  data
}


#' Prepare Labels And Colors
#'
#' @param data The data frame to be joined
#' @param var Prepare colors for this columns
#' @param na_replaced If true, replace NAs for var before adding colors
#' @param path The location of the lookup file
#'
#' @return A new data frame for build on the var var, it will
#' include colors, and order, and proper name labels.
#'
#' @section This function applies standard colors and orders to a single
#' data frame var. Colors and order are drawn from pre-existing lookup tables.
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
prepare_labels_and_colors<-function(data
                                    ,var=NULL
                                    ,na_replaced=FALSE
                                    ,path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/"
                                    #                                  ,VAR.override.coloration=NA
)
{
  if(na_replaced==TRUE){
    data<-replace_nas_with_unlabeled(data,var)
  }
  
  data<-as.data.frame(data)
  
  #Confirm that the category is even available in the data set.
  if(!is.null(var)){
    if(!var %in% names(data)){
      stop(paste(var,"is not found in data frame passed to PrepareLabelsAndColors"))
    }
  }
  
  #Read in coloration
  coloration<-read.csv(
    paste(path,"Lookup_Coloration.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )
  
  #Fix oddities involving coloration text
  coloration$variable <- gsub("\\\\n","\n",coloration$variable)
  coloration$Label <- gsub("\\\\n","\n",coloration$Label)
  
  
  #Translate the category name into the appropriate coloration.key
  #This is used because we have more category names than coloration.key
  column_key<-get_column_key(data)
  
  #If a column has been passed
  if(!is.null(var)){
    column_key<-subset(column_key, column==var)
    
    #Should adjust this to give proper errors for multiple vars
    #when only one is missing
    if(nrow(column_key)<length(var)){
      stop(paste(var,"is missing from Lookup_column_key.csv"))
    }
  }
  else {
    column_key<-subset(column_key, !is.na(coloration.key))
  }
  
  names.data<-NULL
  for(v in (1:nrow(column_key))){
    #Limit the lookup table to those series that match the variable
    labels_category_data<-subset(coloration, coloration.key==
                                   column_key$coloration.key[v] )
    
    if(anyDuplicated(labels_category_data$variable)>0){
      print(labels_category_data$variable[
        duplicated(labels_category_data$variable)])
      stop(paste("Lookup_Coloration.csv has"
                 ,sum(duplicated(labels_category_data$variable))
                 ,"duplicate value(s) for category="
                 ,column_key$coloration.key[1], ". See above for a list of missing labels")
      )
    }
    c<-as.character(column_key$column[v])
    #Check for any values in the current field that are not assigned a color.
    NA.labels<-subset(data,!(data.frame(data)[,c] %in% labels_category_data$variable))
    
    
    
    if (nrow(NA.labels)>0){
      #Unlabeled is highly standardized, adding automatically
      if(length(unique(NA.labels[,c]))==1 &
         !is.na(unique(NA.labels[1,c]))&
         unique(NA.labels[1,c])=="Unlabeled")
      {
        
        labels_category_data<-rbind(labels_category_data,
                                    data.frame(coloration.key=column_key$coloration.key[v],
                                               variable="Unlabeled",
                                               Label="Unlabeled",
                                               Display.Order= 999,
                                               Color="reddish gray",
                                               RGB= "#967878",
                                               shape=NA,
                                               size=NA,
                                               alpha=NA
                                    ))
      }
      else{
        #Otherwise create an error.
        print(unique(NA.labels[,c]))
        stop(paste("Lookup_Coloration.csv is missing"
                   ,length(unique(NA.labels[,c]))
                   ,"label(s) for category="
                   ,c, ". See above for a list of missing labels")
        )
      }
    }
    
    labels_category_data<-subset(labels_category_data
                                 , variable %in% unique(data[,c]))
    
    
    #Order the names.data and then pass on the same order to the actual data in data
    labels_category_data<-labels_category_data[order(labels_category_data$Display.Order),]
    labels_category_data$column<-c
    names.data<-rbind(names.data,labels_category_data)
  }
  names.data
}
