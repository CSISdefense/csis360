


#***********************Standardize Variable Names
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


PrepareLabelsAndColors<-function(VAR.Coloration
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


  #Translate the category name into the appropriate coloration.key
  #This is used because we have more category names than coloration.key
  Coloration.Key<-read.csv(
    paste(Path,"Lookups\\","lookup_coloration_key.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )
  Coloration.Key<-subset(Coloration.Key, category==VAR.y.series)

  if(nrow(Coloration.Key)==0){
    stop(paste(VAR.y.series,"is missing from Lookup_Coloration.Key.csv"))
  }


  #Limit the lookup table to those series that match the variable
  labels.category.DF<-subset(VAR.Coloration, coloration.key==Coloration.Key$coloration.key[1] )

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
