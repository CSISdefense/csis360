#************************************Remove NAs
replace_nas_with_unlabeled<- function(VAR.df,VAR.column){
  VAR.df<-as.data.frame(VAR.df)
  if(any(is.na(VAR.df[VAR.column,]))){
    #Make sure unlabeled is within the list of levels
    if (!("Unlabeled" %in% levels(VAR.df[,VAR.column]))){
      VAR.df[,VAR.column]<-addNA(VAR.df[,VAR.column],ifany=TRUE)
      levels(VAR.df[,VAR.column])[is.na(levels(VAR.df[,VAR.column]))] <- "Unlabeled"
    }
  }
  VAR.df
}




NA.check<-function(VAR.df
  , VAR.input
  , VAR.output
  , VAR.file
){
  #Limit just to relevant columns
  NA.check.df<-subset(VAR.df
    , select=c(VAR.input,VAR.output)
  )
  #Drop all rows
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

  #Remove nonsense characters sometimes added to start of files
  colnames(VAR.existing.df)[substring(colnames(VAR.existing.df),1,3)=="?.."]<-
    substring(colnames(VAR.existing.df)[substring(colnames(VAR.existing.df),1,3)=="?.."],4)

  #Remove nonsense characters sometimes added to start of files
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

