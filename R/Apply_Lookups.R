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


#' When passed a file that doesn't exist, check for the zip version
#'
#' @param filename The name of the data file
#' @param path="" The location of the data file#'
#' @param directory="Data\\" The directory within the path that holds the lookup
#'
#' @return The filename name, if the file exists. Otherwise the zip file that contains the file.
#'
#' @details This function is meant for large data files. Our default approach is to zip them
#' up, one zip per file, and name the zip file the same name as the data file, except with zip
#' as an extension instead of .txt or .csv. This checks if the base file is available and if
#' not it handles opening the zip file instead.
#'
#' examples swap_in_zip(filename="Defense_Contract_SP_ContractSampleCriteriaDetailsCustomer.csv)
#'
swap_in_zip<-function(filename,path,directory=""){
  input<-paste(path,directory,filename,sep="")
  #File.exist seems only to work for local files.
  if(!file.exists(input) & !tolower(substr(input,1,4)) %in% c("http","ftp:") ){
    zip_file<-paste(substring(input,1,nchar(input)-3),"zip",sep="")
    if (!file.exists(zip_file)){
      stop(paste(input,"does not exist"))
    }
    input<-unz(description=zip_file,filename=filename)
    file_size<-file.info(zip_file)$size
    if (file_size>200000000){
      stop(paste("Zip file size (",file_size,") exceeds 200 megabytes and unz can't handle this. Current solution is to unzip in file system and read in directly."))
    }
  }
  input
}

#' Return the appropriate delimeter for the file type
#'
#' @param filename The name of the data file
#'
#' @return The delimter to use.
#'
#' @details Returns ',' for csv and '/t' for txt files. Creates an error for other file types.
#'
#' @examples get_delim("test.csv")
get_delim<-function(filename){
  delim<-NULL
  extension<-substring(filename,nchar(filename)-2,nchar(filename))
  if(extension=="csv") delim<-","
  else if(extension=="txt") delim<-"\t"
  else stop(paste("Unknown file type",extension))
  delim
}


#' Replace NAs in one variable of a data frame with a specified valued
#'
#' @param data A data frame
#' @param var The variable to have NAs removed
#' @param replacement Charater string that will replace NAs, by default "Unlabeled"
#'
#' @return Returns a data frame with the var values replaced with replacement,
#' otherwise identical to the original data frame
#'
#' @details This function is intended for preparing columns for use.
#' As far as the function creator understands it, NAs do not match to NAs. However,
#' we often wish to include NAs in a graph under a proper name, such as Unlabeled
#' and with a color of our choosing. One critical step in this process is the addition
#' of the replacement term to the levels of the var factor. Which means we
#' should probably add input protection when non factors are passed. I also haven't
#' tested the handling if multiple columns are passed, I think it may not work.
#'
#' @examples VAR.long.DF<-replace_nas_with_unlabeled(data=data.DF
#'   ,var="SubCustomer"),
#'   replacement = "Uncategorized"
#'   )
#'
#' @export
replace_nas_with_unlabeled<- function(data,
                                      var,
                                      replacement="Unlabeled"){
  data<-as.data.frame(data)

  if(!is.factor(data[,var]))
    data[,var]<-factor(data[,var])


  if(any(is.na(data[,var]))){
    #Make sure the replacement value is in the is within the list of levels
    # if (!(replacement %in% levels(data[,var]))){
    data[,var]<-addNA(data[,var],ifany=TRUE)
    # }
    levels(data[,var])[is.na(levels(data[,var]))] <- replacement
  }
  data
}



#' An internal function to check for NAs in columns after a join
#'
#' @param data A data frame
#' @param input_var The var(s) that had been used to join
#' @param output_var The var(s) that result from the join
#' @param lookup_file The file used in the join
#'
#' @return None. Instead the function raises an error if there are NAs
#'
#' @details This function is intended to catch gaps in lookup tables
#' and to alert the developer before they can come into use. The core intent
#' is to throw an error message that, if needed, will guide the developer to
#' the file they need to update and the rows they need to add.
#'
#' @examples na_check(data,
#'   input_var=by,
#'   output_var=add_var,
#'   lookup_file=lookup_file)
#'
#' @export
na_check<-function(data
                   , input_var
                   , output_var
                   , lookup_file
){


  #Limit just to relevant columns
  na_check.df<-subset(data
                      , select=c(input_var,output_var)
  )

  #Drop cases where there is an na in the input_var
  na_check.df<-na_check.df[complete.cases(na_check.df[,input_var]),]

  #Drop all complete rows
  na_check.df<-na_check.df[!complete.cases(na_check.df),]

  if(nrow(na_check.df)>0){
    print(unique(na_check.df))
    stop(paste(nrow(na_check.df)
               ,"rows of NAs generated in "
               ,paste(output_var,collapse=", ")
               ,"from "
               ,lookup_file)
    )
  }
}



#' Remove byte order marks, these appear before the first column name:
#'
#' @param data A /code(data.frame)
#'
#' @return /code(data) with the first column stripped of a byte order mark, if present
#'
#' @details Byte order marks are included at the start of UTF
#' text files and if not properly processed on read in, can add
#' nonsense characters to the start of the first column of a the
#' /code(data.frame) read in from a text file.
#'
#' @examples remove_bom(data)
#'
#' @export
remove_bom<-function(data
){
  #Remove nonsense characters sometimes added to start of files
  if(substring(colnames(data)[1],2,3)==".."&
     charToRaw(as.character(substring(colnames(data)[1],1,1)))=='ef')
    colnames(data)[1]<-substring(colnames(data)[1],4)

  colnames(data)[substring(colnames(data),2,3)=="?.."]<-
    substring(colnames(data)[substring(colnames(data),1,3)=="?.."],4)

  data
}


#' Read in an external file and join it with an existing data frame
#'
#' @param data The data frame to be joined
#' @param lookup_file The name of the lookup file
#' @param path="https://github.com/CSISdefense/R-scripts-and-data/tree/master/",
#' The location of the lookup file
#' @param directory="Lookups\\" The directory within the path that holds the lookup
#' @param by=NULL The columns used to join, if not provided, matching columns will be used
#' @param replace_na_var=NULL Before the join, these columns will have NAs values replaced
#' @param overlap_var_replaced=TRUE Should the function replace for common columns not used to join?
#' @param add_var=NULL, What new columns should be checked for NA values?
#' @param new_var_checked=FALSE Should only checked new columns be kept?
#' @param skip_check_var=NULL List of vars that should not be checked for NA values
#'
#' @return The data frame plus new columns from the lookup file. If new_var_checked is
#' true and only new columns listed in add_var will be kept. Note to self, should
#' add input protection that throws an error if new_var_checked is set to true when
#' add_var is false.
#'
#' @details This function is an elaborate join with various quality check measures thrown in.
#' At its simplest, it just joins the existing data frame with the passed file. But along the way
#' it will make some fixes to common CSV errors and also take advantage of some known facts about
#' how CSIS data is organized.
#'
#' @examples na_check(data,
#'   input_var=by,
#'   output_var=add_var,
#'   lookup_file=lookup_file)
#'
#' @import utils
#' @import stringr
#' @export
read_and_join<-function(
  data,
  lookup_file,
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  directory="Lookups/",
  by=NULL,
  replace_na_var=NULL,
  overlap_var_replaced=TRUE,
  add_var=NULL,
  new_var_checked=TRUE,
  skip_check_var=NULL){


  if(is.data.frame(lookup_file))
    stop("lookup_file parameter is a data frame, it should be a filename, e.g. 'lookup_customer.csv'.")

  #Replace NAs in input column if requested
  if(!is.null(replace_na_var)){
    data<-replace_nas_with_unlabeled(data,
                                     replace_na_var)
  }

  #This doesn't  work for URLs. Worth trying again later with some parsing
  # if (!file.exists(paste(path,directory,lookup_file,sep=""))){
  # stop(paste(path,directory,lookup_file," does not exist.",sep=""))
  # }

  #Read in the lookup file
  lookup<-read.csv(
    paste(path,directory,lookup_file,sep=""),
    header=TRUE,
    sep=ifelse(substring(lookup_file,nchar(lookup_file)-3)==".csv",",","\t"),
    na.strings=c("NA","NULL"),
    quote = "\"",#Necessary because there are some 's in the names.
    dec=".",
    strip.white=TRUE,
    stringsAsFactors=FALSE  #This can get weird when true, as sometimes it confuses numerical variables and factors
  )


  #Remove byte order marks present in UTF encoded files
  data<-remove_bom(data)
  lookup<-remove_bom(lookup)


  #Raise an error if by is missing from either file
  if(any(!by %in% colnames(lookup))){
    by<-by[!by %in% colnames(lookup)]
    stop(paste(paste(by,collapse=" & "), "not present in lookup"))
  }
  if(any(!by %in% colnames(data))){
    by<-by[!by %in% colnames(data)]
    stop(paste(paste(by,collapse=" & "),"not present in data"))
  }
  if(any(duplicated(lookup[,by]))){
    print(unique(lookup[duplicated(lookup[,by]),by]))
    stop(paste("Duplicate entries in lookup for by variables: ",by))
  }
  #Handle any fields in both data and lookup held in common not used in the joining
  if(!is.null(by)){
    droplist<-names(lookup)[names(lookup) %in% names(data)]
    droplist<-droplist[!droplist %in% by]
    if(length(droplist)>0){
      if(overlap_var_replaced)
        data<-data[,!names(data) %in% droplist]
      else
        lookup<-lookup[,!names(lookup) %in% droplist]
    }
  }

  #Fixes for Excel's penchant to drop leading 0s.
  if("Contracting.Agency.ID" %in% names(lookup) & "data" %in% names(lookup)){
    lookup$Contracting.Agency.ID<-factor(str_pad(lookup$Contracting.Agency.ID,4,side="left",pad="0"))
    data$Contracting.Agency.ID<-as.character(data$Contracting.Agency.ID)
    data$Contracting.Agency.ID[is.na(data$Contracting.Agency.ID=="")]<-"0000"
    data$Contracting.Agency.ID<-factor(str_pad(data$Contracting.Agency.ID,4,side="left",pad="0"))
  }

  #Make sure CSIScontractIDs are numeric and not a factor
  if("CSIScontractID" %in% colnames(lookup)){
    if(!is.numeric(lookup$CSIScontractID)){
      lookup$CSIScontractID<-as.numeric(as.character(lookup$CSIScontractID))
    }
  }

  #Conduct the join
  if(is.null(by)){
    data<- dplyr::left_join(
      data,
      lookup,
      match="first"
    )
  }
  else{
    data<- dplyr::left_join(
      data,
      lookup,
      match="first",
      by=by
    )
  }

  #If add_var is specified, dropped new fields not in add_var
  if(!is.null(add_var)){
    droplist<-names(lookup)[!names(lookup) %in% by
                            &!names(lookup) %in% add_var]
    data<-data[,!names(data) %in% droplist]
  }
  #If add_var is not specified, set it equal to all new vars
  else{
    add_var<-colnames(lookup)[!colnames(lookup) %in% by]
  }

  if(!is.null(by)&new_var_checked==TRUE){
    if(!is.null(skip_check_var)){
      add_var<-add_var[!add_var %in% skip_check_var]
    }

    na_check(data,
             input_var=by,
             output_var=add_var,
             lookup_file = lookup_file)
  }

  data
}



#' Read in an external file and join it with an existing data frame
#'
#' @param data The data frame to be joined
#' @param lookup_file The name of the lookup file
#' @param path The location of the lookup file
#' @param directory The directory within the path that holds the lookup
#' @param by=NULL The columns used to join, if not provided, matching columns will be used
#' @param replace_na_var=NULL Before the join, these columns will have NAs values replaced
#' @param overlap_var_replaced=TRUE Should the function replace for common columns not used to join?
#' @param add_var=NULL, What new columns should be checked for NA values?
#' @param new_var_checked=FALSE Should only checked new columns be kept?
#' @param skip_check_var=NULL List of vars that should not be checked for NA values
#' @param zip_file=NULL
#'
#' @return The data frame plus new columns from the lookup file. If new_var_checked is
#' true and only new columns listed in add_var will be kept. Note to self, should
#' add input protection that throws an error if new_var_checked is set to true when
#' add_var is false.
#'
#' @details This function is an elaborate join with various quality check measures thrown in.
#' At its simplest, it just joins the existing data frame with the passed file. But along the way
#' it will make some fixes to common CSV errors and also take advantage of some known facts about
#' how CSIS data is organized.
#'
#' @examples na_check(data,
#'   input_var=by,
#'   output_var=add_var,
#'   lookup_file=lookup_file)
#'
#' @import utils
#' @export
read_and_join_experiment<-function(
  data,
  lookup_file,
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  directory="Lookups\\",
  by=NULL,
  replace_na_var=NULL,
  overlap_var_replaced=TRUE,
  add_var=NULL,
  new_var_checked=TRUE,
  skip_check_var=NULL,
  zip_file=NULL,
  col_types=NULL
){


  case_match<-function(name, list){
    if(!name %in% (list)){
      if(tolower(name) %in% tolower(list)){
        name<-list[tolower(list)==tolower(name)]
      }
    }
    name
  }


  #Replace NAs in input column if requested
  if(!is.null(replace_na_var)){
    data<-replace_nas_with_unlabeled(data,
                                     replace_na_var)
  }

  if(is.data.frame(lookup_file))
    stop("lookup_file parameter is a data frame, it should be a filename, e.g. 'lookup_customer.csv'.")


  if(!is.null(zip_file)){#No zip file
    #Case sensitivity fix for zip filename
    # dir_list<-list.files(paste(path,directory,sep=""))
    # zip_file<-case_match(zip_file,dir_list)

    #Read in the lookup file
    if (!file.exists(paste(path,directory,zip_file,sep=""))){
      stop(paste(path,directory,zip_file," does not exist",sep=""))
    }
    file_size<-file.info(paste(path,directory,zip_file,sep=""))$size
    if (file_size>200000000){
      stop(paste("Zip file size (",file_size,") exceeds 200 megabytes and unz can't handle this. Current solution is to unzip in file system and read in directly."))
    }

    #Case sensitivity fix for data filename
    file_list<-unzip(paste(path,directory,zip_file,sep=""),list=TRUE)
    lookup_file<-case_match(lookup_file,file_list$Name)
    if(!lookup_file %in% (file_list$Name)){
      print(file_list)
      stop(paste(lookup_file,"not present in",zip_file))
    }
    input<-paste(path,directory,zip_file,sep="")#unz(description=paste(path,directory,zip_file,sep=""),filename=lookup_file)

  }
  else{
    input<-swap_in_zip(lookup_file,path,directory)
  }
  lookup<-readr::read_delim(
    input,
    col_names=TRUE,
    delim=ifelse(substring(lookup_file,nchar(lookup_file)-3)==".csv",",","\t"),
    na=c("NA","NULL"),
    trim_ws=TRUE,
    col_types=col_types
  )

  #Remove byte order marks present in UTF encoded files
  data<-remove_bom(data)
  lookup<-remove_bom(lookup)


  #Raise an error if by is missing from either file
  if(any(!by %in% colnames(lookup))){
    by<-by[!by %in% colnames(lookup)]
    stop(paste(paste(by,collapse=" & "), "not present in lookup"))
  }
  if(any(!by %in% colnames(data))){
    by<-by[!by %in% colnames(data)]
    stop(paste(paste(by,collapse=" & "),"not present in data"))
  }


  #Handle any fields in both data and lookup held in common not used in the joining
  if(!is.null(by)){
    droplist<-names(lookup)[names(lookup) %in% names(data)]
    droplist<-droplist[!droplist %in% by]
    if(length(droplist)>0){
      if(overlap_var_replaced)
        data<-data[,!names(data) %in% droplist]
      else
        lookup<-lookup[,!names(lookup) %in% droplist]
    }
  }

  #Fixes for Excel's penchant to drop leading 0s.
  if("Contracting.Agency.ID" %in% names(lookup) & "data" %in% names(lookup)){
    lookup$Contracting.Agency.ID<-factor(str_pad(lookup$Contracting.Agency.ID,4,side="left",pad="0"))
    data$Contracting.Agency.ID<-as.character(data$Contracting.Agency.ID)
    data$Contracting.Agency.ID[is.na(data$Contracting.Agency.ID=="")]<-"0000"
    data$Contracting.Agency.ID<-factor(str_pad(data$Contracting.Agency.ID,4,side="left",pad="0"))
  }

  #Make sure CSIScontractIDs are numeric and not a factor
  if("CSIScontractID" %in% colnames(lookup)){
    if(!is.numeric(lookup$CSIScontractID)){
      lookup$CSIScontractID<-as.numeric(as.character(lookup$CSIScontractID))
    }
  }

  #Conduct the join
  if(is.null(by)){
    data<- plyr::join(
      data,
      lookup,
      match="first"
    )
  }
  else{
    data<- plyr::join(
      data,
      lookup,
      match="first",
      by=by
    )
  }
  if(any(duplicated(lookup[,by]))){
    print(unique(lookup[duplicated(lookup[,by]),by]))
    stop(paste("Duplicate entries in lookup for by variables: ",by))
  }
  #If add_var is specified, dropped new fields not in add_var
  if(!is.null(add_var)){
    droplist<-names(lookup)[!names(lookup) %in% by
                            &!names(lookup) %in% add_var]
    data<-data[,!names(data) %in% droplist]
  }
  #If add_var is not specified, set it equal to all new vars
  else{
    add_var<-colnames(lookup)[!colnames(lookup) %in% by]
  }

  if(!is.null(by)&new_var_checked==TRUE){
    if(!is.null(skip_check_var)){
      add_var<-add_var[!add_var %in% skip_check_var]
    }

    na_check(data,
             input_var=by,
             output_var=add_var,
             lookup_file = lookup_file)
  }

  data
}


#' Deflation using GitHub-based CSV file
#'
#' @param data A data frame
#' @param money_var The quoted name of the dollar-value variable
#' @param fy_var The quoted name of the fiscal year variable
#' @param deflator_file The quoted file name of the deflators to use;
#' must be a CSV with the column "fiscal.year."
#' @param deflator_var The quoted name of the defalator variable variable,
#' by default "deflator.2017"
#' @param path The path or url for the deflator_file CSV.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/csis360/Lookup-Tables/data/economic/
#'
#' @return Returns a data frame with the money_var deflated, using deflator_var
#' otherwise identical to the original data frame
#'
#' @details Warning: This function should be used __in data processing only__,
#' not in a live app.  It reads an external file from GitHub,
#' which will slow down an app substantially if done repeatedly.
#'
#' @examples Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
#'
#' FullData <- read_csv("2017_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
#'   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))
#' PrepareLabelsAndColors(Coloration,FullData,"Customer")
#'
#' @export
deflate <- function(
  data,
  money_var = "Amount",
  fy_var = "Fiscal.Year",
  deflator_file = "Lookup_Deflators.csv",
  deflator_var="Deflator.2017",
  path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
  directory="economic/",
  deflator_dropped=TRUE
){
  #Tiblbes run into trouble with the [[]] new variable specifying.
  data<-as.data.frame(data)

  if(!fy_var %in% colnames(data))
    stop(paste(fy_var," is not present in data."))

  if(!money_var %in% colnames(data)){
    if((paste(money_var,"Then.Year",sep=".") %in% colnames(data)) &
       (paste(money_var,deflator_var,sep=".") %in% colnames(data))){
      warning(paste(money_var," is not present in data, due to prior run of deflate with money_var=",money_var,".",sep=""))
      return(data)
    }
    else{
      stop(paste(money_var,"is not present in data."))
    }
  }


  data[[money_var]] <- as.numeric(data[[money_var]])

  cat(paste("\n Applying\n", deflator_var, "\n in \n", deflator_file, "\n from\n", path, "\n"))
  deflators_retrieved <- readr::read_csv(paste0(path, directory,deflator_file))

  #Rename the Fiscal.Year variable to be match the name used in data
  colnames(deflators_retrieved)[colnames(deflators_retrieved)=="Fiscal.Year"]<-fy_var

  #Drop unneded deflator columns and then join the deflators to the data
  deflators_retrieved<-subset(deflators_retrieved,select=c(fy_var,deflator_var))
  data<-plyr::join(data,deflators_retrieved,by=fy_var)

  #Create current and constant dollar variants of money_var
  data[[paste(money_var,deflator_var,sep=".")]] <- as.numeric(as.character(
    data[[money_var]])) /
    data[[deflator_var]]

  colnames(data)[colnames(data)==money_var]<-paste(money_var,"Then.Year",sep=".")

  #Drop the deflator var unless deflator_dropped = FALSE
  if(deflator_dropped)
    data<-data[,colnames(data)!=deflator_var]

  #Standardize the newly created names
  data<-standardize_variable_names(data,
                                   var=c(paste(money_var,"Then.Year",sep="."),
                                         paste(money_var,deflator_var,sep=".")))

  return(data)
}




#' Get Column Key based on the names in a data frame
#'
#' @param data A data frame
#' @param path The path or url for the column key.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/csis360/master/style/
#'
#' @return A data frame of the column names from data joined up to the column key
#'
#' @details Warning: This function should be used in data processing only,
#' not in a live app.  It reads an external file from GitHub,
#' which will slow down an app substantially if done repeatedly. Works best
#' when standardize_names has already been run on the data frame in question.
#'
#' @examples
#'
#' FullData <- read_csv("2017_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
#'   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))
#' PrepareLabelsAndColors(Coloration,FullData,"Customer")
#'
#' @export
get_column_key <- function(
  data,
  path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/"
){
  column_key<-colnames(data)
  column_key<-as.data.frame(column_key)
  colnames(column_key)[1]<-"column"

  #Join up the files
  column_key<-read_and_join(column_key,
                            "Lookup_Column_Key.csv",
                            path=path,
                            directory="",
                            by="column",
                            new_var_checked=FALSE
  )

  #Set empty string coloration.keys equal to na
  column_key$coloration.key[column_key$coloration.key==""]<-NA
  return(column_key)
}


text_to_number<-function(x){
  if ((is.factor(x))||(is.character(x))){
    x<-gsub('\\$','',as.character( x))
    x<-as.double(gsub('\\,','',as.character( x)))
  }
  x
}
