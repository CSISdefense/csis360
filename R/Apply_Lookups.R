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
#' @param path The location of the data file. Default ""
#' @param directory The directory within the path that holds the lookup, default "Data\\
#'
#' @return The filename name, if the file exists. Otherwise the zip file that contains the file.
#'
#' @details This function is meant for large data files. Our default approach is to zip them
#' up, one zip per file, and name the zip file the same name as the data file, except with zip
#' as an extension instead of .txt or .csv. This checks if the base file is available and if
#' not it handles opening the zip file instead.
#'
#' @examples swap_in_zip(filename="Defense_Contract_SP_ContractSampleCriteriaDetailsCustomer.csv")
#'
#' @export
swap_in_zip<-function(filename,path,directory=""){
  input<-file.path(path,directory,filename)
  if (path=="") input<-file.path(path,directory,filename)
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

#' A kludge, tests a variety of local paths to find the lookup-tables reposistory
#'
#' @return The path, if a known one exists. Otherwise it will throw an error.
#'
#' @details The lookup-table repository is a public one and can be found online.
#' However, when updating lookup files, it can be faster to use the local version
#' rather than wait for github to full parse the pushed files. In addition a
#' local path can enable working offline.
#'
#' @examples get_local_lookup_path()
#'
#' @export
get_local_lookup_path<-function(){
  local_path<-"C:\\Users\\Present\\Documents\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"C:\\Users\\grego\\Repos\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"K:\\Users\\Greg\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"C:\\Users\\gsand\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"D:\\Users\\Greg\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"F:\\Users\\Greg\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"D:\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"F:\\Users\\gsanders\\Documents\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"C:\\Users\\grego\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"F:\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"E:\\Repository\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"F:\\REPOs\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"C:\\Users\\HCarroll\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"F:\\Users\\gsanders\\Repositories\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"C:\\Users\\WRumbaugh\\OneDrive - Center Strategic Intl Studies Inc CSIS\\Documents\\Lookup-Tables"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"\\Users\\henrycarroll\\Desktop\\CSIS Work\\"
  if(file.exists(local_path))
    return(local_path)
  local_path<-"C:\\Users\\GSanders\\Repos\\Lookup-Tables\\"
  if(file.exists(local_path))
    return(local_path)
  local_path <- "C:/Users/namye/Repo/Lookup-Tables"
  if(file.exists(local_path))
    return(local_path)
  local_path <- "C:\Users\CBarrie\Repos\Lookup-Tables"
  if(file.exists(local_path))
    return(local_path)
  stop("Could not find local path. Update the list in Apply_Lookups.R")
}



#' A kludge, that should someday be replaced by using Microsoft365 repo.
#'
#'' @param site Which site. The DIIG sharepoint folder is the default.
#'
#' @return The path, if a known one exists. Otherwise it will throw an error.
#'
#' @details This is strictly for internal CSIS usage, as the user will need to
#' separately be syncing sharepoint to their drive. The intent is to update,
#' particularly via log_path,
#'
#' @examples get_local_sharepoint_path()
#'
#' @export
get_local_sharepoint_path<-function(site="DIIG - Documents"){
  local_path<-file.path("C:\\Users\\Present\\Documents\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("K:\\Users\\Greg\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\gsand\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("D:\\Users\\Greg\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("F:\\Users\\Greg\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    local_path<-file.path("F:\\Users\\Greg\\Center Strategic Intl Studies Inc CSIS",site)
  local_path<-file.path("F:\\Users\\gsanders\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    local_path<-file.path("F:\\Users\\gsanders\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("D:\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("F:\\Users\\gsanders\\Documents\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\grego\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\HCarroll\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\HCarroll\\OneDrive - Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\AAldisert\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\HHolopainen\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\WRumbaugh\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\GSanders\\Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\GSanders\\OneDrive - Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\GSanders\\OneDrive - Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\HCarroll\\OneDrive - Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  local_path<-file.path("C:\\Users\\CBarrie\\OneDrive - Center Strategic Intl Studies Inc CSIS",site)
  if(file.exists(local_path))
    return(local_path)
  stop("Could not find local path. Update the list in Apply_Lookups.R")
}

#' Turn a factor  into a number
#'
#' @param x The factor or character string
#'
#' @return The same column in numerical format.
#'
#' @details Returns the variable after removing $s and ,s.
#'
#' @examples FactorToNumber("5")
#'
#' @export
FactorToNumber<-function(x){
  warning("Deprecated in favor of text_to_number")
  if ((is.factor(x))||(is.character(x))){
    x<-gsub('\\$','',as.character( x))
    x<-as.double(gsub('\\,','',as.character( x)))
  }
  x
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
#'
#' @export
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
#' VAR.long.DF<-replace_nas_with_unlabeled(data=data.DF
#'   ,var="SubCustomer")
#'   replacement = "Uncategorized"
#'   )
#'
#' @export
replace_nas_with_unlabeled<- function(data,
                                      var,
                                      replacement="Unlabeled"){
  data<-as.data.frame(data)
  if(is.factor(var)) var<-as.character(var)

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
#' @param missing_file Filename to output any unmatched variables for easy of processing
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
                   , missing_file=NULL
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
    if(!is.null(missing_file))
      write.csv(file=missing_file,unique(na_check.df))
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
#' @param by The columns used to join, if not provided, matching columns will be used
#' @param replace_na_var Before the join, these columns will have NAs values replaced
#' @param overlap_var_replaced Should the function replace for common columns not used to join?
#' @param add_var, What new columns should be checked for NA values?
#' @param new_var_checked Should only checked new columns be kept?
#' @param skip_check_var List of vars that should not be checked for NA values
#' @param missing_file Filename to output any unmatched variables for easy of processing
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
    skip_check_var=NULL,
    missing_file=NULL,
    lookup_char_as_factor=FALSE){


  if(is.data.frame(lookup_file))
    stop("lookup_file parameter is a data frame, it should be a filename, e.g. 'lookup_customer.csv'.")

  if(is.null(nrow(data))) stop("Data parameter passed to read_and_join has no rows.")

  if(any(!by %in% colnames(data))){
    by<-by[!by %in% colnames(data)]
    stop(paste(paste(by,collapse=" & "),"not present in data"))
  }

  #Replace NAs in input column if requested
  if(!is.null(replace_na_var)){
    data<-replace_nas_with_unlabeled(data,
                                     replace_na_var)
  }
  pathdir=file.path(path,directory)
  if(path=="") pathdir<-directory
  #This doesn't  work for URLs. Worth trying again later with some parsing
  # if (!file.exists(file.path(pathdir,lookup_file))){
  # stop(paste(file.path(pathdir,lookup_file)," does not exist.",sep=""))
  # }

  #Read in the lookup file
  lookup<-read.csv(
    file.path(pathdir,lookup_file),
    header=TRUE,
    sep=if_else(substring(lookup_file,nchar(lookup_file)-3)==".csv",",","\t"),
    na.strings=c("NA","NULL"),
    quote = "\"",#Necessary because there are some 's in the names.
    dec=".",
    strip.white=TRUE,
    stringsAsFactors=lookup_char_as_factor  #This can get weird when true, as sometimes it confuses numerical variables and factors
  )


  #Remove byte order marks present in UTF encoded files
  data<-remove_bom(data)
  lookup<-remove_bom(lookup)


  #Raise an error if newly added variables are  absent from the lookup
  if(any(!add_var %in% colnames(lookup))){
    print(colnames(lookup))
    add_var<-add_var[!add_var %in% colnames(lookup)]
    stop(paste(paste(add_var,collapse=" & "), "not present in lookup"))
  }

  #Raise an error if by is missing from either file
  if(any(!by %in% colnames(lookup))){
    by<-by[!by %in% colnames(lookup)]
    stop(paste(paste(by,collapse=" & "), "not present in lookup"))
  }

  if(any(duplicated(lookup[,by]))){
    print(unique(lookup[duplicated(lookup[,by]),by]))
    stop(paste("Duplicate entries in lookup for by variables: ",by))
  }

  #Handle any fields in both data and lookup held in common not used in the joining
  if(!is.null(by)){
    #If add_var is specified, dropped new fields not in add_var
    if(!is.null(add_var)){
      droplist<-names(lookup)[!names(lookup) %in% by
                              &!names(lookup) %in% add_var]
      data<-data[,!names(data) %in% droplist]
    }

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
      lookup
    )
  }
  else{
    data<- dplyr::left_join(
      data,
      lookup,
      by=by
    )
  }




  if(!is.null(by)&new_var_checked==TRUE){
    #If add_var is not specified, set it equal to all new vars
    if(is.null(add_var))
      add_var<-colnames(lookup)[!colnames(lookup) %in% by]

    if(!is.null(skip_check_var)){
      add_var<-add_var[!add_var %in% skip_check_var]
    }
    #First verify  there are any variables to check
    if(length(add_var)>0){
      na_check(data,
               input_var=by,
               output_var=add_var,
               lookup_file = lookup_file,
               missing_file= missing_file)
    }
  }

  data
}



#' Read in an external file and join it with an existing data frame
#'
#' @param data The data frame to be joined
#' @param lookup_file The name of the lookup file
#' @param path The location of the lookup file
#' @param directory The directory within the path that holds the lookup
#' @param by The columns used to join, if not provided, matching columns will be used
#' @param replace_na_var Before the join, these columns will have NAs values replaced
#' @param overlap_var_replaced Should the function replace for common columns not used to join?
#' @param add_var What new columns should be checked for NA values?
#' @param new_var_checked Should only checked new columns be kept?
#' @param skip_check_var List of vars that should not be checked for NA values
#' @param zip_file The source zip file.
#' @param missing_file Filename to output any unmatched variables for easy of processing
#' @param create_lookup_rdata Whether to create a rdata file using the lookup for ease of future inputing
#' @param lookup_char_as_factor Whether to treat character strings as a factor
#' @param guess_max Default null, but if not null the number of rows to consider when guessing column types.
#' @param guess_max How to join the data file and the lookup_file. Default is left, keeping all rows from the data and matches from the lookup
#' @param prefix What characters to include before any added variable, default none. Helpful when joining a single lookup table with multiple variables, e.g. country codes.
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
    path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
    directory="Lookups/",
    by=NULL,
    replace_na_var=NULL,
    overlap_var_replaced=TRUE,
    add_var=NULL,
    new_var_checked=TRUE,
    skip_check_var=NULL,
    zip_file=NULL,
    col_types=NULL,
    case_sensitive=TRUE,
    missing_file=NULL,
    create_lookup_rdata=FALSE,
    lookup_char_as_factor=FALSE,
    guess_max=NULL,
    join_type="left",
    prefix=""
){
  if(!is.null(names(by)))
    left_by<-names(by)
  else left_by<-by

  # read.delim doesn't like \\
  path<-gsub("\\\\","//",path)
  directory<-gsub("\\\\","//",directory)

  if(tolower(substr(path,1,4))=="http"&!RCurl::url.exists(file.path(path,directory,lookup_file))
     || path=="offline"){
    warning("Using offline path")
    path<-get_local_lookup_path()
  }

  #Prevent "" from sending the path back to the \ directory rather than the root for the project.
  pathdir=file.path(path,directory)
  if(path=="") pathdir<-directory


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
    if(any(!replace_na_var %in% colnames(data))) stop("replace_na_var is missing.")
    data<-replace_nas_with_unlabeled(data,
                                     replace_na_var)
  }

  if(is.data.frame(lookup_file))
    stop("lookup_file parameter is a data frame, it should be a filename, e.g. 'lookup_customer.csv'.")

  #If the  file specified is an RDA
  if(tolower(substring(lookup_file,nchar(lookup_file)-3))==".rda"){
    if (!file.exists(file.path(pathdir,lookup_file)))
      stop(paste0(file.path(pathdir,lookup_file)," does not exist"))
    load(file.path(pathdir,lookup_file))
  }
  #If there exists an rda variant of the file passed.
  else if (file.exists(file.path(pathdir,paste0(substring(lookup_file,1,nchar(lookup_file)-3),"rda"))))
    load(file.path(pathdir,paste0(substring(lookup_file,1,nchar(lookup_file)-3),"rda")))

  else{ if(!is.null(zip_file)){
    #Case sensitivity fix for zip filename
    # dir_list<-list.files(file.path(pathdir))
    # zip_file<-case_match(zip_file,dir_list)

    #Read in the lookup file
    if (!file.exists(file.path(pathdir,zip_file))){
      stop(paste(file.path(pathdir,zip_file),"does not exist"))
    }
    file_size<-file.info(file.path(pathdir,zip_file))$size
    if (file_size>200000000){
      stop(paste("Zip file size (",file_size,") exceeds 200 megabytes and unz can't handle this. Current solution is to unzip in file system and read in directly."))
    }

    #Case sensitivity fix for data filename
    file_list<-unzip(file.path(pathdir,zip_file),list=TRUE)
    lookup_file<-case_match(lookup_file,file_list$Name)
    if(!lookup_file %in% (file_list$Name)){
      print(file_list)
      stop(paste(lookup_file,"not present in",zip_file))
    }
    input<-file.path(pathdir,zip_file)#unz(description=paste(pathdir,zip_file,sep=""),filename=lookup_file)

  }
    else{#No zip file
      input<-swap_in_zip(lookup_file,pathdir)
    }
    na_list<-c("NA","NULL")
    #Known case where na=c("NA","NULL") will cause errors.
    if(lookup_file=="Location_CountryCodes.csv"){
      na_list<-"NULL"
    }

    if(is.null(guess_max)){
      lookup<-readr::read_delim(
        input,
        col_names=TRUE,
        delim=if_else(substring(lookup_file,nchar(lookup_file)-3)==".csv",",","\t"),
        na=na_list,
        trim_ws=TRUE,
        col_types=col_types
      )
    } else{
      lookup<-readr::read_delim(
        input,
        col_names=TRUE,
        delim=if_else(substring(lookup_file,nchar(lookup_file)-3)==".csv",",","\t"),
        na=na_list,
        trim_ws=TRUE,
        col_types=col_types,
        guess_max =guess_max
      )

    }

    if(prefix!=""){
      if (is.null(by))
        names(lookup)<-paste0(prefix,names(lookup))
      else
        names(lookup)[!names(lookup) %in% by]<-
          paste0(prefix,names(lookup)[!names(lookup) %in% by])
      add_var<-paste0(prefix,add_var)
      skip_check_var<-paste0(prefix,skip_check_var)
    }


    #Convert character strings to factors
    if (lookup_char_as_factor==TRUE){
      #Found here: https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
      lookup<-lookup %>% mutate_if(is.character, factor)
    }

    if (create_lookup_rdata==TRUE)
      save(lookup,file=file.path(pathdir,
                                 paste0(substring(lookup_file,1,nchar(lookup_file)-3),"rda"))
      )
  }
  #Remove byte order marks present in UTF encoded files
  data<-remove_bom(data)
  lookup<-remove_bom(lookup)


  #Raise an error if by is missing from either file
  if(any(!by %in% colnames(lookup))){
    by<-by[!by %in% colnames(lookup)]
    stop(paste(paste(by,collapse=" & "), "not present in lookup"))
  }
  if(any(!left_by %in% colnames(data))){
    left_by<-left_by[!left_by %in% colnames(data)]
    stop(paste(paste(left_by,collapse=" & "),"not present in data"))
  }
  if(any(!add_var %in% colnames(lookup))){
    add_var<-add_var[!add_var %in% colnames(lookup)]
    stop(paste(paste(add_var,collapse=" & "),"not present in lookup"))
  }

  #Handle any fields in both data and lookup held in common not used in the joining
  #And if add_vars is specified, drop non-add_vars from lookup.
  if(!is.null(by)){
    if(!is.null(add_var))
      lookup<-lookup[,names(lookup) %in% c(by, add_var)]

    droplist<-names(lookup)[names(lookup) %in% names(data)]
    droplist<-droplist[!droplist %in% by]

    if(length(droplist)>0){
      if(overlap_var_replaced)
        data<-data[,!names(data) %in% droplist]
      else{
        if(!is.null(add_var))
          stop(paste("Not replacing overlap, but add_var present in data:",droplist))
        lookup<-lookup[,!names(lookup) %in% droplist]
      }
    }
    rm(droplist)
  }
  #If add_vars is specified but there is no by, drop non add_vars from lookup
  else if( !is.null(add_var)){
    bylist<-names(lookup)[names(lookup) %in% names(data)]
    lookup<-lookup[,names(lookup) %in% c(bylist, add_var)]
    rm(bylist)
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

  lookup<-as.data.frame(lookup)
  if(any(!complete.cases(lookup[,by]))){
    warning("NAs found in by variable. Filtering them out.")
    lookup<-lookup[complete.cases(lookup[,by]),]
  }

  if(anyDuplicated(lookup[,by])){
    print(unique(lookup[duplicated(lookup[,by]),by]))
    stop(paste("Duplicate entries in lookup for by variables: ",by))
  }

  #Conduct the join
  if(is.null(by)){
    if(case_sensitive==FALSE) stop("Haven't implemented case insensitive yet w/o by parameter")

    #If add_var is not specified, set it equal to all lookup vars not present in data
    if(is.null(add_var))
      add_var<-colnames(lookup)[!colnames(lookup) %in% colnames(data)]
    left_by<-colnames(lookup)[colnames(lookup) %in% colnames(data)]

    if(join_type=="left"){
      data<- dplyr::left_join(
        data,
        lookup
      )
    } else if(join_type=="full"){
      data<- dplyr::full_join(
        data,
        lookup
      )
    }
    else stop("Unrecognized join_type")
  }
  else{
    if(case_sensitive==FALSE){
      #Create a temporary holder for original values of each of the by ariables
      #And then switch them to lower case before the  join is run.
      data<-as.data.frame(data)
      lookup<-as.data.frame(lookup)
      for(i in 1:length(left_by)){
        original_temp_name<-paste(left_by[i],"original",sep="_")
        if(original_temp_name %in% colnames(data)) stop(paste(original_temp_name,"already exists as a column in data, nowhere to store the original values."))
        data[,original_temp_name]<-data[,left_by[i]]
        data[,left_by[i]]<-tolower(data[,left_by[i]])
        # lookup[,original_temp_name]<-lookup[,left_by[i]]
        lookup[,by[i]]<-tolower(lookup[,by[i]])
      }
    }

    if(join_type=="left"){
      data<- dplyr::left_join(
        data,
        lookup,
        by=by
      )
    } else if(join_type=="full"){
      data<- dplyr::full_join(
        data,
        lookup,
        by=by
      )
    }
    else stop("Unrecognized join_type")


    if(case_sensitive==FALSE){
      #Switch back the by variables to their pre-tolower value
      #Lookup isn't kept, commented code was just switching it back for error checking purposes.
      #But creating a lookup column also imports it into data, a needless compllication.
      for(i in 1:length(left_by)){
        original_temp_name<-paste(left_by[i],"original",sep="_")
        data[,left_by[i]]<-data[,original_temp_name]
        data <- data[,!(colnames(data) %in% original_temp_name)]
        # lookup[,left_by[i]]<-lookup[,original_temp_name]
        # lookup <- lookup[,!(colnames(lookup) %in% original_temp_name)]
      }
    }
  }


  if(new_var_checked==TRUE){
    #If add_var is not specified, set it equal to all new vars
    if(is.null(add_var))
      add_var<-colnames(lookup)[!colnames(lookup) %in% by]

    if(!is.null(skip_check_var)){
      add_var<-add_var[!add_var %in% skip_check_var]
    }
    #First verify  there are any variables to check
    if(length(add_var)>0){
      na_check(data,
               input_var=left_by,
               output_var=add_var,
               lookup_file = lookup_file,
               missing_file= missing_file)
    }
  }

  return(data)
}


#' Deflation using GitHub-based CSV file
#'
#' @param data A data frame
#' @param money_var The quoted name of the dollar-value variable
#' @param fy_var The quoted name of the fiscal year variable
#' @param deflator_file The quoted file name of the deflators to use;
#' must be a CSV with the column "Fiscal_Year."
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
#' @examples
#'
#' FullData <- read_csv("2017_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv",
#'   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))
#' PrepareLabelsAndColors(Coloration,FullData,"Customer")
#'
#' @export
deflate <- function(
    data,
    money_var = "Amount",
    fy_var = "Fiscal_Year",
    deflator_file = "Lookup_Deflators.csv",
    deflator_var="OMB25_GDP23",
    path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
    directory="economic/",
    deflator_dropped=TRUE
){
  #Default deflator if none passed.
  if(is.null(deflator_var)) deflator_var<-"OMB25_GDP23"

  #Tibbles run into trouble with the [[]] new variable specifying.
  data<-as.data.frame(data)


  #If the money_var already has _Then_Year, remove it so it can be added again
  if(substr(money_var,nchar(money_var)-9,nchar(money_var))=="_Then_Year"){
    colnames(data)[colnames(data)==money_var]<-substr(money_var,1,nchar(money_var)-10)
    money_var<-substr(money_var,1,nchar(money_var)-10)
  }

  if(!fy_var %in% colnames(data)){
    if(fy_var == "Fiscal.Year" & "Fiscal_Year" %in% colnames(data)) fy_var<-"Fiscal_Year"
    if(fy_var == "fiscal_year" & "Fiscal_Year" %in% colnames(data)) fy_var<-"Fiscal_Year"
    else stop(paste(fy_var," is not present in data."))
  }

  if(!money_var %in% colnames(data)){
    if((paste(money_var,"Then_Year",sep="_") %in% colnames(data)) &
       (paste(money_var,deflator_var,sep="_") %in% colnames(data))){
      warning(paste(money_var," is not present in data, due to prior run of deflate with money_var=",money_var,".",sep=""))
      return(data)
    }
    else{
      stop(paste(money_var,"is not present in data."))
    }
  }


  data[[money_var]] <- as.numeric(data[[money_var]])

  if(tolower(substr(path,1,4))=="http"&!RCurl::url.exists(file.path(path,directory,deflator_file))
     || path=="offline"){
    warning("Using offline path")
    path<-get_local_lookup_path()
  }

  cat(paste("\n Applying\n", deflator_var, "\n in \n", deflator_file, "\n from\n", path, "\n"))
  deflators_retrieved <- readr::read_csv(file.path(path, directory,deflator_file))

  #Rename the Fiscal.Year variable to be match the name used in data
  colnames(deflators_retrieved)[colnames(deflators_retrieved)=="Fiscal_Year"]<-fy_var

  #Drop unneded deflator columns and then join the deflators to the data
  deflators_retrieved<-subset(deflators_retrieved,select=c(fy_var,deflator_var))
  data<-plyr::join(data,deflators_retrieved,by=fy_var)

  #Create current and constant dollar variants of money_var
  data[[paste(money_var,deflator_var,sep="_")]] <- as.numeric(as.character(
    data[[money_var]])) / as.numeric(data[[deflator_var]])

  colnames(data)[colnames(data)==money_var]<-paste(money_var,"Then_Year",sep="_")

  #Drop the deflator var unless deflator_dropped = FALSE
  if(deflator_dropped)
    data<-data[,colnames(data)!=deflator_var]

  #Standardize the newly created names
  data<-standardize_variable_names(data,
                                   var=c(paste(money_var,"Then_Year",sep="_"),
                                         paste(money_var,deflator_var,sep="_")))

  return(data)
}






#' Get Column Key based on the names in a data frame
#'
#' @param x A list of numbers stored as characters or factor
#'
#' @return The list in numerical format
#'
#' @details Converts from factor/character to number and strips
#' out commas and $s along the way.
#'
#' @examples
#'
#' text_to_number(c('10','1,000','$20.00'))
#'
#' @export
text_to_number<-function(x){
  if ((is.factor(x))||(is.character(x))){
    x<-gsub('\\$','',as.character( x))
    #Remove commas
    x<-gsub('\\,','',as.character( x))
    #Switch to negative if handling Handle the ($XX,XXX) case
    x<-gsub('\\(','-',as.character( x))
    x<-gsub('\\)$','',as.character( x))
    # if(length(grep("\\([0-9|\\,|\\.]+\\)", as.character(x)))==1){
    #   x<-gsub('[\\(|\\)]','',as.character( x))
    #   x<-as.double(as.character(x))*-1
    # }
    x<-as.double(as.character(x))
  }
  x
}



#' Get Column Key based on the names in a data frame
#'
#' @param x Text / numerical expression of a bit
#'
#' @return A 0,1,NULL value.
#'
#' @details Converts from character/number to bit
#'
#' @examples
#'
#' text_to_bit(c('Y','1','$20.00'))
#'
#' @export
text_to_bit<-function(x){
  x<-trimws(str_to_upper(as.character(x)))
  yes_list<-c("Y","YES","Y: YES","Y:",
              "1","TRUE","TRUE:","T","T:")
  no_list<-c("N","NO","N: NO",": NO","N:",
             "0","FALSE","FALSE:","F","F:")
  na_list<-c("",":")
  b<-
    case_when(x %in% yes_list ~
                1,
              x %in% no_list ~
                0,
              x %in% na_list~
                NA,
    )
  if(any(!x %in% c(yes_list,no_list,na_list) & !is.na(x))){
    stop(paste0("text_to_bit does not know how to handle ",
                x[!x %in% c(yes_list,no_list,na_list) & !is.na(x)],"\n"))
  }
  x<-as.integer(b)
  x
}


#' Get Column Key based on the names in a data frame
#'
#' @param x A date.
#'
#' @return The fiscal year of the date.
#'
#' @details Converts the date to fiscal year by
#' extracting the date and adding 1 if the month
#' is october or later.
#' @examples
#'
#' get_fiscal_year(as.Date('2019-12-10'))
#'
#' @export
get_fiscal_year<-function(
    x){

  lubridate::year(x)+if_else(lubridate::month(x)<10,0,+1)
  # ymd(paste(          ,"-10-1",sep=""))
}


#' Label Top X entries in a data frame
#'
#' @param df A data frame.
#' @param col The column to be evaluated.
#' @param weight The variable summed to determine top entries, by default Action_Obligation_OMB25_GDP23
#' @param top_name The new column that will include
#' @param group Sub groupings by which to rank, by default not included
#' @param time The variable used for when considering recent top entries, by default Fiscal_Year
#' @param recent Value used to give extra weight to recently top entries rather than just the overall period
#' @param retain_rank Retain any ranked variables created in processing.
#' @param write_file File name in which to record the calculations underlying the labeling. Default is not to record.
#'
#' @return The revised data frame with the new variable
#'
#' @details Add column variants, including summaries, and deflated dollars,
#' to a contract dataset.
#'
#'
#' @export
#'
label_top<-function(df,
                    col,
                    n=5,
                    weight="Action_Obligation_OMB25_GDP23",
                    top_name=NA,
                    group_list=NA,
                    time="Fiscal_Year",
                    recent=NA,
                    retain_rank=FALSE,
                    write_file=NA){


  agg_list<-col
  if(!col %in% colnames(df))
    stop(paste(col,"is not founds in the columns of df"))
  if(!weight %in% colnames(df))
    stop(paste(weight,"is not founds in the columns of df"))
  if(any(!is.na(group_list))){
    agg_list<-c(col,group_list)
    for(g in group_list){
      if(!g %in% colnames(df))
        stop(paste(g,"is not founds in the columns of df"))
    }
  }

  if(is.na(top_name)) top_name<-paste0("Top_",col)
  if(top_name %in% colnames(df)){
    warning(paste(top_name,"already present in df, removing."))
    df<-df[,colnames(df)!=top_name]
  }

  if(is.na(recent)){
    #Just overall
    if(length(agg_list) == 1){
      agg_df <- df %>%
        dplyr::group_by(!! as.name(agg_list)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), weight = as.name(weight)))
      agg_df<- agg_df %>% mutate(
        rank_total=rank(desc(agg_val))) %>%
        arrange(desc(agg_val))
    } else {
      agg_df <- df %>%
        dplyr::group_by_(.dots = c(agg_list)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(weight)))
      agg_df<- agg_df %>% dplyr::group_by_(.dots = c(group_list)) %>%
        mutate(
          rank_total=rank(desc(agg_val))) %>%
        arrange(desc(agg_val))
    }
    agg_df[,top_name]<-NA
    agg_df[agg_df$rank_total<=n,top_name]<-
      agg_df[agg_df$rank_total<=n,col]

  } else{
    #Recent
    agg_df<-df
    if("recent" %in% colnames(agg_df))
      stop("'recent' is a column in agg_df and that name is needed.")

    agg_df$recent_weight<-if_else(agg_df[,time]>=recent,agg_df[,weight],NA)
    recent_weight<-"recent_weight"

    if(length(agg_list) == 1){
      agg_df <- agg_df %>%
        dplyr::group_by(!! as.name(agg_list)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(weight)),
          agg_val_recent = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(recent_weight))
        )
      agg_df<- agg_df %>% mutate(
        rank_total=rank(desc(agg_val)),
        rank_recent=rank(desc(agg_val_recent))) %>%
        arrange(desc(agg_val))
    } else {
      agg_df <- agg_df %>%
        dplyr::group_by_(.dots = c(agg_list)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(weight)),
          agg_val_recent = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(recent_weight)))
      agg_df<- agg_df %>% dplyr::group_by_(.dots = c(group_list)) %>%
        mutate(
          rank_total=rank(desc(agg_val)),
          rank_recent=rank(desc(agg_val_recent))) %>%
        arrange(desc(agg_val))

    }
    agg_df[,top_name]<-NA
    agg_df[agg_df$rank_total<=n|agg_df$rank_recent<=n ,top_name]<-
      agg_df[agg_df$rank_total<=n|agg_df$rank_recent<=n,col]

  }


  if(retain_rank){
    rank_total_name<-paste0(top_name,"_rank_total")
    if(rank_total_name %in% colnames(df)){
      warning(paste(rank_total_name,"already present in df, removing."))
      df<-df[,colnames(df)!=rank_total_name]
    }
    colnames(agg_df)[colnames(agg_df)=="rank_total"]<-rank_total_name
    if(!is.na(recent)){
      rank_recent_name<-paste0(top_name,"_rank_recent")
      if(rank_recent_name %in% colnames(df)){
        warning(paste(rank_recent_name,"already present in df, removing."))
        df<-df[,colnames(df)!=rank_recent_name]
      }
      colnames(agg_df)[colnames(agg_df)=="rank_recent"]<-rank_recent_name
    }
  }

  if(!is.na(write_file))
    write.csv(agg_df,file=write_file,row.names = FALSE)

  df<-left_join(df,agg_df[,!colnames(agg_df) %in% c(weight,"rank_total","rank_recent")],
                by=c(agg_list))


  # agg_df<-df %>% group_by (Project.Name,PlatformPortfolio) %>%
  #             # Action_Obligation_2022=sum(if_else(Fiscal_Year>=2022,Action_Obligation_OMB24_GDP22,0)))%>%
  #   summarise(Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23),
  #   group_by (PlatformPortfolio) %>%
  #   mutate(rank_total=rank(desc(Action_Obligation_OMB25_GDP23)),
  #          # rank_2022=rank(desc(Action_Obligation_2022)))
  # agg_df %>% arrange(desc(Action_Obligation_OMB25_GDP23))

  df[is.na(df[,top_name]) & !is.na(df[,col]),top_name]<-
    "Other Labeled"
  df
}


#' Add column variants, including summaries, and deflated dollars, to a contract dataset.
#'
#' @param df A data frame.
#' @param ISOalpha3_col="ISOalpha3" The column with country codes in 3 letter ISO format
#' @param drop_col=FALSE If true drop many of the supporting columns, by default FALSE.
#' @param prefix=NULL If provided, add to the start of all the added variables (e.g. Place, Vendor, Origin). Default NULL.
#' @param purge_unprefixed=FALSE If TRUE, drop all columns previously added by this function that lack a prefix. Used to avoid adding the same columns repeatedly.
#' @param skip_name=FALSE If true, do not add the country name field
#' @param path The path or url for the column key.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/Lookup/master/
#'
#' @return The fiscal year of the date.
#'
#' @details Add column variants, including summaries, and deflated dollars,
#' to a contract dataset.
#'
#'
#' @export
add_alliance<-function(df,ISOalpha3_col=  "ISOalpha3",drop_col=FALSE,prefix=NULL,purge_unprefixed=FALSE,skip_name=FALSE,
                       path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/"){
  df<-as.data.frame(df)
  if(!file.exists(file.path(path,"location/","SOSA.csv")) || path=="offline"){
    warning("Using offline path")
    path<-get_local_lookup_path()
  }


  if(purge_unprefixed)
    df<-df[,!colnames(df) %in%
             c("NATOyear","MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear","SEATOendYear","RioTreatyEndYear",
               "FiveEyes","OtherTreatyName","OtherTreatyStartYear","OtherTreatyEndYear",
               "RDPyear","SOSAyear","RDPsosa",
               "StateRegion","AcquisitionCooperation","MutualDefense","MutualAcquisition",
               "isforeign","IsForeign","EUentryYear","EUexitYear")]

  if("MutualDefense" %in%  colnames(df)) stop("Add Alliance has already been run on the data.frame")

  if(any(duplicated(colnames(df)))) stop("Duplicate Column Names")

  if(!ISOalpha3_col %in% colnames(df)) stop(paste("ISOalpha3_col,",ISOalpha3_col,"is missing."))
  if(!is.null(ISOalpha3_col)){
    if("alpha-3" %in% colnames(df)) stop("Already alpha-3 in column names")
    if(!is.null(prefix)) if(prefix %in% colnames(df) & !skip_name) stop(paste("Already",prefix,"in column names"))
    colnames(df)[colnames(df)==ISOalpha3_col]<-"alpha-3"
    df<-read_and_join_experiment(df,lookup_file="Location_CountryCodes.csv",
                                 dir="location/",
                                 path=path,
                                 add_var = c("name", "StateRegion","CombatantCommand","NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear",	"SEATOendYear",	"RioTreatyStartYear","RioTreatyEndYear"	,"FiveEyes"	,"NTIByear"	,"OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign","EUentryYear","EUexitYear"),#"USAIDregion",
                                 by="alpha-3",
                                 skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign","EUentryYear","EUexitYear"),
                                 missing_file="missing_CoutryCode_iso.csv"
    )
    df$isforeign<-as.logical(df$isforeign)
    colnames(df)[colnames(df)=="alpha-3"]<-ISOalpha3_col
    if(skip_name) df <- df %>% dplyr::select(-name)
    else if(!is.null(prefix)) colnames(df)[colnames(df)=="name"]<-prefix
    else if("CountryName" %in% colnames(df)) stop(paste("Already CountryName in column names"))
    else colnames(df)[colnames(df)=="name"]<-"CountryName"
  }
  if(any(duplicated(colnames(df)))) stop(paste("Duplicate Column Names",colnames(df)[duplicated(colnames(df))]))


  if("dtDelivYear" %in% colnames(df)) compare_year<-year(df$dtDelivYear)
  else if("year" %in% colnames(df)) compare_year<-df$year
  else if("Delivery.year" %in% colnames(df)) compare_year<-df$Delivery.year
  else if("Fiscal_Year" %in% colnames(df)) compare_year<-df$Fiscal_Year
  else stop("Unkown comparison year")

  if(!all(c("MajorNonNATOentryYear","MajorNonNATOexitYear","NATOyear","NTIByear") %in% colnames(df))) stop("Missing Columns. Run Read And Join")


  ###### defense acquisition cooperation
  # major non-nato ally
  # afghanistan added 2012,  tunisia added 2015

  rdp_qa<-as.data.frame(readr::read_csv(file.path(
    path,"location/","RDP_QA.csv")))
  rdp_qa$Agreement_Start<-as.Date(rdp_qa$Agreement_Start,format="%m/%d/%Y")
  if(any(is.na(rdp_qa$Agreement_Start)))
    stop("NA Agreement_Start after import in location/RDO_QA.csv")
  rdp_qa<-rdp_qa %>% filter(IsRDP==TRUE) %>%
    group_by(ISOalpha3) %>%
    dplyr::summarise(RDPyear=min(lubridate::year(Agreement_Start)))
  rdp_by<-"ISOalpha3"
  names(rdp_by)<-ISOalpha3_col
  df<-left_join(df,rdp_qa,by=rdp_by)

  if(any(duplicated(colnames(df)))) stop(paste("Duplicate Column Names",colnames(df)[duplicated(colnames(df))]))
  rm(rdp_by)


  sosa<-as.data.frame(readr::read_csv(file.path(
    path,"location/","SOSA.csv")))
  sosa$Agreement_Start<-as.Date(sosa$Agreement_Start,format="%m/%d/%Y")
  if(any(is.na(sosa$Agreement_Start)))
    stop("NA Agreement_Start after import in location/SOSA.csv")
  sosa<-sosa %>%
    group_by(ISOalpha3) %>%
    dplyr::summarise(SOSAyear=min(lubridate::year(Agreement_Start)))
  sosa_by<-"ISOalpha3"
  names(sosa_by)<-ISOalpha3_col
  df<-left_join(df,sosa,by=sosa_by)

  if(any(duplicated(colnames(df)))) stop(paste("Duplicate Column Names",colnames(df)[duplicated(colnames(df))]))
  rm(sosa_by)


  df$AcquisitionCooperation<-NA
  df$AcquisitionCooperation[!is.na(df$MajorNonNATOentryYear)&
                              df$MajorNonNATOentryYear<=compare_year&
                              (is.na(df$MajorNonNATOexitYear)|
                                 df$MajorNonNATOexitYear>compare_year)
  ] <- "Major Non-NATO"

  if("Buyer" %in% colnames(df)){
    if("dtDelivYear" %in% colnames(df))
      df %>% group_by(AcquisitionCooperation,Buyer,RDPyear) %>%
      summarise(MinYear=min(dtDelivYear),
                MaxYear=max(dtDelivYear))
    else if("year" %in% colnames(df))
      df %>% group_by(AcquisitionCooperation,Buyer,RDPyear) %>%
      summarise(MinYear=min(year),
                MaxYear=max(year))
    else if("Delivery.year" %in% colnames(df))
      df %>% group_by(AcquisitionCooperation,Buyer,RDPyear) %>%
      summarise(MinYear=min(Delivery.year),
                MaxYear=max(Delivery.year))
    else if("Fiscal_Year" %in% colnames(df))
      df %>% group_by(AcquisitionCooperation,Buyer,RDPyear) %>%
      summarise(MinYear=min(Fiscal_Year),
                MaxYear=max(Fiscal_Year))

  }

  # df$EUtrade[!is.na(df$EUentryYear)&df$name %in%
  #            !is.na(df$EUtrade) &
  #             df$EUentryYear>compare_year]<- "Future EU"


  # nato for ac
  # montengro nato after 2017
  df$AcquisitionCooperation[!is.na(df$NATOyear)&
                              df$NATOyear<=compare_year] <- "NATO"

  # NTIB for cooperation
  df$AcquisitionCooperation[!is.na(df$NTIByear)&
                              df$NTIByear<=compare_year] <- "NTIB"


  # EU
  df$EUtrade<-NA
  df$EUtrade[!is.na(df$EUentryYear)&
               df$EUentryYear<=compare_year &
               (is.na(df$EUexitYear) | df$EUexitYear>compare_year)]<- "EU"
  # df$EUtrade[is.na(df$EUtrade) & !is.na(df$EUentryYear)&
  #             df$EUentryYear>compare_year]<- "Future EU"
  # df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("GBR")]<-"Post-Brexit UK"
  df$EUtrade[is.na(df$EUtrade) & (df$AcquisitionCooperation=="NATO" | df[,ISOalpha3_col] %in% c("GBR","CAN"))]<-"Other NATO"
  # df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("ISR","KOR","CHE")]<-"Switzerland, Israel, & South Korea" #"JPN","TWN"
  df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("KOR")]<-"South Korea" #"JPN","TWN"
  df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("RUS","CHN")]<-"PRC and Russia"
  df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("UKR")]<-"Ukraine"
  df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("USA")]<-"United States"
  df$EUtrade[is.na(df$EUtrade) & df[,ISOalpha3_col] %in% c("@QW","@QZ","@QS")]<-"Unspecified extra-union"
  df$EUtrade[is.na(df$EUtrade) & (is.na(df$StateRegion)|df$StateRegion=="Unlabeled")]<-"Unlabeled"
  df$EUtrade[is.na(df$EUtrade)]<-"Rest of World"


  # NATO
  df$NATOtrade<-NA
  df$NATOtrade[!is.na(df$EUentryYear)&
                 df$EUentryYear<=compare_year &
                 (is.na(df$EUexitYear) | df$EUexitYear>compare_year)&
                 (df$AcquisitionCooperation=="NATO" & !is.na(df$AcquisitionCooperation))
  ]<- "EU & NATO"
  # df$NATOtrade[is.na(df$NATOtrade) & !is.na(df$EUentryYear)&
  #             df$EUentryYear>compare_year]<- "Future EU"
  # df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("GBR")]<-"Post-Brexit UK"
  df$NATOtrade[is.na(df$NATOtrade) & (df$AcquisitionCooperation=="NATO" | df[,ISOalpha3_col] %in% c("GBR","CAN"))]<-"Other NATO"
  df$NATOtrade[!is.na(df$EUentryYear)&
                 df$EUentryYear<=compare_year &
                 (is.na(df$EUexitYear) | df$EUexitYear>compare_year)&
                 (df$AcquisitionCooperation!="NATO" & !is.na(df$AcquisitionCooperation))
  ]<- "EU Outside NATO"
  # df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("ISR","KOR","CHE")]<-"Switzerland, Israel, & South Korea" #"JPN","TWN"
  df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("KOR")]<-"South Korea" #"JPN","TWN"
  # df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("RUS","CHN")]<-"PRC and Russia"
  df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("UKR")]<-"Ukraine"
  df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("USA")]<-"United States"
  df$NATOtrade[is.na(df$NATOtrade) & df[,ISOalpha3_col] %in% c("@QW","@QZ","@QS")]<-"Unspecified extra-union"
  df$NATOtrade[is.na(df$NATOtrade) & (is.na(df$StateRegion)|df$StateRegion=="Unlabeled")]<-"Unlabeled"
  df$NATOtrade[is.na(df$NATOtrade)]<-"Rest of World"

  # NTIB
  df$NTIBtrade<-NA
  df$NTIBtrade[is.na(df$NTIBtrade) & df[,ISOalpha3_col] %in% c("USA")]<-"United States"
  df$NTIBtrade[!is.na(df$EUentryYear)&
                 df$EUentryYear<=compare_year &
                 (is.na(df$NTIByear) | df$NTIByearWill>compare_year)&
                 (df$AcquisitionCooperation=="NATO" & !is.na(df$AcquisitionCooperation))
  ]<- "EU & NATO"

  df$NTIBtrade[!is.na(df$EUentryYear)&
                 df$EUentryYear<=compare_year &
                 (is.na(df$EUexitYear) | df$EUexitYear>compare_year)&
                 (df$AcquisitionCooperation=="NATO" & !is.na(df$AcquisitionCooperation))
  ]<- "EU & NATO"
  # df$NTIBtrade[is.na(df$NTIBtrade) & !is.na(df$EUentryYear)&
  #             df$EUentryYear>compare_year]<- "Future EU"
  # df$NTIBtrade[is.na(df$NTIBtrade) & df[,ISOalpha3_col] %in% c("GBR")]<-"Post-Brexit UK"
  df$NTIBtrade[is.na(df$NTIBtrade) & (df$AcquisitionCooperation=="NATO" | df[,ISOalpha3_col] %in% c("GBR","CAN"))]<-"Other NATO"
  df$NTIBtrade[!is.na(df$EUentryYear)&
                 df$EUentryYear<=compare_year &
                 (is.na(df$EUexitYear) | df$EUexitYear>compare_year)&
                 (df$AcquisitionCooperation!="NATO" & !is.na(df$AcquisitionCooperation))
  ]<- "EU Outside NATO"
  df$NTIBtrade[is.na(df$NTIBtrade) & df[,ISOalpha3_col] %in% c("ISR","KOR","CHE")]<-"Switzerland, Israel, & South Korea" #"JPN","TWN"
  # df$NTIBtrade[is.na(df$NTIBtrade) & df[,ISOalpha3_col] %in% c("RUS","CHN")]<-"PRC and Russia"
  df$NTIBtrade[is.na(df$NTIBtrade) & df[,ISOalpha3_col] %in% c("UKR")]<-"Ukraine"

  df$NTIBtrade[is.na(df$NTIBtrade) & df[,ISOalpha3_col] %in% c("@QW","@QZ","@QS")]<-"Unspecified extra-union"
  df$NTIBtrade[is.na(df$NTIBtrade) & (is.na(df$StateRegion)|df$StateRegion=="Unlabeled")]<-"Unlabeled"
  df$NTIBtrade[is.na(df$NTIBtrade)]<-"Rest of World"

  #RDPsosa
  df$RDPsosa<-NA
  df$RDPsosa[is.na(df$RDPyear) |  df$RDPyear>compare_year] <- "None"
  df$RDPsosa[!is.na(df$RDPyear)& df$RDPyear<=compare_year] <- "RDP"
  df$RDPsosa[!is.na(df$SOSAyear)&
               df$SOSAyear<=compare_year] <- paste(
                 df$RDPsosa[!is.na(df$SOSAyear)&
                              df$SOSAyear<=compare_year] ,"and SOSA")

  df$RDPsosa[df$RDPsosa=="None and SOSA"]<-"SOSA alone"
  summary(factor(df$RDPsosa))

  #RDP
  df$AcquisitionCooperation[!is.na(df$RDPyear)&
                              df$RDPyear<=compare_year] <- paste(
                                df$AcquisitionCooperation[!is.na(df$RDPyear)&
                                                            df$RDPyear<=compare_year] ,"and RDP")

  df$AcquisitionCooperation[df$AcquisitionCooperation=="NA and RDP"]<-"RDP alone"

  df$AcquisitionCooperation[df$Buyer=="United States"] <- "United States"

  df$AcquisitionCooperation<-ifelse(is.na(df$AcquisitionCooperation), "Rest of World", df$AcquisitionCooperation)


  #Mutual Defense Categories
  if(any(!complete.cases(df %>% dplyr::filter(!StateRegion %in% "Non-Regional" & !is.na(StateRegion)) %>% group_by() %>%
                         dplyr::select(ISOalpha3_col,"AcquisitionCooperation"))))
    stop(paste("Missing AcquisitionCooperation:",
               paste(unique(df[df$StateRegion !="Non-Regional" & !complete.cases(df %>% group_by() %>%
                                                                                   dplyr::select(ISOalpha3_col,"AcquisitionCooperation")),
                               ISOalpha3_col]),
                     collapse=", ")
    ))


  # assigning values - reverse priority
  #### mutual defense agreements
  # Southeast asia
  df$MutualDefense<-NA
  df$MutualDefense[!is.na(df$SEATOendYear) & compare_year<=df$SEATOendYear] <- "Southeast Asia"

  # rio
  df$MutualDefense[compare_year>=df$RioTreatyStartYear &
                     (df$RioTreatyEndYear>compare_year |
                        is.na(df$RioTreatyEndYear))
  ]<-"Rio Treaty"

  # nato
  # montengro nato after 2017
  df$MutualDefense[!is.na(df$NATOyear)&
                     df$NATOyear<=compare_year] <- "NATO"

  # Other Treaties for cooperation
  # ANZUS
  df$MutualDefense[!is.na(df$OtherTreatyStartYear)& df$OtherTreatyName=="ANZUS" &
                     df$OtherTreatyStartYear<=compare_year&
                     (df$OtherTreatyEndYear>compare_year |
                        is.na(df$OtherTreatyEndYear))] <- "ANZUS"

  # Bilateral
  df$MutualDefense[!is.na(df$OtherTreatyStartYear)& df$OtherTreatyName!="ANZUS" &
                     df$OtherTreatyStartYear<=compare_year&
                     (df$OtherTreatyEndYear>compare_year |
                        is.na(df$OtherTreatyEndYear))] <- "Bilateral Mutual Defense"

  df$MutualDefense<-ifelse(is.na(df$MutualDefense), "Rest of World", df$MutualDefense)
  summary(factor(df$MutualDefense))

  #Mutual Defense Categories
  if(any(!complete.cases(df %>% dplyr::filter(!StateRegion %in% c("Non-Regional") & !is.na(StateRegion)) %>%
                         group_by %>% dplyr::select(ISOalpha3_col,"MutualDefense"))))
    stop(paste("Missing MutualDefense:",
               paste(unique(df[df$StateRegion !="Non-Regional" & !complete.cases(df %>% group_by() %>%
                                                                                   dplyr::select(ISOalpha3_col,"MutualDefense")),
                               ISOalpha3_col]),
                     collapse=", ")
    ))


  # Combined Category
  df$MutualAcquisition<-factor(df$MutualDefense)



  levels(df$MutualAcquisition)<-
    list("NATO"=c("NATO"),
         "Major Non-NATO & Treaty Ally"=c("Major Non-NATO & Treaty Ally"),
         "Other Treaty Ally"=c("Other Treaty Ally","ANZUS","Southeast Asia","Bilateral Mutual Defense"),
         "Other Major Non-NATO"=c("Other Major Non-NATO"),
         "Rest of World"=c("Rest of World","Rio Treaty"))


  df$MutualAcquisition[df$MutualAcquisition=="Other Treaty Ally" &
                         !is.na(df$MajorNonNATOentryYear)&
                         df$MajorNonNATOentryYear<=compare_year&
                         (is.na(df$MajorNonNATOexitYear)|
                            df$MajorNonNATOexitYear>compare_year)] <- "Major Non-NATO & Treaty Ally"

  df$MutualAcquisition[df$MutualAcquisition=="Rest of World" &
                         !is.na(df$MajorNonNATOentryYear)&
                         df$MajorNonNATOentryYear<=compare_year&
                         (is.na(df$MajorNonNATOexitYear)|
                            df$MajorNonNATOexitYear>compare_year)] <- "Other Major Non-NATO"


  df$MutualAcquisition<-factor(df$MutualAcquisition,
                               levels=c("NATO","Major Non-NATO & Treaty Ally",
                                        "Other Major Non-NATO",
                                        "Other Treaty Ally",
                                        "Rest of World"))


  #Mutual Acquisition Categories
  if(any(!complete.cases(df %>% dplyr::filter(!StateRegion %in% c("Non-Regional") & !is.na(StateRegion)) %>% group_by() %>%
                         dplyr::select(ISOalpha3_col,"MutualAcquisition"))))
    stop(paste("Missing MutualAcquisition:",
               paste(unique(df[df$StateRegion !="Non-Regional" & !complete.cases(df %>% group_by() %>%
                                                                                   dplyr::select(ISOalpha3_col,"MutualAcquisition")),
                               ISOalpha3_col]),
                     collapse=", ")
    ))

  if(drop_col==TRUE)
    df <- df %>% dplyr::select(-NATOyear,-EUentryYear,-EUexitYear,	-MajorNonNATOentryYear,	-MajorNonNATOexitYear,
                               -NTIByear	,-SEATOendYear,-RioTreatyEndYear,-FiveEyes,
                               -OtherTreatyName	,-OtherTreatyStartYear,-OtherTreatyEndYear)

  if(!is.null(prefix)){
    renamelist<-c("NATOyear","MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear","SEATOendYear",
                  "RioTreatyStartYear","RioTreatyEndYear","FiveEyes",
                  "OtherTreatyName","OtherTreatyStartYear","OtherTreatyEndYear",
                  "RDPyear","SOSAyear","EUentryYear","EUexitYear","RDPsosa","EUtrade","NATOtrade","NTIBtrade")
    colnames(df)[colnames(df)%in% renamelist]<-
      paste(prefix,colnames(df)[colnames(df)%in%renamelist],sep="")
    colnames(df)[colnames(df)=="StateRegion"]<-paste(prefix,"StateRegion",sep="")
    colnames(df)[colnames(df)=="CombatantCommand"]<-paste(prefix,"CombatantCommand",sep="")
    colnames(df)[colnames(df)=="AcquisitionCooperation"]<-paste(prefix,"AcquisitionCooperation",sep="")
    colnames(df)[colnames(df)=="MutualDefense"]<-paste(prefix,"MutualDefense",sep="")
    colnames(df)[colnames(df)=="MutualAcquisition"]<-paste(prefix,"MutualAcquisition",sep="")
    colnames(df)[colnames(df)=="isforeign"]<-paste(prefix,"IsForeign",sep="")
  }
  if(any(duplicated(colnames(df)))) stop(paste("Duplicate Column Names",colnames(df)[duplicated(colnames(df))]))

  df

}


#' Add column variants, including summaries, and deflated dollars, to a contract dataset.
#'
#' @param df A data frame.
#' @param path The path or url for the column key.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/Lookup/master/
#' @param deflator_var Specifying which deflator to use, if unspecified will pass NULL to use default of deflate function
#' @param add_alliance=FALSE Should the function add alliance variables using country code variables? These are numerous, so left off by default.
#'
#' @return The fiscal year of the date.
#'
#' @details Add column variants, including summaries, and deflated dollars,
#' to a contract dataset.
#'
#'
#' @export
apply_standard_lookups<- function(df,path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                  deflator_var=NULL,
                                  call_add_alliance=FALSE){

  if(path=="offline"){
    warning("Using offline path")
    path<-get_local_lookup_path()
  }
  df<-standardize_variable_names(df)

  #Clear out blank/admin/error message rows at the end of the input file.
  while(substring(df[nrow(df),1],1,15) %in% c(
    "Completion time",
    "An error occurr",#ed while executing batch. Error message is: One or more errors occurred
    "Msg 208, Level ",#16, State 1, Procedure
    "Invalid object "#Invalid object name 'contract.FDPSpartia
  )){

    df<-df[-nrow(df),]
  }
  while(stringr::str_sub(df[nrow(df),1],-15,-1) %in% c(
    " row affected)\r",
    "rows affected)\r"
  )){

    df<-df[-nrow(df),]
  }


  #Empty rows
  if("Fiscal_Year" %in% colnames(df))
    if((df[nrow(df),1]=="" | is.na(df[nrow(df),1]))  & is.na(df$Fiscal_Year[nrow(df)]))
      df<-df[-nrow(df),]

  #### Dates #####
  if("action_date" %in% names(df)){
    conversion_test<- as.Date(df$action_date)
    if(any(is.na(conversion_test)&!is.na(df$action_date))){
      stop("Failed action_date conversion")
    }
    df$action_date <- conversion_test
    rm(conversion_test)

  }
  if("signeddate" %in% names(df)){
    conversion_test<- as.Date(df$signeddate)
    if(any(is.na(conversion_test)&!is.na(df$signeddate))){
      stop("Failed signeddate conversion")
    }
    df$signeddate <- conversion_test
    rm(conversion_test)

  }
  if("Fiscal_Year"%in% names(df)){

    df$Fiscal_Year <- text_to_number(df$Fiscal_Year)
    df$dFYear<-as.Date(paste("1/1/",as.character(df$Fiscal_Year),sep=""),"%m/%d/%Y")


    if("fiscal_quarter" %in% colnames(df)){
      df$Fiscal_YQ<-NA
      df$Fiscal_YQ[!is.na(df$fiscal_quarter)]<-text_to_number(paste(df$Fiscal_Year[!is.na(df$fiscal_quarter)],
                                                                    text_to_number(df$fiscal_quarter[!is.na(df$fiscal_quarter)]),sep="."))
      df$Fiscal_YQ[is.na(df$Fiscal_YQ)]<-df$Fiscal_Year[is.na(df$Fiscal_YQ)]
      df$YTD<-factor(if_else(df$Fiscal_Year==max(df$Fiscal_Year),"YTD","Full Year"))
    }
    else if ("fiscal_quarter_YTD" %in% colnames(df)){
      df$Fiscal_YQ<-NA
      df$Fiscal_YQ[!is.na(df$fiscal_quarter_YTD)]<-text_to_number(paste(df$Fiscal_Year[!is.na(df$fiscal_quarter_YTD)],
                                                                        text_to_number(df$fiscal_quarter_YTD[!is.na(df$fiscal_quarter_YTD)]),sep="."))
      df$Fiscal_YQ[is.na(df$Fiscal_YQ)]<-df$Fiscal_Year[is.na(df$Fiscal_YQ)]
      df$YTD<-factor(if_else(df$Fiscal_Year==max(df$Fiscal_Year),"YTD","Full Year"))
    }

    # df$Fiscal_Year.End <-as.Date(paste("9/30/",as.character(year(df$Fiscal_Year)),sep=""),"%m/%d/%Y")
    # df$Fiscal_Year.Start <-as.Date(paste("10/1/",as.character(year(df$Fiscal_Year)-1),sep=""),"%m/%d/%Y")
  } else if ("signeddate" %in% colnames(df)){
    df$Fiscal_Year<-get_fiscal_year(df$signeddate)
    df$Fiscal_YQ<-NA
    df$fiscal_quarter<-lubridate::quarter(df$Fiscal_Year,fiscal_start = 10)
    df$Fiscal_YQ[!is.na(df$fiscal_quarter)]<-text_to_number(paste(df$Fiscal_Year[!is.na(df$fiscal_quarter)],
                                                                  text_to_number(df$fiscal_quarter[!is.na(df$fiscal_quarter)]),sep="."))
    df$YTD<-factor(if_else(df$Fiscal_Year==max(df$Fiscal_Year),"YTD","Full Year"))

    df$dFYear<-as.Date(paste("1/1/",as.character(df$Fiscal_Year),sep=""),"%m/%d/%Y")
  }
  #
  #   #***Join relevant variables to lookup tables
  #   if("ContractingAgencyID" %in%  names(df) &
  #      "ContractingOfficeID" %in%  names(df)  &
  #      "Fiscal_Year" %in%  names(df)  &
  #      !("ContractingOfficeName" %in%  names(df)|
  #        "MajorCommandID"  %in%  names(df))){
  #
  #     if("MajorCommandCode"%in% names(df)){
  #       df<-subset(df, select=-c(MajorCommandCode))
  #     }
  #
  #     if("MajorCommandName"%in% names(df)){
  #       df<-subset(df, select=-c(MajorCommandName))
  #     }
  #
  #     if("MajorCommandID"%in% names(df)){
  #       df<-subset(df, select=-c(MajorCommandCode))
  #     }
  #
  #
  #     df<-replace_nas_with_unlabeled(df,"ContractingOfficeID")
  #     df<-replace_nas_with_unlabeled(df,"ContractingAgencyID")
  #
  #     df<-read_and_join_experiment(df,
  #                           "Defense_Major_Command_Codes_and_Offices.csv",
  #                           by=c("Fiscal_Year",
  #                                "ContractingAgencyID",
  #                                "ContractingOfficeID"),
  #                           new_var_checked=FALSE)
  #
  #     NA.check.df<-subset(df, is.na(MajorCommandCode) &
  #                           ContractingAgencyID!="Uncategorized" &
  #                           ContractingAgencyID!="ContractingOfficeID" &
  #                           !is.na(Fiscal_Year),
  #                         select=c("Fiscal_Year",
  #                                  "ContractingOfficeID",
  #                                  "ContractingOfficeName",
  #                                  "MajorCommandID",
  #                                  "MajorCommandCode",
  #                                  "MajorCommandName"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in MajorCommandCode"))
  #     }
  #     #
  #     #     NA.check.df<-subset(df,is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
  #     #     if(nrow(NA.check.df)>0){
  #     #       print(unique(NA.check.df))
  #     #       stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.detail"))
  #     #     }
  #     #
  #     #     NA.check.df<-subset(df,is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
  #     #     if(nrow(NA.check.df)>0){
  #     #       print(unique(NA.check.df))
  #     #       stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.component"))
  #     #     }
  #     #
  #     #Rather than have one uncategorized per year, just manually assigning
  #     df<-replace_nas_with_unlabeled(df,"MajorCommandName","Uncategorized")
  #
  #
  #   }
  #   else if("MajorCommandID" %in%  names(df)){
  #
  #     if("MajorCommandCode"%in% names(df)){
  #       df<-subset(df, select=-c(MajorCommandCode))
  #     }
  #
  #     if("MajorCommandName"%in% names(df)){
  #       df<-subset(df, select=-c(MajorCommandName))
  #     }
  #
  #     # df<-replace_nas_with_unlabeled(df,"MajorCommandName","Uncategorized")
  #
  #     df<-read_and_join_experiment(df,
  #                           "Lookup_MajorCommandID.csv",
  #                           by="MajorCommandID",
  #                           skip_check_var=c("ContractingOfficeCode",
  #                                            "ContractingOfficeName"
  #                           ))
  #
  #     df<-replace_nas_with_unlabeled(df,"MajorCommandID","Uncategorized")
  #
  #
  #   }
  #
  #   else if("CSISofficeName" %in%  names(df)){
  #
  #     df<-replace_nas_with_unlabeled(df,"CSISofficeName","Uncategorized")
  #
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_CSISofficeName.txt",
  #                           by="CSISofficeName")
  #
  #   }
  #
  #


  # Organization Funder ####



  if("fundingrequestingagencyid" %in% names(df))
  {

    if("Funding.Department.ID" %in% names(df)){
      df<-subset(df, select=-c(Funding.Department.ID))
    }
    if("Funding.Agency.Name"%in% names(df)){
      df<-subset(df, select=-c(Funding.Agency.Name))
    }

    if("FundingAgencyName"%in% names(df)){
      df<-subset(df, select=-c(FundingAgencyName))
    }
    if("Funder"%in% names(df)){
      df<-subset(df, select=-c(Funder))
    }
    if("SubFunder"%in% names(df)){
      df<-subset(df, select=-c(SubFunder))
    }
    if(is.numeric(df$fundingrequestingagencyid))
      df$fundingrequestingagencyid<-as.character(df$fundingrequestingagencyid)
    df<-read_and_join_experiment(df,
                                 path=path,
                                 "Agency_AgencyID.csv",
                                 directory="",
                                 by=c("fundingrequestingagencyid"="AgencyID"),
                                 add_var=c("Customer","SubCustomer","AgencyIDtext"),#Funding.Agency.ID
                                 skip_check_var=c("Platform","Customer","SubCustomer","AgencyIDtext"),
                                 guess_max=2000,
                                 lookup_char_as_factor = TRUE
    )
    colnames(df)[colnames(df)=="AgencyIDtext"]<-"FundingAgencyName"

    if("fundingrequestingofficeid" %in% names(df) & !"FundingMajorCommandID" %in% names(df)){

      colnames(df)[colnames(df)=="MajorCommandID"]<-"ContractingMajorCommandID"
      colnames(df)[colnames(df)=="MajorCommandCode"]<-"ContractingMajorCommandCode"
      colnames(df)[colnames(df)=="MajorCommandName"]<-"ContractingMajorCommandName"

      df<-read_and_join_experiment(df,
                                   path=path,
                                   directory="office\\",
                                   lookup_file = "MajComID.csv",
                                   by =c("Fiscal_Year"="Fiscal_Year",
                                         "fundingrequestingagencyid"="Contracting_Agency_ID",
                                         "fundingrequestingofficeid"="ContractingOfficeID"),
                                   skip_check_var = "MajorCommandID")

      df<-read_and_join_experiment(df,
                                   path=path,
                                   directory="office\\",
                                   lookup_file = "MajComSum.csv",
                                   lookup_char_as_factor = TRUE)

      colnames(df)[colnames(df)=="MajorCommandID"]<-"FundingMajorCommandID"
      colnames(df)[colnames(df)=="MajorCommandCode"]<-"FundingMajorCommandCode"
      colnames(df)[colnames(df)=="MajorCommandName"]<-"FundingMajorCommandName"


      colnames(df)[colnames(df)=="ContractingMajorCommandID"]<-"MajorCommandID"
      colnames(df)[colnames(df)=="ContractingMajorCommandCode"]<-"MajorCommandCode"
      colnames(df)[colnames(df)=="ContractingMajorCommandName"]<-"MajorCommandName"
    }
  }
  # if("Customer" %in% names(df) && "SubCustomer" %in% names(df)){
  #   if("SubCustomer.sum"%in% names(df)){
  #     df<-subset(df, select=-c(SubCustomer.sum))
  #   }
  #
  #
  #   df<-replace_nas_with_unlabeled(df,"SubCustomer","Uncategorized")
  #   df<-replace_nas_with_unlabeled(df,"Customer","Uncategorized")
  #
  #   #     debug(read_and_join_experiment)
  #   df<-read_and_join_experiment(df,
  #                                         "SubCustomer.csv",
  #                                         by=c("Customer"="Customer","SubCustomer"="SubCustomer"),
  #                                         add_var=c("SubCustomer.platform","SubCustomer.sum"),
  #                                         path=path,
  #                                         directory="office/"
  #   )
  # }
  # else if ("FundingCustomer" %in% names(df) & "FundingSubCustomer" %in% names(df)){
  #   df<-replace_nas_with_unlabeled(df,"FundingSubCustomer","Uncategorized")
  #   df<-read_and_join_experiment(df,
  #                                         "SubCustomer.csv",
  #                                         by=c("FundingCustomer"="Customer","FundingSubCustomer"="SubCustomer"),
  #                                         add_var=c("SubCustomer.platform","SubCustomer.sum"),
  #                                         path=path,
  #                                         directory="office/")
  # }

  # Organization Awarding ####
  if("agencyid" %in% names(df) &
     "PIID" %in% names(df) &
     "idvagencyid" %in% names(df) &
     "idvpiid" %in% names(df)
  )
  {

    df<-df%>% mutate(derived_link=paste("https://www.usaspending.gov/award/CONT_AWD_",PIID,"_",agencyid,"_",
                                        if_else(is.na(idvpiid)|idvpiid=="","-NONE-",idvpiid),"_",
                                        if_else(is.na(idvagencyid)|idvagencyid=="","-NONE-",idvagencyid),"/",sep=""))
  }

  if("Contracting_Agency_ID" %in% names(df))
  {

    if("Contracting.Department.ID" %in% names(df)){
      df<-subset(df, select=-c(Contracting.Department.ID))
    }
    if("ContractingAgencyName"%in% names(df)){
      df<-subset(df, select=-c(ContractingAgencyName))
    }

    if("Contracting.Agency.Name"%in% names(df)){
      df<-subset(df, select=-c(Contracting.Agency.Name))
    }
    if("Customer"%in% names(df)){
      df<-subset(df, select=-c(Customer))
    }
    if("SubCustomer"%in% names(df)){
      df<-subset(df, select=-c(SubCustomer))
    }
    if(is.numeric(df$Contracting_Agency_ID))
      df$Contracting_Agency_ID<-as.character(df$Contracting_Agency_ID)
    df<-read_and_join_experiment(df,
                                 path=path,
                                 "Agency_AgencyID.csv",
                                 directory="",
                                 by=c("Contracting_Agency_ID"="AgencyID"),
                                 add_var=c("Customer","SubCustomer","AgencyIDtext"),#Contracting.Agency.ID
                                 skip_check_var=c("Platform","Customer","SubCustomer","AgencyIDtext"),
                                 guess_max=2000,
                                 lookup_char_as_factor = TRUE)
    colnames(df)[colnames(df)=="AgencyIDtext"]<-"ContractingAgencyName"

    if("ContractingOfficeID" %in% names(df) & !"MajorCommandID" %in% names(df)){

      df<-read_and_join_experiment(df,
                                   path=path,
                                   directory="office\\",
                                   lookup_file = "MajComID.csv",
                                   by =c("Fiscal_Year"="Fiscal_Year",
                                         "Contracting_Agency_ID"="Contracting_Agency_ID",
                                         "ContractingOfficeID"="ContractingOfficeID"),
                                   skip_check_var = "MajorCommandID",
                                   lookup_char_as_factor = TRUE)

      df<-read_and_join_experiment(df,
                                   path=path,
                                   directory="office\\",
                                   lookup_file = "MajComSum.csv",
                                   lookup_char_as_factor = TRUE)




    }
  }

  # if("awarding_agency_code" %in% names(df))
  # {
  #
  #   df<-read_and_join_experiment(df,
  #                                path=path,
  #                                "Awarding_Agency_Code.csv",
  #                                directory="office",
  #                                by=c("awarding_agency_code"="awarding_agency_code"),
  #                                add_var=c("Customer","SubCustomer","AgencyIDtext"),#Contracting.Agency.ID
  #                                skip_check_var=c("Platform","Customer","SubCustomer","AgencyIDtext"),
  #                                guess_max=2000)
  #   colnames(df)[colnames(df)=="AgencyIDtext"]<-"ContractingAgencyName"
  #
  #   if("ContractingOfficeID" %in% names(df) & !"MajorCommandID" %in% names(df)){
  #
  #     df<-read_and_join_experiment(df,
  #                                  path=path,
  #                                  directory="office\\",
  #                                  lookup_file = "MajComID.csv",
  #                                  by =c("Fiscal_Year"="Fiscal_Year",
  #                                        "Contracting_Agency_ID"="Contracting_Agency_ID",
  #                                        "ContractingOfficeID"="ContractingOfficeID"),
  #                                  skip_check_var = "MajorCommandID")
  #
  #     df<-read_and_join_experiment(df,
  #                                  path=path,
  #                                  directory="office\\",
  #                                  lookup_file = "MajComSum.csv")
  #
  #
  #
  #
  #   }
  # }


  if("Customer" %in% names(df)){
    df$Customer<-as.factor(df$Customer)
    if("SubCustomer" %in% names(df)){
      if("SubCustomer.sum"%in% names(df)){
        df<-subset(df, select=-c(SubCustomer.sum))
      }


      df<-replace_nas_with_unlabeled(df,"SubCustomer","Uncategorized")
      df<-replace_nas_with_unlabeled(df,"Customer","Uncategorized")

      #     debug(read_and_join_experiment)
      df<-read_and_join_experiment(df,
                                   "SubCustomer.csv",
                                   by=c("Customer"="Customer","SubCustomer"="SubCustomer"),
                                   add_var=c("SubCustomer.platform","SubCustomer.sum"),
                                   path=path,
                                   directory="office/",
                                   lookup_char_as_factor = TRUE
      )
    }
  }
  else if ("ContractingCustomer" %in% names(df)){
    df$ContractingCustomer<-as.factor(df$ContractingCustomer)
    if("ContractingSubCustomer" %in% names(df)){
    df$ContractingCustomer[df$ContractingCustomer==""]<-NA
    df<-replace_nas_with_unlabeled(df,"ContractingSubCustomer","Uncategorized")
    df<-read_and_join_experiment(df,
                                 "SubCustomer.csv",
                                 by=c("ContractingCustomer"="Customer","ContractingSubCustomer"="SubCustomer"),
                                 add_var=c("SubCustomer.platform","SubCustomer.sum"),
                                 path=path,
                                 directory="office/",
                                 lookup_char_as_factor = TRUE)
    }
  }

  # else if ("SubCustomer" %in% names(df)){
  #   stop("Customer is missing from the table, SubCustomer does not stand alone.")
  # }
  # else if("Customer" %in% names(df)){
  #   df<-replace_nas_with_unlabeled(df,"Customer")
  #   df<-read_and_join_experiment(df,
  #                         "LOOKUP_Customer.csv",
  #                         by=c("Customer"))
  #   # NA.check.df<-subset(df,is.na(Customer.sum), select=c("Customer","Customer.sum"))
  #   # if(nrow(NA.check.df)>0){
  #   #   print(unique(NA.check.df))
  #   #   stop(paste(nrow(NA.check.df),"rows of NAs generated in Customer.sum"))
  #   # }
  #
  # }


  # classify competition
  if("CompetitionClassification" %in% names(df) & "ClassifyNumberOfOffers" %in% names(df) )
  {
    df$CompetitionClassification <- as.factor(df$CompetitionClassification)
    df<-read_and_join_experiment(df,
                                 "CompetitionClassification.csv",
                                 by=c("CompetitionClassification","ClassifyNumberOfOffers"),
                                 replace_na_var="ClassifyNumberOfOffers",
                                 add_var=c("Competition.sum",
                                           "Competition.multisum",
                                           "Competition.effective.only",
                                           "No.Competition.sum"),
                                 path=path,
                                 directory="contract/",
                                 lookup_char_as_factor = TRUE
    )
  }
  if("Vehicle" %in% names(df) ){
    df$Vehicle<-factor(df$Vehicle)
    df<-read_and_join_experiment(df,
                                 "Vehicle.csv",
                                 by=c("Vehicle"="Vehicle.detail"),
                                 add_var=c("Vehicle.sum","Vehicle.sum7","Vehicle.AwardTask"),
                                 path=path,
                                 # path="K:/Users/Greg/Repositories/Lookup-Tables/",
                                 directory="contract/",
                                 lookup_char_as_factor = TRUE
    )
  }

  # Mechanism Type ####
  if("assistance_type_code" %in% names(df) ){
    df<-df %>% mutate(cfda_num=if_else(
      nchar(cfda_number)>6,
      round(text_to_number(cfda_number),3),
      cfda_number))
    if(any(!is.na(df$cfda_number)&is.na(df$cfda_num)))
      stop("Mangled CFDA number")
    df<-read_and_join_experiment(df,directory="assistance//",lookup_file="assistance_type_code.csv",
                                 by="assistance_type_code",path=path)
  }

  if("assistance_type_code" %in% names(df) ){
    df<-read_and_join_experiment(data=df
                                 ,"assistance_type_code.csv"
                                 ,path=path
                                 ,directory="assistance//"
                                 ,by="assistance_type_code",
                                 ,add_var = c("assistance_type_description"),
                                 # ,skip_check_var = c("PricingInflation","TypeOfContractPricingText")
                                 ,lookup_char_as_factor = TRUE
                                 # ,new_var_checked=FALSE
                                 # ,create_lookup_rdata=TRUE
                                 # ,col_types="dddddddddccc"
    )
  }
  if("costaccountingstandardsclause" %in% names(df)){
    df$costaccountingstandardsclause[df$costaccountingstandardsclause==""]<-NA
    df$costaccountingstandardsclause<-as.factor(df$costaccountingstandardsclause)
  }
  if("costorpricingdata" %in% names(df))
    df$costorpricingdata[df$costorpricingdata==""]<-NA

  if("LetterContract" %in% names(df) & !"IsUndefinitizedAction" %in% names(df) ){
    df<-read_and_join_experiment(data=df
                                 ,"LetterContract.csv"
                                 ,path=path
                                 ,directory="contract/"
                                 ,add_var = c("IsUndefinitizedAction")
                                 ,skip_check_var = c("IsUndefinitizedAction")
    )
  }
  if("IsUndefinitizedAction" %in% names(df)){
    df$IsUndefinitizedAction<-as.logical(df$IsUndefinitizedAction)
  }

  if(is.character(df$AnyCommercial)){
    df$AnyCommercial<-factor(df$AnyCommercial)
  }

  if("TypeOfContractPricing" %in% names(df) ){
    df$TypeOfContractPricing<-factor(df$TypeOfContractPricing)

    if("TypeOfContractPricingText" %in% names(df))
      df<-df %>% select(-TypeOfContractPricingText)

    df<-read_and_join_experiment(data=df
                                 ,"contract.TypeOfContractPricing.csv"
                                 ,path=path
                                 ,directory="contract/"
                                 ,add_var = c("TypeOfContractPricingText","PricingFee",
                                              "PricingInflation","Pricing.sum")
                                 ,skip_check_var = c("TypeOfContractPricingText","PricingFee",
                                                     "PricingInflation","Pricing.sum"),
                                 lookup_char_as_factor = TRUE
                                 # ,by=c("informationtechnologycommercialitemcategory"="informationtechnologycommercialitemcategory")
                                 # ,new_var_checked=FALSE
                                 # ,create_lookup_rdata=TRUE
                                 # ,col_types="dddddddddccc"
    )
  }


  if("IsUndefinitizedAction" %in% names(df) & !"PricingUCA" %in% names(df) &
     "PricingFee" %in% names(df)){
    df$PricingUCA<-as.character(df$PricingFee)
    df$PricingUCA[df$IsUndefinitizedAction==1]<-"UCA"
    df$PricingUCA<-factor(df$PricingUCA)

  }
  else if("PricingUCA" %in% names(df)){
    if(is.character(df$PricingUCA))
      df$PricingUCA<-factor(df$PricingUCA)
  }

  if("PricingUCA" %in% names(df) & !"PricingUCA.sum" %in% names(df)  & "Pricing.sum" %in% names(df)){
    # # df$PricingUCA<-as.character(df$PricingUCA)
    # # df$PricingUCA[df$PricingUCA!="UCA"&!is.na(df$PricingUCA)]<-
    # #   as.character(df$PricingFee[df$PricingUCA!="UCA"&!is.na(df$PricingUCA)])
    df$PricingUCA.sum<-as.character(df$Pricing.sum)
    df$PricingUCA.sum[df$PricingUCA=="UCA"|is.na(df$PricingUCA)]<-"Crosscutting"
    df$PricingUCA.sum<-factor(df$PricingUCA.sum)
  }


  if("informationtechnologycommercialitemcategory" %in% names(df)){
    df<-read_and_join_experiment(data=df
                                 ,"InformationTechnologyCommercialItemCategory.csv"
                                 ,path=path
                                 ,directory="productorservice/"
                                 ,by=c("informationtechnologycommercialitemcategory"="informationtechnologycommercialitemcategory"),
                                 lookup_char_as_factor = TRUE
                                 # ,new_var_checked=FALSE
                                 # ,create_lookup_rdata=TRUE
                                 # ,col_types="dddddddddccc"
    )
  }

  if("gfe_gfp_code" %in% names(df)){
    df$gfe_gfp_code<-factor(df$gfe_gfp_code)
    if(!"gfe_gfp_value" %in% names(df)){
      df$gfe_gfp_value<-factor(df$gfe_gfp_code)
      levels(df$gfe_gfp_value)<-list(
        "No Government Furnished"="N",
        "Transaction uses\nGovernment Furnished\nEquipment or Property"="Y"
      )
    }
  }


  #
  #
  #     df<-replace_nas_with_unlabeled(df,"Contracting.Agency.ID","Uncategorized")
  #
  #     df<-read_and_join_experiment(df,
  #                                      "Agency_AgencyID.csv",
  #                                      by=c("Contracting.Agency.ID"="AgencyID"),
  #                                      path=path,
  #                                      directory="")
  #
  #     # NA.check.df<-subset(df, is.na(Contracting.Agency.Name) , select=c("Contracting.Agency.ID"))
  #     # if(nrow(NA.check.df)>0){
  #     #   print(unique(NA.check.df))
  #     #   stop(paste(nrow(NA.check.df),"rows of NAs generated in Contracting.Agency.Name"))
  #     # }
  #
  #   }
  #   else if("Contracting.Department.ID" %in% names(df)){
  #
  #     if("Contracting.Agency.Name"%in% names(df)){
  #       df<-subset(df, select=-c(Contracting.Agency.Name))
  #     }
  #     if("Customer"%in% names(df)){
  #       df<-subset(df, select=-c(Customer))
  #     }
  #     if("SubCustomer"%in% names(df)){
  #       df<-subset(df, select=-c(SubCustomer))
  #     }
  #     names(df)[which(names(df)=="Contracting.Department.ID")]<-"Contracting.Agency.ID"
  #     #     stop("safety")
  #
  #     #     debug(read_and_join_experiment)
  #
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_Contracting_Agencies.csv",
  #                           by=c("Contracting.Agency.ID"))
  #     # NA.check.df<-subset(df, is.na(Contracting.Agency.Name), select=c("Fiscal_Year","Contracting.Agency.ID"))
  #     # if(nrow(NA.check.df)>0){
  #     #   print(unique(NA.check.df))
  #     #   stop(paste(nrow(NA.check.df),"rows of NAs generated in Contracting.Agency.Name"))
  #     # }
  #   }
  #
  #
  #
  #
  #   if("Funder" %in% names(df) && "SubFunder" %in% names(df)){
  #
  #     if("SubFunder.Sum"%in% names(df)){
  #       df<-subset(df, select=-c(SubFunder.Sum))
  #     }
  #
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_SubFunder.csv",
  #                           by=c("Funder","Subfunder"))
  #     # NA.check.df<-subset(df, is.na(SubFunder.Sum) & !is.na(Funder), select=c("Fiscal_Year","Funder","SubFunder"))
  #     # if(nrow(NA.check.df)>0){
  #     #   print(unique(NA.check.df))
  #     #   stop(paste(nrow(NA.check.df),"rows of NAs generated in SubFunder.Sum"))
  #     # }
  #   }
  #
  #
  #   df<-competition_vehicle_lookups(path,df)
  if("Action_Obligation"%in% names(df))
    df$Action_Obligation<-text_to_number(df$Action_Obligation)
  if("NumberOfActions" %in% colnames(df))
    df$NumberOfActions <- text_to_number(df$NumberOfActions)
  #
  #
  #   if("PoPstateCode" %in% names(df)){
  #
  #     if("StateText"%in% names(df)){
  #       df<-subset(df, select=-c(StateText))
  #     }
  #
  #     if("Census.Region"%in% names(df)){
  #       df<-subset(df, select=-c(Census.Region))
  #     }
  #
  #     if("BEA.Region"%in% names(df)){
  #       df<-subset(df, select=-c(BEA.Region))
  #     }
  #
  #     if("CSIS.Region"%in% names(df)){
  #       df<-subset(df, select=-c(CSIS.Region))
  #     }
  #
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_State_Code.csv")
  #     NA.check.df<-subset(df, is.na(StateText) & !is.na(PoPstateCode), select=c("PoPstateCode","StateText"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in StateText"))
  #     }
  #   }
  #
  #
  #
  #
  #   if(("OMBagencyCode" %in% names(df))
  #      #     &"OMBagencyName" %in% names(df)
  #      &"OMBbureauCode" %in% names(df)
  #      #     &"OMBbureauname" %in% names(df)
  #   )
  #   {
  #     #
  #     if("OMBagencyName" %in% names(df)){
  #       df<-subset(df, select=-c(OMBagencyName))
  #     }
  #     if("OMBbureauname"%in% names(df)){
  #       df<-subset(df, select=-c(OMBbureauname))
  #     }
  #     #   if("Customer"%in% names(df)){
  #     #     df<-subset(df, select=-c(Customer))
  #     #   }
  #     #   if("SubCustomer"%in% names(df)){
  #     #     df<-subset(df, select=-c(SubCustomer))
  #     #   }
  #     #       debug(read_and_join_experiment)
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_OMBagencyBureau.csv")
  #     NA.check.df<-subset(df
  #                         , is.na(CSISbureau) #& !is.na(OMBbureauCode)
  #                         , select=c("OMBagencyCode"
  #                                    ,"OMBagencyName"
  #                                    ,"OMBbureauCode"
  #                                    ,"OMBbureauname"
  #                                    ,"CSISbureau"
  #                         )
  #     )
  #
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in CSISbureau"))
  #     }
  #
  #   }
  #
  #If there's existing productorservce descriptions, these may be more precise than
  #What we can provide via lookup tables.
  #### PSC / NAICS / Project ####
  if("ProductOrServiceCode" %in% names(df) & !"Product.or.Service.Description" %in% names(df)&
     !"ProductOrServiceCodeText" %in% names(df))
  {
    if(is.integer(df$ProductOrServiceCode) | is.character(df$ProductOrServiceCode)){
      df$ProductOrServiceCode<-factor(df$ProductOrServiceCode)
    }
    df$ProductOrServiceCode[df$ProductOrServiceCode==""]<-NA

    df<-read_and_join_experiment(df,
                                 "ProductOrServiceCodes.csv",
                                 by=c("ProductOrServiceCode"="ProductOrServiceCode"),
                                 add_var=c("ProductOrServiceCodeText"),
                                 path=path,
                                 skip_check_var = c("ProductServiceOrRnDarea"),
                                 directory="",
                                 lookup_char_as_factor = TRUE
    )
  }
  #We can be more precise
  if("ProductOrServiceCode" %in% names(df) &
     !"ProductServiceOrRnDarea" %in% names(df) &
     "Fiscal_Year"  %in% names(df))
  {
    if(is.integer(df$ProductOrServiceCode)){
      df$ProductOrServiceCode<-factor(df$ProductOrServiceCode)
    }
    df$ProductOrServiceCode[df$ProductOrServiceCode %in% c("","0000")]<-NA

    df<-df %>% mutate(Fiscal_Year_gt_2020=if_else(Fiscal_Year>2020,1,0))
    df<-read_and_join_experiment(df,
                                 "PSCAtransition.csv",
                                 directory="ProductOrService/",
                                 by=c("ProductOrServiceCode"="ProductOrServiceCode",
                                      "Fiscal_Year_gt_2020"="Fiscal_Year_gt_2020"),
                                 add_var=c("ProductServiceOrRnDarea"),
                                 path=path,
                                 skip_check_var = c("ProductServiceOrRnDarea")
    )
    colnames(df)[colnames(df)=="ProductServiceOrRnDarea"]<-"TransitionProductServiceOrRnDarea"

    df<-read_and_join_experiment(df,
                                 "ProductOrServiceCodes.csv",
                                 by=c("ProductOrServiceCode"="ProductOrServiceCode"),
                                 add_var=c("Simple","ProductOrServiceArea","ProductServiceOrRnDarea"),
                                 path=path,
                                 skip_check_var = c("ProductServiceOrRnDarea"),
                                 directory=""

    )
    df$ProductServiceOrRnDarea[is.na(df$ProductServiceOrRnDarea)]<-
      df$TransitionProductServiceOrRnDarea[is.na(df$ProductServiceOrRnDarea)]

    df$ProductServiceOrRnDarea<-factor(df$ProductServiceOrRnDarea)
    df$Simple<-factor(df$Simple)
    df$ProductOrServiceArea<-factor(df$ProductOrServiceArea)

    #Manual check because we have to draw from two sources
    NA.check.df<-subset(df, is.na(ProductServiceOrRnDarea)&!is.na(ProductOrServiceCode),
                        select=c("ProductOrServiceCode","TransitionProductServiceOrRnDarea","ProductServiceOrRnDarea"))

    if(nrow(NA.check.df)>0){
      print(unique(NA.check.df))
      warning(paste(nrow(NA.check.df),"rows of NAs generated in ProductServiceOrRnDarea"))
    }

    df<-df %>% dplyr::select(-Fiscal_Year_gt_2020,-TransitionProductServiceOrRnDarea)

  }
  else if("ProductServiceOrRnDarea" %in% names(df))
  {
    df<-replace_nas_with_unlabeled(df,"ProductServiceOrRnDarea")

    if("ProductServiceOrRnDarea.sum" %in% names(df)){
      df<-subset(df, select=-c(ProductServiceOrRnDarea.sum))
    }
    if("SimpleArea" %in% names(df)){
      df<-subset(df, select=-c(SimpleArea))
    }
    if("Simple" %in% names(df)){
      df<-subset(df, select=-c(Simple))
    }
    # if("ServicesCategory.sum" %in% names(df)){
    #   df<-subset(df, select=-c(ServicesCategory.sum))
    # }
    # if("ProductOrServiceArea" %in% names(df)){
    #   df<-subset(df, select=-c(ProductOrServiceArea))
    # }
    # if("ServicesCategory.detail" %in% names(df)){
    #   df<-subset(df, select=-c(ServicesCategory.detail))
    # }
    #     debug(read_and_join_experiment)

    #Classify Product or Service Codes
    df<-read_and_join_experiment(df,
                                 "ProductServiceOrRnDarea.csv",
                                 # by="ProductOrServiceArea",
                                 by="ProductServiceOrRnDarea",
                                 replace_na_var="ProductServiceOrRnDarea",
                                 add_var=c("ProductServiceOrRnDarea.sum","ServicesCategory.detail","ServicesCategory.sum"),
                                 path=path,
                                 directory="productorservice/",
                                 lookup_char_as_factor = TRUE
    )


    NA.check.df<-subset(df, is.na(ServicesCategory.sum), select=c("ProductServiceOrRnDarea"))
    if(nrow(NA.check.df)>0){
      print(unique(NA.check.df))
      stop(paste(nrow(NA.check.df),"rows of NAs generated in ProductServiceOrRnDarea"))
    }
  }

  if(is.character(df$gfe_gfp_code)){
    df$gfe_gfp_code<-factor(df$gfe_gfp_code)
  }

  if(is.character(df$informationtechnologycommercialitemcategoryText)){
    df$informationtechnologycommercialitemcategoryText<-factor(df$informationtechnologycommercialitemcategoryText)
  }


  #   else if("ProductOrServiceArea" %in% names(df))
  #   {
  #     df<-replace_nas_with_unlabeled(df,"ProductOrServiceArea")
  #
  #     if("ServicesCategory.sum" %in% names(df)){
  #       df<-subset(df, select=-c(ServicesCategory.sum))
  #     }
  #     #     debug(read_and_join_experiment)
  #     colnames(df)[colnames(df)=="ProductOrServiceArea"]<-"ProductServiceOrRnDarea"
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_Buckets.csv",by="ProductServiceOrRnDarea")
  #     colnames(df)[colnames(df)=="ProductServiceOrRnDarea"]<-"ProductServiceOrRnDarea"
  #
  #     NA.check.df<-subset(df, is.na(ServicesCategory.sum), select=c("ProductOrServiceArea"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in ProductOrServiceArea"))
  #     }
  #
  #     NA.check.df<-subset(df, is.na(ServicesCategory.detail), select=c("ProductOrServiceArea"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in ServicesCategory.detail"))
  #     }
  #
  #   }
  #
  #   else if("ServicesCategory.detail" %in% names(df))
  #   {
  #
  #     if("ServicesCategory.sum" %in% names(df)){
  #       df<-subset(df, select=-c(ServicesCategory.sum))
  #     }
  #
  #     #     debug(read_and_join_experiment)
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_Buckets.csv")
  #     NA.check.df<-subset(df, is.na(ServicesCategory.sum), select=c("Fiscal_Year","ServicesCategory.detail"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in ServicesCategory.sum"))
  #     }
  #
  #   }
  #
  #
  #
  if(is.character(df$claimantprogramcode)){
    df$claimantprogramcode<-factor(df$claimantprogramcode)
  }

  if("PlatformPortfolio" %in% names(df)){
    #   {
    #
    #     if("PlatformPortfolio.sum" %in% names(df)){
    #       df<-subset(df, select=-c(PlatformPortfolio.sum))
    #     }
    #
    df<-replace_nas_with_unlabeled(df,"PlatformPortfolio")
    #
    #     df<-read_and_join_experiment(df,
    #                           "LOOKUP_PlatformPortfolio.csv")
    #     NA.check.df<-subset(df, is.na(PlatformPortfolio.sum), select=c("PlatformPortfolio"))
    #     if(nrow(NA.check.df)>0){
    #       print(unique(NA.check.df))
    #       stop(paste(nrow(NA.check.df),"rows of NAs generated in PlatformPortfolio.sum"))
    #     }

    if("IsEOsensor" %in% names(df)){
      df$IsEOsensor<-as.logical(df$IsEOsensor)
    }

    if("IsRemotelyOperated" %in% names(df)){
      df$PlatformPortfolioUAV<-as.character(df$PlatformPortfolio)
      df$IsRemotelyOperated<-as.logical(df$IsRemotelyOperated)
      df$PlatformPortfolioUAV[df$IsRemotelyOperated & !is.na(df$IsRemotelyOperated)]<-"Remotely Crewed"
      df$PlatformPortfolioUAV<-factor(df$PlatformPortfolioUAV)

    }
    else if ("ProductOrServiceCode" %in% names(df) & "ProjectID" %in% names(df)){

      df<-read_and_join_experiment(df,
                                   lookup_file="ProjectID.txt",
                                   path=path,directory="project/",
                                   add_var = c("IsRemotelyOperated"),
                                   by=c("ProjectID"),
                                   # missing_file="missing_iso.csv",
                                   skip_check_var = c("IsRemotelyOperated"))
      df$IsRemotelyOperated<-as.logical(df$IsRemotelyOperated)
      df$IsRemotelyOperated[df$ProductOrServiceCode==1550]<-T
      df$PlatformPortfolioUAV<-as.character(df$PlatformPortfolio)
      df$PlatformPortfolioUAV[df$IsRemotelyOperated & !is.na(df$IsRemotelyOperated)]<-"Remotely Crewed"
      df$PlatformPortfolioUAV<-factor(df$PlatformPortfolioUAV)
    }
  }


  #**** ProjectID *******
  if("ProjectID" %in% names(df)){


    if("Project.Name" %in% names(df)){
      df<-subset(df, select=-c(Project.Name))
    }

    df<-read_and_join_experiment(df,
                                 lookup_file="ProjectID.txt",
                                 path=path,directory="project/",
                                 add_var = c("ProjectName","IsUnknown","IsRemotelyOperated"),
                                 by=c("ProjectID"),
                                 # missing_file="missing_iso.csv",
                                 skip_check_var = c("IsRemotelyOperated"),
                                 lookup_char_as_factor = TRUE
    )


    if("IsRemotelyOperated" %in% colnames(df)){
      df$IsRemotelyOperated<-as.logical(df$IsRemotelyOperated)
    }

    if("IsUnknown" %in% colnames(df)){
      df$IsUnknown<-as.logical(df$IsUnknown)
    }


    if ("SubCustomer.platform" %in% names(df) & "ProjectName" %in% names(df)){
      df$SubCustomer.JPO<-as.character(df$SubCustomer.platform)
      df$SubCustomer.JPO[df$ProjectName %in% c("JSF (F-35) ","JSF (F-35)") & !is.na(df$ProjectName)&df$SubCustomer.platform=="Navy"]<-"F-35 JPO"
      df$SubCustomer.JPO<-factor(df$SubCustomer.JPO)
    }

  } else if ("SubCustomer.platform" %in% names(df) & "IsF35" %in% names(df)){
    df$SubCustomer.JPO<-as.character(df$SubCustomer.platform)

    df$IsF35<-as.logical(df$IsF35)
    df$SubCustomer.JPO[df$IsF35 & !is.na(df$IsF35)&df$SubCustomer.platform=="Navy"]<-"F-35 JPO"
    df$SubCustomer.JPO<-factor(df$SubCustomer.JPO)
  }


  #
  #
  #
  #   if("Arms.Type" %in% names(df)){
  #     #     debug(read_and_join_experiment)
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_ArmsType.csv")
  #
  #
  #     NA.check.df<-subset(df, is.na(Arms.Summary), select=c("Arms.Type","Arms.Summary")
  #     )
  #
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Arms.Type"))
  #     }
  #
  #   }
  #
  #   if("Country" %in% names(df)){
  #     #     debug(read_and_join_experiment)
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_Country.csv")
  #
  #
  #     NA.check.df<-subset(df, is.na(Country.Proper), select=c("Country","Country.Proper")
  #     )
  #
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Countries"))
  #     }
  #
  #   }
  #   else if("Destination.Country" %in% names(df)){
  #     #     debug(read_and_join_experiment)
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_Country.csv")
  #
  #
  #     NA.check.df<-subset(df, is.na(Country.Proper), select=c("Destination.Country","Country.Proper")
  #     )
  #
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Countries"))
  #     }
  #   }
  #   #     browser()
  #   if("Pricing.Mechanism" %in% names(df)){
  #     df$Pricing.Mechanism[df$Pricing.Mechanism==""]<-NA
  #
  #     df<-replace_nas_with_unlabeled(df,"Pricing.Mechanism")
  #
  #     if("Pricing.Mechanism.sum" %in% names(df)){
  #       df<-subset(df, select=-c(Pricing.Mechanism.sum))
  #     }
  #
  #     if("Pricing.Mechanism.detail" %in% names(df)){
  #       df<-subset(df, select=-c(Pricing.Mechanism.detail))
  #     }
  #
  #     if("Pricing.Mechanism.Correlation" %in% names(df)){
  #       df<-subset(df, select=-c(Pricing.Mechanism.Correlation))
  #     }
  #
  #     if("Pricing.Mechanism.Graph" %in% names(df)){
  #       df<-subset(df, select=-c(Pricing.Mechanism.Graph))
  #     }
  #
  #     #     stop("hammertiime")
  #
  #
  #     df<-read_and_join_experiment(df,
  #                           "LOOKUP_Pricing_Mechanism.csv",
  #                           by=c("Pricing.Mechanism"),
  #                           skip_check_var=c("IsCostBased","Pricing.Mechanism.Code","IsFixedPrice",	"IsIncentive"),
  #                           replace_na_var=("Pricing.Mechanism")
  #     )
  #   }
  #   #   else if ("Pricing.Mechanism.Code" %in% names(df)){
  #   #     #Replace blank strings with Unlabeled
  #   # #     df$Pricing.Mechanism<-mapvalues(df$Pricing.Mechanism,from=c(""),to=c("Unlabeled"))
  #   # #
  #   # #     #Handle NA values if present
  #   # #     if(any(is.na(df$Pricing.Mechanism))){
  #   # #       #Make sure unlabeled is within the list of levels
  #   # #       if (!("Unlabeled" %in% levels(df$Pricing.Mechanism))){
  #   # #         df$Pricing.Mechanism=factor(df$Pricing.Mechanism,levels=c(unique(df$Pricing.Mechanism),"Unlabeled"))
  #   # #       }
  #   # #     }
  #   # #
  #   # #     #Replace NAs with Uncategorized
  #   # #     df$Pricing.Mechanism[is.na(df$Pricing.Mechanism)]<-"Unlabeled"
  #   # #
  #   #
  #   #
  #   #     if("IsCostBased" %in% names(df) && (!any(!is.na(df$IsCostBased)))){
  #   #       df<-subset(df, select=-c(IsCostBased))
  #   #     }
  #   #
  #   #     if("IsFixedPrice" %in% names(df) && all(is.na(df$IsFixedPrice))){
  #   #       df<-subset(df, select=-c(IsFixedPrice))
  #   #     }
  #   #
  #   #
  #   #     if("IsIncentive" %in% names(df) && (!any(!is.na(df$IsIncentive)))){
  #   #       df<-subset(df, select=-c(IsIncentive))
  #   #     }
  #   #
  #   #     if("Pricing.Mechanism.sum" %in% names(df)){
  #   #       df<-subset(df, select=-c(Pricing.Mechanism.sum))
  #   #     }
  #   #
  #   #     if("Pricing.Mechanism.detail" %in% names(df)){
  #   #       df<-subset(df, select=-c(Pricing.Mechanism.detail))
  #   #     }
  #   #
  #   #     if("Pricing.Mechanism.Correlation" %in% names(df)){
  #   #       df<-subset(df, select=-c(Pricing.Mechanism.Correlation))
  #   #     }
  #   #
  #   #     if("Pricing.Mechanism.Graph" %in% names(df)){
  #   #       df<-subset(df, select=-c(Pricing.Mechanism.Graph))
  #   #     }
  #   #
  #   #     #     stop("hammertiime")
  #   #
  #   #
  #   #
  #   #     df<-read_and_join_experiment(df,"LOOKUP_Pricing_Mechanism.csv")
  #   #
  #   #
  #   #     NA.check.df<-subset(df, !is.na(Pricing.Mechanism.Code) & is.na(Pricing.Mechanism.sum), select=c("Pricing.Mechanism"))
  #   #     if(nrow(NA.check.df)>0){
  #   #       print(unique(NA.check.df))
  #   #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Pricing.Mechanism.sum"))
  #   #     }
  #   #
  #   #     NA.check.df<-subset(df, is.na(Pricing.Mechanism.detail), select=c("Pricing.Mechanism"))
  #   #     if(nrow(NA.check.df)>0){
  #   #       print(unique(NA.check.df))
  #   #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Pricing.Mechanism.detail"))
  #   #     }
  #   #   }
  #
  #
  #   if("Contractor.Size" %in% names(df)){
  #     df<-read_and_join_experiment(df,"LOOKUP_Contractor_Size_named.csv")
  #
  #     NA.check.df<-subset(df, is.na(Contractor.Size.detail), select=c("Contractor.Size"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Contractor.Size.detail"))
  #     }
  #
  #     NA.check.df<-subset(df, is.na(Contractor.Size.sum), select=c("Contractor.Size"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Contractor.Size.sum"))
  #     }
  #
  #     NA.check.df<-subset(df, is.na(Contractor.Size.correlation), select=c("Contractor.Size"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Contractor.Size.correlation"))
  #     }
  #
  #   }
  #
  #

  if("principalnaicscode" %in% names(df))
  {
    df$principalnaicscode[df$principalnaicscode==""]<-NA
    if(any(is.na(text_to_number(df$principalnaicscode))&!is.na(df$principalnaicscode)))
      stop(paste("Text in principalnaicscode:",
                 levels(factor(df$principalnaicscode[is.na(text_to_number(df$principalnaicscode))&!is.na(df$principalnaicscode)]))))
    df$principalnaicscode<-text_to_number(df$principalnaicscode)

    df<-read_and_join_experiment(df,
                                 "Lookup_PrincipalNAICScode.csv",
                                 by=c("principalnaicscode"="principalnaicscode"),
                                 add_var=c("principalnaicscodeText"),
                                 path=path,
                                 skip_check_var = c("principalnaicscodeText"),
                                 directory="economic",
                                 lookup_char_as_factor = TRUE
    )
  }
  #### Budget #####
  if("MainAccountTitle" %in% colnames(df)){
    df$MainAccountTitle<-as.factor(df$MainAccountTitle)
  }
  if("CrisisFunding" %in% colnames(df)){
    df$CrisisFunding<-as.factor(df$CrisisFunding)
  }

  if("foreign_funding_description" %in% colnames(df) &
     "IsFMSml" %in% colnames(df) &
     "IsFMSmac" %in% colnames(df)){
    df$foreign_funding_description<-as.factor(df$foreign_funding_description)
    df$IsFMSml<-as.logical(df$IsFMSml)
    df$IsFMSmac<-as.logical(df$IsFMSmac)
    df$IsFMS<-as.logical(NA)
    df$IsFMS[df$foreign_funding_description %in% c("Foreign Funds FMS")]<-T
    df$IsFMS[df$foreign_funding_description %in% c("Foreign Funds non-FMS", "Not Applicable")]<-F
    df$IsFMS[is.na(df$IsFMS)]<-df$IsFMSml[is.na(df$IsFMS)]
    df$IsFMS[is.na(df$IsFMS) & df$IsFMSmac]<-T
    df$IsFMS[is.na(df$IsFMS) & df$IsFMSmac==F]<-F
    if("mainaccountcode" %in% colnames(df) & "treasuryagencycode" %in% colnames(df)){
      df$IsUnlabeledMAC<-as.logical(is.na(df$mainaccountcode) | is.na(df$treasuryagencycode))
      df$IsFMS[is.na(df$IsFMS) & df$IsUnlabeledMAC]<-F
    }
  } else if ("IsFMS" %in% colnames(df)){
    df$IsFMS<-as.logical(df$IsFMS)
  }


  #### Vendor Size and Location####
  if("VendorSize" %in% names(df)){
    df<-replace_nas_with_unlabeled(df,"VendorSize")

    df<-read_and_join_experiment(df,
                                 "VendorSize.csv",
                                 by=c("VendorSize"="VendorSize"),
                                 add_var="Shiny.VendorSize",
                                 path=path,
                                 directory="vendor/",
                                 lookup_char_as_factor = TRUE
    )

    # df<-read_and_join_experiment(df,"LOOKUP_Contractor_Size.csv")
    #
    # NA.check.df<-subset(df, is.na(Vendor.Size.detail), select=c("Vendor.Size"))
    # if(nrow(NA.check.df)>0){
    #   print(unique(NA.check.df))
    #   stop(paste(nrow(NA.check.df),"rows of NAs generated in Vendor.Size.detail"))
    # }
    #
    # NA.check.df<-subset(df, is.na(Vendor.Size.sum), select=c("Vendor.Size"))
    # if(nrow(NA.check.df)>0){
    #   print(unique(NA.check.df))
    #   stop(paste(nrow(NA.check.df),"rows of NAs generated in Vendor.Size.sum"))
    # }

  }

  #
  #
  #   if("Contract.Size" %in% names(df)){
  #     df<-replace_nas_with_unlabeled(df,"Contract.Size")
  #
  #     df<-read_and_join_experiment(df,"LOOKUP_Contract_Size.csv")
  #
  #     NA.check.df<-subset(df, is.na(Contract.Size.detail), select=c("Contract.Size"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Contract.Size.detail"))
  #     }
  #
  #     NA.check.df<-subset(df, is.na(Contract.Size.sum), select=c("Contract.Size"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Contract.Size.sum"))
  #     }
  #
  #   }
  #
  #
  #   if("Extent.Competed" %in% names(df)){
  #     df<-read_and_join_experiment(df,"LOOKUP_Extent_Competed.csv")
  #
  #     NA.check.df<-subset(df, is.na(Extent.Competed.Sum), select=c("Extent.Competed.Sum"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Extent.Competed.Sum"))
  #     }
  #   }
  #
  #   if("systemequipmentcode" %in% names(df)){
  #     df<-replace_nas_with_unlabeled(df,"systemequipmentcode")
  #
  #     df<-read_and_join_experiment(df,"LOOKUP_systemequipmentcode.csv")
  #
  #     NA.check.df<-subset(df, is.na(systemequipmentcode)|is.na(systemequipmentshorttext), select=c("systemequipmentcode","systemequipmentcodeText","systemequipmentshorttext"))
  #     if(nrow(NA.check.df)>0){
  #       print(unique(NA.check.df))
  #       stop(paste(nrow(NA.check.df),"rows of NAs generated in systemequipmentcodeText or systemequipmentshorttext"))
  #     }
  #   }
  #
  #

  if("IsEntityTraditional" %in% names(df)){
    df$IsEntityTraditional<-as.logical(df$IsEntityTraditional)
  }

  if("EntitySizeCode" %in% names(df)){

    # EntitySizeText.detail = EntitySizeText,
    # EntitySizeText = fct_recode(
    #   EntitySizeText,
    #   Small = "Always Small Vendor",
    #   Small = "Sometimes Small Vendor",
    #   Medium = "Medium Vendor",
    #   "Large+" = "Big Five",
    #   "Large+" = "Large Vendor",
    #   "Large+" = "Large: Big 5 JV"))%>%

    df<-read_and_join_experiment(df,
                                 "EntitySizeCode.csv",
                                 by=c("EntitySizeCode"="EntitySizeCode"),
                                 add_var=c("EntitySizeText","EntitySmall","EntitySizeText.sum"),
                                 path=path,
                                 directory="vendor/",
                                 lookup_char_as_factor = TRUE)
    df<-replace_nas_with_unlabeled(df,"EntitySizeText",replacement="Unlabeled Vendor")
    df<-replace_nas_with_unlabeled(df,"EntitySizeText.sum",replacement="Unlabeled Vendor")
    df<-replace_nas_with_unlabeled(df,"EntitySmall",replacement="Unlabeled Vendor")
  }



  #Deflators
  if("Fiscal_Year"%in% names(df)){
    if("Action_Obligation"%in% names(df)){
      df<-deflate(df,
                  money_var = "Action_Obligation",
                  fy_var="Fiscal_Year",
                  deflator_var=deflator_var,
                  path=path
      )
    }
  }

  if("fundedbyforeignentity" %in% colnames(df) &
     !"foreign_funding_description" %in% colnames(df)){
    df$fundedbyforeignentity[df$fundedbyforeignentity==""]<-NA
    if(is.character(df$fundedbyforeignentity)){
      df$fundedbyforeignentity<-factor(df$fundedbyforeignentity)
    }
    df<-   read_and_join_experiment(df,lookup_file="Budget_FundedByForeignEntity.csv",
                                    path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",directory="budget/",
                                    add_var = c("foreign_funding_description"),
                                    by=c("fundedbyforeignentity"),
                                    lookup_char_as_factor = TRUE
                                    # missing_file="missing_iso.csv",
                                    # skip_check_var = "territory_capital"
    )
  }


  if("PlaceOfManufacture" %in% colnames(df)){
    #Place of manufacture
    df$PlaceOfManufacture<-as.factor(df$PlaceOfManufacture)
    df<-read_and_join_experiment(df,
                                 "Location_PlaceOfManufacture.csv",
                                 by="PlaceOfManufacture",
                                 add_var=c("PlaceOfManufactureText","PlaceOfManufacture_Sum","PlaceOfManufacture_DoD",
                                           "MFGorPerformIsForeign","MFGisForeign"),
                                 skip_check_var = c("PlaceOfManufactureText","PlaceOfManufacture_Sum","PlaceOfManufacture_DoD",
                                                    "MFGorPerformIsForeign","MFGisForeign"),
                                 path=path,
                                 directory="location/",
                                 case_sensitive = FALSE,
                                 lookup_char_as_factor = TRUE
    )
  }




  if("MFGorPerformIsForeign" %in% colnames(df)){
    df$MFGorPerformIsForeign<-as.logical(df$MFGorPerformIsForeign)
  }
  if("MFGisForeign" %in% colnames(df)){
    df$MFGisForeign<-as.logical(df$MFGisForeign)
  }



  if("ManufacturingOrganizationType" %in% colnames(df)){
    #ManufacturingOrganizationType
    if(is.character(df$ManufacturingOrganizationType)){
      df$ManufacturingOrganizationType<-factor(df$ManufacturingOrganizationType)
    }
    df<-read_and_join_experiment(df,
                                 "ManufacturingOrganizationType.csv",
                                 by="ManufacturingOrganizationType",
                                 add_var=c("ManufacturingOrganizationText","ManufacturingOrganizationText_sum",
                                           "ManufacturingOrganizationParentHQisForeign"),
                                 skip_check_var = c("ManufacturingOrganizationText","ManufacturingOrganizationText_sum",
                                                    "ManufacturingOrganizationParentHQisForeign"),
                                 path="offline",
                                 directory="location/",
                                 case_sensitive = FALSE,
                                 lookup_char_as_factor = TRUE
    )
  }


  if("multiyearcontract" %in% colnames(df)){
    df$multiyearcontract<-as.logical(df$multiyearcontract)
  }

  if("ManufacturingOrganizationParentHQisForeign" %in% colnames(df)){
    df$ManufacturingOrganizationParentHQisForeign<-as.logical(df$ManufacturingOrganizationParentHQisForeign)
  }


  if("PrincipalPlaceofPerformanceCountryCode" %in% colnames(df) & !"PlaceISOalpha3" %in% colnames(df)){
    if("ISOalpha3" %in% colnames(df))
      df<-subset(df,select=-c(ISOalpha3))
    df<-read_and_join_experiment(df,lookup_file="Location_CountryCodes.csv",
                                 path=path,directory="location/",
                                 add_var = c("ISOalpha3"),#"USAIDregion",
                                 by=c("PrincipalPlaceofPerformanceCountryCode"="alpha-3"),
                                 # skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                 missing_file="missing_DSCA_iso.csv",
                                 lookup_char_as_factor = TRUE)
    colnames(df)[colnames(df)=="PlaceISOalpha3"]<-"PlaceISOalpha3"
  }

  if("PlaceISOalpha3" %in% colnames(df)){
    if(is.character(df$PlaceISOalpha3)){
      df$PlaceISOalpha3<-factor(df$PlaceISOalpha3)
    }
    if("PlaceIsForeign" %in% colnames(df))
      df<-subset(df,select=-c(PlaceIsForeign))

    if(call_add_alliance)
      df <- df %>% add_alliance(ISOalpha3_col= "PlaceISOalpha3", drop_col = TRUE,prefix="Place")
    else{
      df<-read_and_join_experiment(df,lookup_file="Location_CountryCodes.csv",
                                   path=path,directory="location/",
                                   add_var = c("isforeign"),#"USAIDregion",
                                   by=c("PlaceISOalpha3"="alpha-3"),
                                   # skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                   missing_file="missing_DSCA_iso.csv",
                                   lookup_char_as_factor = TRUE)
      colnames(df)[colnames(df)=="isforeign"]<-"PlaceIsForeign"
    }
  }
  if ("PlaceIsForeign" %in% colnames(df))
    df$PlaceIsForeign<-as.logical(df$PlaceIsForeign)

  if(is.character(df$PlaceStateCode)){
    df$PlaceStateCode<-factor(df$PlaceStateCode)
  }
  if(is.character(df$pop_state_code)){
    df$pop_state_code<-factor(df$pop_state_code)
  }


  if("OriginISOalpha3" %in% colnames(df)){
    if("OriginIsForeign" %in% colnames(df))
      df<-subset(df,select=-c(OriginIsForeign))
    if(is.character(df$OriginISOalpha3)){
      df$OriginISOalpha3<-factor(df$OriginISOalpha3)
    }
    if(call_add_alliance){
      df$OriginISOalpha3[df$OriginISOalpha3=="~NJ"]<-NA
      df <- df %>% add_alliance(ISOalpha3_col= "OriginISOalpha3", drop_col = TRUE,prefix="Origin")
    }
    else{
      df<-read_and_join_experiment(df,lookup_file="Location_CountryCodes.csv",
                                   path=path,directory="location/",
                                   add_var = c("isforeign"),#"USAIDregion",
                                   by=c("OriginISOalpha3"="alpha-3"),
                                   # skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                   missing_file="missing_DSCA_iso.csv",
                                   lookup_char_as_factor = TRUE)
      colnames(df)[colnames(df)=="isforeign"]<-"OriginIsForeign"
    }
    # if("MFGorPerformIsForeign" %in% colnames(df)){
    #   df <- df %>% mutate(MFGorPerformIsForeign=case_when(
    #     MFGorPerformIsForeign==1~1,
    #     OriginIsForeign=1~1,
    #     TRUE~MFGorPerformIsForeign
    #   ))
    # }
    # if("MFGisForeign" %in% colnames(df)){
    #   df <- mutate(MFGisForeign=case_when(
    #     ~is.na(MFGisForeign)==1~1,
    #     OriginIsForeign=1~1,
    #     TRUE~MFGorPerformIsForeign
    #   ))
    # }
  }
  if("OriginIsForeign" %in% colnames(df)){
    df$OriginIsForeign<-as.logical(df$OriginIsForeign)
  }

  if("VendorISOalpha3" %in% colnames(df)){
    if("VendorIsForeign" %in% colnames(df))
      df<-subset(df,select=-c(VendorIsForeign))

    df$VendorISOalpha3[df$VendorISOalpha3=="~NJ"]<-NA


    if(is.character(df$VendorISOalpha3)){
      df$VendorISOalpha3<-factor(df$VendorISOalpha3)
    }

    if(call_add_alliance){
      df <- df %>% add_alliance(ISOalpha3_col= "VendorISOalpha3", drop_col = TRUE,prefix="Vendor")
    } else {
      df<-read_and_join_experiment(df,lookup_file="Location_CountryCodes.csv",
                                   path=path,directory="location/",
                                   add_var = c("isforeign"),#"USAIDregion",
                                   by=c("VendorISOalpha3"="alpha-3"),
                                   # skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                   missing_file="missing_DSCA_iso.csv",
                                   lookup_char_as_factor = TRUE)
      colnames(df)[colnames(df)=="isforeign"]<-"VendorIsForeign"
    }
    # if("ParentHQisForeign" %in% colnames(df)){
    #   df <- mutate(ParentHQisForeign=case_when(
    #     ParentHQisForeign==1~1,
    #     VendorIsForeign==1~1,
    #     TRUE~ParentHQisForeign
    #   ))
    # }
  }
  if("VendorIsForeign" %in% colnames(df)){
    df$VendorIsForeign<-as.logical(df$VendorIsForeign)
  }
  if(is.character(df$VendorStateCode)){
    df$VendorStateCode<-factor(df$VendorStateCode)
  }



  if("VendorAddressCountry" %in% colnames(df) & !"VendorAddressISOalpha3" %in% colnames(df)){
    if("ISOalpha3" %in% colnames(df))
      df<-subset(df,select=-c(ISOalpha3))
    df$VendorAddressCountry[df$VendorAddressCountry==""]<-NA
    df<-read_and_join_experiment(df,lookup_file="Location_CountryName.csv",
                                 path=path,directory="location/",
                                 add_var = c("ISOalpha3"),
                                 by=c("VendorAddressCountry"="CountryName"),
                                 # skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                 missing_file="missing_VendorAddressCountry.csv",
                                 case_sensitive = FALSE,
                                 lookup_char_as_factor = TRUE)
    colnames(df)[colnames(df)=="ISOalpha3"]<-"VendorAddressISOalpha3"
  }


  if("VendorAddressISOalpha3" %in% colnames(df)){
    if("VendorAddressIsForeign" %in% colnames(df))
      df<-subset(df,select=-c(VendorIsForeign))
    if(call_add_alliance){
      df <- df %>% add_alliance(ISOalpha3_col= "VendorAddressISOalpha3", drop_col = TRUE,prefix="VendorAddress")
    } else {
      df<-read_and_join_experiment(df,lookup_file="Location_CountryCodes.csv",
                                   path=path,directory="location/",
                                   add_var = c("isforeign"),#"USAIDregion",
                                   by=c("VendorAddressISOalpha3"="alpha-3"),
                                   # skip_check_var=c("NATOyear",	"MajorNonNATOentryYear","MajorNonNATOexitYear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                   missing_file="missing_iso.csv",
                                   lookup_char_as_factor = TRUE)
      colnames(df)[colnames(df)=="isforeign"]<-"VendorAddressIsForeign"
    }
    # if("ParentHQisForeign" %in% colnames(df)){
    #   df <- mutate(MFGorPerformIsForeign=case_when(
    #     ParentHQisForeign==1~1,
    #     VendorAddressIsForeign==1~1,
    #     TRUE~ParentHQisForeign
    #   ))
    #}
  }
  if("VendorAddressIsForeign" %in% colnames(df)){
    df$VendorAddressIsForeign<-as.logical(df$VendorAddressIsForeign)
  }


  if ("Shiny.VendorSize" %in% colnames(df) & "VendorIsForeign" %in% colnames(df)){
    df$VendorSize_Intl<-factor(df$Shiny.VendorSize)
    levels(df$VendorSize_Intl)<-list(
      "Unlabeled"="Unlabeled",
      "International"="International",
      "U.S. Big Five"=c("Big Five","U.S. Big Five"),
      "U.S. Large"=c("Large","U.S. Large"),
      "U.S. Medium"=c("Medium","U.S. Medium"),
      "U.S. Small"=c("Small","U.S. Small")
    )
    df$VendorSize_Intl[df$VendorIsForeign==1]<-"International"
    df$VendorSize_Intl[is.na(df$VendorIsForeign)]<-"Unlabeled"

  }

  if("PricingUCA" %in% colnames(df) & "PricingInflation" %in% colnames(df)){
    df$PricingInflationUCA<-as.character(df$PricingInflation)
    df$PricingInflationUCA[df$PricingUCA=="UCA"]<-"UCA"
    df$PricingInflationUCA<-as.factor(df$PricingInflationUCA)
  }

  #### Duration ####
  if("CurrentDurationCategory" %in% colnames(df)){
    df$CurrentDurationCategory<-factor(df$CurrentDurationCategory)
    df$CurrentDurationIsYear<-df$CurrentDurationCategory
    levels(df$CurrentDurationIsYear)<-list(
      "<=1 year" =c("<=2 Months", ">2-7 Months"  ,">7-12 Months"),
      ">1 year"=c(">1-2 Years",   ">2-4 Years",  ">4 years")
    )
    if("PricingInflation" %in% colnames(df)){
      df$PricingInflation.1year<-as.character(df$PricingInflation)
      df$PricingInflation.1year[df$CurrentDurationIsYear=="<=1 year"]<-"<=1 Year (All Types)"
      df$PricingInflation.1year<-as.factor(df$PricingInflation.1year)
    }

    if("PricingInflationUCA" %in% colnames(df)){
      df$PricingInflation.1yearUCA<-as.character(df$PricingInflationUCA)
      df$PricingInflation.1yearUCA[df$CurrentDurationIsYear=="<=1 year"]<-"<=1 Year (All Types)"
      df$PricingInflation.1yearUCA<-as.factor(df$PricingInflation.1yearUCA)
    }

    if("PricingUCA.sum" %in% colnames(df)){
      df$PricingUCA.1year<-as.character(df$PricingUCA.sum)
      df$PricingUCA.1year[df$CurrentDurationIsYear=="<=1 year"]<-"<=1 Year (All Types)"
      df$PricingUCA.1year<-as.factor(df$PricingUCA.1year)
    }
  }


  if("UnmodifiedUltimateDurationCategory" %in% colnames(df)){
    df$UnmodifiedUltimateDurationCategory<-factor(df$UnmodifiedUltimateDurationCategory)
    df$UnmodifiedUltimateDurationIsYear<-factor(df$UnmodifiedUltimateDurationCategory)
    levels(df$UnmodifiedUltimateDurationIsYear)<-list(
      "<=1 year" =c("<=2 Months", ">2-7 Months"  ,">7-12 Months"),
      ">1 year"=c(">1-2 Years",   ">2-4 Years",  ">4 years")
    )
  }

  if("recoveredmaterialclauses" %in% colnames(df)){
    df$recoveredmaterialclauses[df$recoveredmaterialclauses==""]<-NA
    df$recoveredmaterialclauses<-as.factor(df$recoveredmaterialclauses)
  }


  if("multiyearcontract" %in% colnames(df)){
    df$multiyearcontract<-as.logical(df$multiyearcontract)
  }


  #
  #   if("Fiscal_Year"%in% names(df)){
  #     df<-read_and_join_experiment(df,
  #                           "Lookup_Deflators.csv",
  #                           by="Fiscal_Year",
  #                           new_var_checked=FALSE,
  #                           path=path,
  #                           directory="economic/")
  #     # NA.check.df<-subset(df,  is.na(Deflator.2014) & is.na(Deflator.2013) & !is.na(Fiscal_Year), select=c("Fiscal_Year","Deflator.2013","Deflator.2014"))
  #     # if(nrow(NA.check.df)>0){
  #     #   print(unique(NA.check.df))
  #     #   stop(paste(nrow(NA.check.df),"rows of NAs generated in value"))
  #     # }
  #
  #
  #     if("Action_Obligation"%in% names(df)){
  #       df$Action_Obligation<-text_to_number(df$Action_Obligation)
  #       if("Deflator.2013"%in% names(df)){
  #         df$Obligation.2013<-df$Action_Obligation/df$Deflator.2013/1000000000
  #       }
  #       if("Deflator.2014"%in% names(df)){
  #         df$Obligation.2014<-df$Action_Obligation/df$Deflator.2014/1000000000
  #       }
  #       if("Deflator.2015"%in% names(df)){
  #         df$Obligation.2015<-df$Action_Obligation/df$Deflator.2015/1000000000
  #       }
  #
  #       if("Deflator.2016"%in% names(df)){
  #         df$Obligation.2016<-df$Action_Obligation/df$Deflator.2016/1000000000
  #       }
  #     }
  #
  #
  #
  #     if("GBKdisbursements"%in% names(df)){
  #       df$GBKdisbursements<-text_to_number(df$GBKdisbursements)
  #       if("Deflator.2013"%in% names(df)){
  #         df$GBKdisbursements.ConstantB<-df$GBKdisbursements/df$Deflator.2013/1000000000
  #       }
  #     }
  #
  #     if("GBKobligations"%in% names(df)){
  #       df$SumOfObligations<-text_to_number(df$GBKobligations)
  #       if("Deflator.2013"%in% names(df)){
  #         df$GBKobligations.2013<-df$GBKobligations/df$Deflator.2013/1000000000
  #       }
  #     }
  #
  #     if("Outlay"%in% names(df)){
  #       df$Outlay<-text_to_number(df$Outlay)
  #       if("Deflator.2013"%in% names(df)){
  #         df$Outlay.2013<-df$Outlay/df$Deflator.2013/1000000000
  #       }
  #     }
  #
  #
  #     if("OutlayNoOffsetAccount"%in% names(df)){
  #       df$OutlayNoOffsetAccount<-text_to_number(df$OutlayNoOffsetAccount)
  #       if("Deflator.2013"%in% names(df)){
  #         df$OutlayNoOffsetAccount.2013<-df$OutlayNoOffsetAccount/df$Deflator.2013/1000000000
  #       }
  #     }
  #
  #     if("OutlayOffsetAccount"%in% names(df)){
  #       df$OutlayOffsetAccount<-text_to_number(df$OutlayOffsetAccount)
  #       if("Deflator.2013"%in% names(df)){
  #         df$OutlayOffsetAccount.2013<-df$OutlayOffsetAccount/df$Deflator.2013/1000000000
  #       }
  #     }
  #
  #
  #
  #     if("Fed_Grant_Funding_Amount"%in% names(df)){
  #       df$Fed_Grant_Funding_Amount	   <-text_to_number(df$Fed_Grant_Funding_Amount	   )
  #       if("Deflator.2013"%in% names(df)){
  #         df$Fed_Grant_Funding_Amount.2013<-df$Fed_Grant_Funding_Amount	   /df$Deflator.2013/1000000000
  #       }
  #     }
  #
  #
  #     if("ContractObligatedAmount"%in% names(df)){
  #       df$ContractObligatedAmount<-text_to_number(df$ContractObligatedAmount)
  #       if("Deflator.2013"%in% names(df)){
  #         df$ContractObligatedAmount.2013<-df$ContractObligatedAmount/df$Deflator.2013/1000000000
  #       }
  #     }
  #   }
  #   if("OutlayNoOffsetAccount.2013" %in% names(df)
  #      & "OutlayOffsetAccount.2013" %in% names(df)
  #      & "ContractObligatedAmount.2013" %in% names(df)
  #      #      & "Fed_Grant_Funding_Amount.2013" %in% names(df)
  #      & "Outlay.2013" %in% names(df)
  #   ){
  #
  #
  #     df$ResidualOutlay.2013<-if_else(is.na(df$OutlayNoOffsetAccount.2013)
  #                                        ,0
  #                                        ,df$OutlayNoOffsetAccount.2013
  #     )
  #
  #     df$ResidualOutlay.2013<-df$ResidualOutlay.2013-if_else(is.na(df$ContractObligatedAmount.2013)
  #                                                                   ,0
  #                                                                   ,df$ContractObligatedAmount.2013
  #     )
  #
  #
  #     Measurement.Vars.List=c("OutlayNoOffsetAccount.2013"
  #                             ,"OutlayOffsetAccount.2013"
  #                             ,"ContractObligatedAmount.2013"
  #                             #                             ,"Fed_Grant_Funding_Amount.2013"
  #                             ,"Outlay.2013"
  #                             ,"ResidualOutlay.2013"
  #     )
  #
  #     if("Fed_Grant_Funding_Amount.2013" %in% names(df))
  #     {
  #
  #       df$ResidualOutlay.2013<-df$ResidualOutlay.2013-if_else(is.na(df$Fed_Grant_Funding_Amount.2013)
  #                                                                     ,0
  #                                                                     ,df$Fed_Grant_Funding_Amount.2013
  #       )
  #
  #       Measurement.Vars.List<-rbind(Measurement.Vars.List,"Fed_Grant_Funding_Amount.2013")
  #
  #
  #     }
  #
  #
  #
  #
  #     df<-melt(df,
  #                  #                        id=c("Fiscal_Year"
  #                  #                                                  ,"SubFunder.Detail")
  #                  measure.vars=Measurement.Vars.List,
  #                  variable.name="comparison.dollar.type")
  #
  #
  #
  #     df<-read_and_join_experiment(
  #       df,
  #       "LOOKUP_comparison_dollar_type.csv"
  #     )
  #
  #   }
  #
  #   if("SimpleArea" %in% names(df))
  #   {
  #     df$SimpleArea[df$SimpleArea==""]<-NA
  #     df$SimpleArea<-factor(df$SimpleArea
  #                               ,exclude=NULL
  #                               ,levels = c("Products"
  #                                           ,"Services"
  #                                           ,"R&D"
  #                                           ,NA)
  #                               ,labels = c("Products"
  #                                           ,"Services"
  #                                           ,"R&D"
  #                                           ,"Mixed or Unlabeled")
  #     )
  #   }
  #
  #   if("IsTerminated" %in% names(df))
  #   {
  #     levels(df$IsTerminated)<-list("Unterminated"=c("Unterminated","0",0),
  #                                       "Terminated"=c("Terminated","1",1)
  #     )
  #   }
  #
  #   if("IsClosed" %in% names(df))
  #   {
  #     levels(df$IsClosed)<-list("Unspecified"=c("Unspecified","0",0),
  #                                   "Closed"=c("Closed","1",1)
  #     )
  #
  #     #     addNA(df$IsClosed, ifany = TRUE)
  #   }
  #   if("numberofoffersreceived" %in% names(df)){
  #     df$numberofoffersreceived[df$numberofoffersreceived==0]<-NA
  #   }
  #
  #   if("UnmodifiedNumberOfOffersReceived" %in% names(df))
  #   {
  #     df$UnmodifiedNumberOfOffersReceived<-text_to_number(df$UnmodifiedNumberOfOffersReceived)
  #     df$UnmodifiedNumberOfOffersReceived[df$UnmodifiedNumberOfOffersReceived==0]<-NA
  #     if("numberofoffersreceived" %in% names(df)){
  #       df$UnmodifiedNumberOfOffersReceived[is.na(df$UnmodifiedNumberOfOffersReceived)]<-df$numberofoffersreceived[is.na(df$UnmodifiedNumberOfOffersReceived)]
  #     }
  #
  #     df$UnmodifiedNumberOfOffersSummary[is.na(df$UnmodifiedNumberOfOffersReceived)]<-NA
  #     df$UnmodifiedNumberOfOffersSummary[df$UnmodifiedNumberOfOffersReceived==1]<-1
  #     df$UnmodifiedNumberOfOffersSummary[df$UnmodifiedNumberOfOffersReceived==2]<-2
  #     df$UnmodifiedNumberOfOffersSummary[df$UnmodifiedNumberOfOffersReceived==3 | df$UnmodifiedNumberOfOffersReceived==4]<-3.5
  #     df$UnmodifiedNumberOfOffersSummary[df$UnmodifiedNumberOfOffersReceived>=5]<-5
  #
  #
  #     df$UnmodifiedNumberOfOffersSummary<-factor(df$UnmodifiedNumberOfOffersSummary,
  #                                                    levels = c(NA,1,2,3.5,5),
  #                                                    labels = c("Unlabeled", "1\nOffer","2\noffers","3-4\noffers","5+\noffers"),
  #                                                    exclude=NULL
  #     )
  #
  #
  #   }
  #
  #
  #
  #   if("MaxOfisChangeOrder" %in% names(df))
  #   {
  #     levels(df$MaxOfisChangeOrder)<-list("No Change Order"=c("No Change Order","0",0),
  #                                             "Closed"=c("Change Order(s)","1",1)
  #     )
  #
  #     addNA(df$MaxOfisChangeOrder, ifany = TRUE)
  #   }
  #
  #   if("IsFixedPrice" %in% names(df))
  #   {
  #     levels(df$IsFixedPrice)<-list("Other"=c("Other","0",0),
  #                                       "Fixed Price"=c("Fixed Price","1",1),
  #                                       "Combination \nor Unlabeled"=c("Combination \nor Unlabeled",NA)
  #     )
  #
  #     # df$IsFixedPrice<-factor(df$IsFixedPrice,
  #     #                             exclude=NULL,
  #     #                             levels = c(1,0,NA),
  #     #                             labels = c("Fixed Price", "Other","Combination \nor Unlabeled")
  #     # )
  #     if (!("Combination \nor Unlabeled" %in% levels(df$IsFixedPrice))){
  #       df$IsFixedPrice<-addNA(df$IsFixedPrice,ifany=TRUE)
  #       levels(df$IsFixedPrice)[is.na(levels(df$IsFixedPrice))] <- "Combination \nor Unlabeled"
  #     }
  #   }
  #
  #
  #   if("Action_Obligation" %in% names(df)){
  #     df$LogOfAction_Obligation<-log10(df$Action_Obligation)
  #     df$LogOfAction_Obligation[is.infinite(df$LogOfAction_Obligation)]<-0
  #
  #     if("NewWorkObligatedAmount" %in% names(df)){
  #       df$pNewWorkVsContractObligatedAmount<-df$NewWorkObligatedAmount/df$Action_Obligation
  #       df$pNewWorkVsContractObligatedAmount[is.infinite(df$pNewWorkVsContractObligatedAmount)]<-NA
  #       df$pNewWorkVsContractObligatedAmount[abs(df$pNewWorkVsContractObligatedAmount)>100]<-NA
  #     }
  #     if("ChangeOrderObligatedAmount" %in% names(df)){
  #       df$pChangeOrderVsContractObligatedAmount<-df$ChangeOrderObligatedAmount/df$Action_Obligation
  #       df$pChangeOrderVsContractObligatedAmount[is.infinite(df$pChangeOrderVsContractObligatedAmount)]<-NA
  #       df$pChangeOrderVsContractObligatedAmount[abs(df$pChangeOrderVsContractObligatedAmount)>100]<-NA
  #     }
  #   }
  #   if("ContractBaseAndAllOptionsValue" %in% names(df)){
  #     df$ContractBaseAndAllOptionsValue<-text_to_number(df$ContractBaseAndAllOptionsValue)
  #     df$LogOfContractBaseAndAllOptionsValue<-log10(df$ContractBaseAndAllOptionsValue)
  #     df$LogOfContractBaseAndAllOptionsValue[is.infinite(df$LogOfContractBaseAndAllOptionsValue)]<-0
  #
  #     if("NewWorkBaseAndAllOptionsValue" %in% names(df)){
  #       df$pNewWorkVsContractBaseAndAllOptionsValue<-df$NewWorkBaseAndAllOptionsValue/df$ContractBaseAndAllOptionsValue
  #       df$pNewWorkVsContractBaseAndAllOptionsValue[is.infinite(df$pNewWorkVsContractBaseAndAllOptionsValue)]<-NA
  #       df$pNewWorkVsContractBaseAndAllOptionsValue[abs(df$pNewWorkVsContractBaseAndAllOptionsValue)>100]<-NA
  #     }
  #     if("ChangeOrderBaseAndAllOptionsValue" %in% names(df)){
  #       df$pChangeOrderVsContractBaseAndAllOptionsValue<-df$ChangeOrderBaseAndAllOptionsValue/df$ContractBaseAndAllOptionsValue
  #       df$pChangeOrderVsContractBaseAndAllOptionsValue[is.infinite(df$pChangeOrderVsContractBaseAndAllOptionsValue)]<-NA
  #       df$pChangeOrderVsContractBaseAndAllOptionsValue[abs(df$pChangeOrderVsContractBaseAndAllOptionsValue)>100]<-NA
  #     }
  #   }
  #   if("ContractBaseAndExercisedOptionsValue" %in% names(df)){
  #     df$ContractBaseAndExercisedOptionsValue<-text_to_number(df$ContractBaseAndExercisedOptionsValue)
  #     df$LogOfContractBaseAndExercisedOptionsValue<-log10(df$ContractBaseAndExercisedOptionsValue)
  #     df$LogOfContractBaseAndExercisedOptionsValue[is.infinite(df$LogOfContractBaseAndExercisedOptionsValue)]<-0
  #
  #     if("NewWorkBaseAndExercisedOptionsValue" %in% names(df)){
  #       df$pNewWorkVsContractBaseAndExercised<-df$NewWorkBaseAndExercisedOptionsValue/df$ContractBaseAndExercisedOptionsValue
  #       df$pNewWorkVsContractBaseAndExercised[is.infinite(df$pNewWorkVsContractBaseAndExercised)]<-NA
  #       df$pNewWorkVsContractBaseAndExercised[abs(df$pNewWorkVsContractBaseAndExercised)>100]<-NA
  #     }
  #     if("ChangeOrderBaseAndExercisedOptionsValue" %in% names(df)){
  #       df$pChangeOrderVsContractBaseAndExercised<-df$ChangeOrderBaseAndExercisedOptionsValue/df$ContractBaseAndExercisedOptionsValue
  #       df$pChangeOrderVsContractBaseAndExercised[is.infinite(df$pChangeOrderVsContractBaseAndExercised)]<-NA
  #       df$pChangeOrderVsContractBaseAndExercised[abs(df$pChangeOrderVsContractBaseAndExercised)>100]<-NA
  #     }
  #   }
  #   if("UnmodifiedContractObligatedAmount" %in% names(df)){
  #     df$UnmodifiedContractObligatedAmount<-text_to_number(df$UnmodifiedContractObligatedAmount)
  #     df$LogOfUnmodifiedContractObligatedAmount<-log10(df$UnmodifiedContractObligatedAmount)
  #     df$LogOfUnmodifiedContractObligatedAmount[is.infinite(df$LogOfUnmodifiedContractObligatedAmount)]<-0
  #     if("Action_Obligation" %in% names(df)){
  #       df$pUnmodifiedContractObligated<-df$UnmodifiedContractObligatedAmount/df$Action_Obligation
  #     }
  #     if("NewWorkObligatedAmount" %in% names(df)){
  #       df$pNewWorkVsUnmodifiedObligatedAmount<-df$NewWorkObligatedAmount/df$UnmodifiedContractObligatedAmount
  #       df$pNewWorkVsUnmodifiedObligatedAmount[is.infinite(df$pNewWorkVsUnmodifiedObligatedAmount)]<-NA
  #       df$pNewWorkVsUnmodifiedObligatedAmount[abs(df$pNewWorkVsUnmodifiedObligatedAmount)>100]<-NA
  #     }
  #     if("ChangeOrderObligatedAmount" %in% names(df)){
  #       df$pChangeOrderVsUnmodifiedObligatedAmount<-df$ChangeOrderObligatedAmount/df$UnmodifiedContractObligatedAmount
  #       df$pChangeOrderVsUnmodifiedObligatedAmount[is.infinite(df$pChangeOrderVsUnmodifiedObligatedAmount)]<-NA
  #       df$pChangeOrderVsUnmodifiedObligatedAmount[abs(df$pChangeOrderVsUnmodifiedObligatedAmount)>100]<-NA
  #     }
  #   }
  #   if("UnmodifiedContractBaseAndAllOptionsValue" %in% names(df)){
  #     df$UnmodifiedContractBaseAndAllOptionsValue<-text_to_number(df$UnmodifiedContractBaseAndAllOptionsValue)
  #     df$LogOfUnmodifiedContractBaseAndAllOptionsValue<-log10(df$UnmodifiedContractBaseAndAllOptionsValue)
  #     df$LogOfUnmodifiedContractBaseAndAllOptionsValue[is.infinite(df$LogOfUnmodifiedContractBaseAndAllOptionsValue)]<-0
  #
  #     df$SizeOfUnmodifiedContractBaseAndAll<-CreateSize(df$UnmodifiedContractBaseAndAllOptionsValue)
  #
  #     if("ContractBaseAndAllOptionsValue" %in% names(df)){
  #       df$pUnmodifiedContractBaseAndAll<-df$UnmodifiedContractBaseAndAllOptionsValue/df$ContractBaseAndAllOptionsValue
  #     }
  #     if("NewWorkBaseAndAllOptionsValue" %in% names(df)){
  #       df$pNewWorkVsUnmodifiedBaseAndAll<-df$NewWorkBaseAndAllOptionsValue/df$UnmodifiedContractBaseAndAllOptionsValue
  #       df$pNewWorkVsUnmodifiedBaseAndAll[is.infinite(df$pNewWorkVsUnmodifiedBaseAndAll)]<-NA
  #       df$pNewWorkVsUnmodifiedBaseAndAll[abs(df$pNewWorkVsUnmodifiedBaseAndAll)>100]<-NA
  #     }
  #     if("ChangeOrderBaseAndAllOptionsValue" %in% names(df)){
  #       df$pChangeOrderVsUnmodifiedBaseAndAll<-df$ChangeOrderBaseAndAllOptionsValue/df$UnmodifiedContractBaseAndAllOptionsValue
  #       df$pChangeOrderVsUnmodifiedBaseAndAll[is.infinite(df$pChangeOrderVsUnmodifiedBaseAndAll)]<-NA
  #       df$pChangeOrderVsUnmodifiedBaseAndAll[abs(df$pChangeOrderVsUnmodifiedBaseAndAll)>100]<-NA
  #     }
  #   }
  #   if("UnmodifiedContractBaseAndExercisedOptionsValue" %in% names(df)){
  #     df$UnmodifiedContractBaseAndExercisedOptionsValue<-text_to_number(df$UnmodifiedContractBaseAndExercisedOptionsValue)
  #     df$LogOfUnmodifiedContractBaseAndExercisedOptionsValue<-log10(df$UnmodifiedContractBaseAndExercisedOptionsValue)
  #     df$LogOfUnmodifiedContractBaseAndExercisedOptionsValue[is.infinite(df$LogOfUnmodifiedContractBaseAndExercisedOptionsValue)]<-0
  #     if("ContractBaseAndExercisedOptionsValue" %in% names(df)){
  #       df$pUnmodifiedContractBaseAndExercised<-df$UnmodifiedContractBaseAndExercisedOptionsValue/df$ContractBaseAndExercisedOptionsValue
  #     }
  #     if("NewWorkBaseAndExercisedOptionsValue" %in% names(df)){
  #       df$pNewWorkVsUnmodifiedBaseAndExercised<-df$NewWorkBaseAndExercisedOptionsValue/df$UnmodifiedContractBaseAndExercisedOptionsValue
  #       df$pNewWorkVsUnmodifiedBaseAndExercised[is.infinite(df$pNewWorkVsUnmodifiedBaseAndExercised)]<-NA
  #       df$pNewWorkVsUnmodifiedBaseAndExercised[abs(df$pNewWorkVsUnmodifiedBaseAndExercised)>100]<-NA
  #     }
  #     if("ChangeOrderBaseAndExercisedOptionsValue" %in% names(df)){
  #       df$pChangeOrderVsUnmodifiedBaseAndExercised<-df$ChangeOrderBaseAndExercisedOptionsValue/df$UnmodifiedContractBaseAndExercisedOptionsValue
  #       df$pChangeOrderVsUnmodifiedBaseAndExercised[is.infinite(df$pChangeOrderVsUnmodifiedBaseAndExercised)]<-NA
  #       df$pChangeOrderVsUnmodifiedBaseAndExercised[abs(df$pChangeOrderVsUnmodifiedBaseAndExercised)>100]<-NA
  #     }
  #   }
  #
  #   #
  #   # ChangeOrderObligatedAmount
  #   # ChangeOrderBaseAndExercisedOptionsValue
  #   # ChangeOrderBaseAndAllOptionsValue
  #   # NewWorkObligatedAmount
  #   # NewWorkBaseAndExercisedOptionsValue
  #   # NewWorkBaseAndAllOptionsValue
  #
  #
  #

  #
  #   if("Date.Signed"%in% names(df)){
  #
  #     if(max(nchar(as.character(df$Date.Signed)))==10){
  #       #         if((max(substring(as.character(df$Date.Signed),7,8))=="99" |
  #       #                max(substring(as.character(df$Date.Signed),7,8))<"20") &
  #       #                !max(substring(as.character(df$Date.Signed),1,2))>"12"){
  #       df$Date.Signed <-as.Date(as.character(df$Date.Signed),"%m/%d/%Y")
  #     }
  #     else{
  #       df$Date.Signed <-as.Date(as.character(df$Date.Signed),"%y/%m/%d")
  #     }
  #   }
  #
  #   if("SignedMonth"%in% names(df)){
  #     df$SignedMonth <-as.Date(as.character(df$SignedMonth),"%Y-%m-%d")
  #   }
  #
  #
  #   if("YEAR"%in% names(df)){
  #     df$YEAR <-as.Date(paste("12/31/",as.character(df$YEAR),sep=""),"%m/%d/%Y")
  #   }
  #
  #   if(!("Graph" %in% names(df))){
  #     df$Graph<-TRUE
  #   }
  #   if("ProductOrServiceArea.Graph"%in% names(df)){
  #     df$Graph<-df$Graph&&df$ProductOrServiceArea.Graph
  #     df<-subset(df, select=-c(ProductOrServiceArea.Graph))
  #   }
  #   if("Contract.Size.Graph"%in% names(df)){
  #     df$Graph<-df$Graph&&df$Contract.Size.Graph
  #     df<-subset(df, select=-c(Contract.Size.Graph))
  #   }
  #
  #   if("Competition.Graph"%in% names(df)){
  #     df$Graph<-df$Graph&&df$Competition.Graph
  #     df<-subset(df, select=-c(Competition.Graph))
  #   }
  #   if("Vehicle.Graph"%in% names(df)){
  #     df$Graph<-df$Graph&&df$Vehicle.Graph
  #     df<-subset(df, select=-c(Vehicle.Graph))
  #   }
  #   if("Pricing.Mechanism.Graph"%in% names(df)){
  #     df$Graph<-df$Graph&&df$Pricing.Mechanism.Graph
  #     df<-subset(df, select=-c(Pricing.Mechanism.Graph))
  #   }
  #   if("Customer.Graph"%in% names(df)){
  #     df$Graph<-df$Graph&&df$Customer.Graph
  #     df<-subset(df, select=-c(Customer.Graph))
  #   }
  #   if("LastCurrentCompletionDate"%in% names(df)&"MinOfEffectiveDate"%in% names(df)){
  #
  #     df$CurrentMonths<-as.numeric(difftime(strptime(df$LastCurrentCompletionDate,"%Y-%m-%d")
  #                                               , strptime(df$MinOfEffectiveDate,"%Y-%m-%d")
  #                                               , unit="weeks"
  #     ))
  #     df$CategoryOfCurrentMonths<-CreateDuration(df$CurrentMonths)
  #     df$CurrentMonths<-ceiling(df$CurrentMonths/4)
  #   }
  #
  #
  #   if("UnmodifiedCurrentCompletionDate"%in% names(df)&"MinOfEffectiveDate"%in% names(df)){
  #
  #     df$UnmodifiedMonths<-as.numeric(difftime(strptime(df$UnmodifiedCurrentCompletionDate,"%Y-%m-%d")
  #                                                  , strptime(df$MinOfEffectiveDate,"%Y-%m-%d")
  #                                                  , unit="weeks"
  #     ))
  #     df$CategoryOfUnmodifiedMonths<-CreateDuration(df$UnmodifiedMonths)
  #     df$UnmodifiedMonths<-ceiling(df$UnmodifiedMonths/4)
  #
  #   }


  #   if("UnmodifiedIsSomeCompetition" %in% names(df))
  #   {
  #     levels(df$UnmodifiedIsSomeCompetition)<-list("No Comp."=c("No Comp.","0",0),
  #                                                      "Comp."=c("Comp.","1",1),
  #                                                      "Unlabeled"=c("Unlabeled",NA)
  #     )
  #
  #     # df$UnmodifiedIsSomeCompetition<-factor(df$UnmodifiedIsSomeCompetition,
  #     #                                            exclude=NULL,
  #     #                                            levels = c(1,0,NA),
  #     #                                            labels = c("Comp.", "No Comp.","Unlabeled")
  #     # )
  #   }
  #
  #   if("IsSomeCompetition" %in% names(df))
  #   {
  #
  #
  #
  #     if ("IsFullAndOpen" %in% names(df)&
  #         "IsOnlyOneSource" %in% names(df)){
  #       df$UnmodifiedCompetition[df$IsFullAndOpen==1]<-1
  #       df$UnmodifiedCompetition[df$IsSomeCompetition==1
  #                                    &is.na(df$UnmodifiedCompetition)]<-2
  #       df$UnmodifiedCompetition[df$IsOnlyOneSource==1
  #                                    &is.na(df$UnmodifiedCompetition)]<-3
  #       df$UnmodifiedCompetition[df$IsOnlyOneSource==0
  #                                    &is.na(df$UnmodifiedCompetition)]<-4
  #       df$UnmodifiedCompetition<-factor(df$UnmodifiedCompetition
  #                                            ,exclude=NULL
  #                                            ,levels=c(1,2,3,4,NA)
  #                                            ,labels=c("Full and Open"
  #                                                      ,"Some Comp."
  #                                                      ,"No Comp.\n1 Source"
  #                                                      ,"No Comp.\nOther"
  #                                                      ,"Unlabeled"
  #                                            )
  #       )
  #
  #     }
  #     levels(df$IsSomeCompetition)<-list("No Comp."=c("No Comp.","0",0),
  #                                            "Comp."=c("Comp.","1",1),
  #                                            "Mixed or \nUnlabeled"=c("Mixed or \nUnlabeled",NA)
  #     )
  #
  #
  #     df$IsSomeCompetition<-factor(df$IsSomeCompetition,
  #                                      exclude=NULL,
  #                                      levels = c(1,0,NA),
  #                                      labels = c("Comp.", "No Comp.","Mixed or \nUnlabeled")
  #     )
  #
  #     if("IsFullAndOpen" %in% names(df))
  #     {
  #       df$IsFullAndOpen<-factor(df$IsFullAndOpen,
  #                                    exclude=NULL,
  #                                    levels = c(1,0,NA),
  #                                    labels = c("Full & Open", "Not Full \n& Open","Mixed or \nUnlabeled")
  #       )
  #     }
  #
  #
  #
  #     if("UnmodifiedIsFullAndOpen" %in% names(df))
  #     {
  #       df$UnmodifiedIsFullAndOpen<-factor(df$UnmodifiedIsFullAndOpen,
  #                                              exclude=NULL,
  #                                              levels = c(1,0,NA),
  #                                              labels = c("Full & Open", "Not Full \n& Open","Unlabeled")
  #       )
  #     }
  #     if("IsOnlyOneSource" %in% names(df))
  #     {
  #       df$IsOnlyOneSource<-factor(df$IsOnlyOneSource,
  #                                      exclude=NULL,
  #                                      levels = c(1,0,NA),
  #                                      labels = c("Only One Source", "Not Only Once Source","Unlabeled")
  #       )
  #     }
  #
  #
  #
  #
  #     if ("IsIDV" %in% names(df)&
  #         "multipleorsingleawardidc" %in% names(df)&
  #         "AwardOrIDVcontractactiontype" %in% names(df)
  #     ){
  #       df$UnmodifiedVehicle[is.na(df$IsIDV)]<-NA
  #       df$UnmodifiedVehicle[df$AwardOrIDVcontractactiontype %in% c("Definitive Contract")
  #                                &is.na(df$UnmodifiedVehicle)]<-1
  #       df$UnmodifiedVehicle[df$AwardOrIDVcontractactiontype %in% c("Purchase Order")
  #                                &is.na(df$UnmodifiedVehicle)]<-2
  #       df$UnmodifiedVehicle[df$AwardOrIDVcontractactiontype %in% c("Blanket Purchase Agreement"
  #                                                                           ,"Federal Supply Schedule"
  #                                                                           ,"Government Wide Acquisition Contract"
  #                                                                           ,"Basic Ordering Agreement")
  #                                &is.na(df$UnmodifiedVehicle)]<-5
  #
  #       df$UnmodifiedVehicle[df$multipleorsingleawardidc=="MULTIPLE AWARD"
  #                                &is.na(df$UnmodifiedVehicle)]<-4
  #       df$UnmodifiedVehicle[df$multipleorsingleawardidc=="SINGLE AWARD"
  #                                &is.na(df$UnmodifiedVehicle)]<-3
  #       df$UnmodifiedVehicle[is.na(df$UnmodifiedVehicle)]<-6
  #       df$UnmodifiedVehicle<-factor(df$UnmodifiedVehicle
  #                                        ,exclude=NULL
  #                                        ,levels=c(1
  #                                                  ,2
  #                                                  ,3
  #                                                  ,4
  #                                                  ,5
  #                                                  ,6
  #                                                  ,NA)
  #                                        ,labels=c("Definitive"
  #                                                  ,"Purchase\nOrder"
  #                                                  ,"Single-Award\nIDC"
  #                                                  ,"Multi-Award\nIDC"
  #                                                  ,"Other IDC"
  #                                                  ,"Unlabeled\nIDC"
  #                                                  ,"Unlabeled"
  #                                        )
  #       )
  #
  #     }
  #
  #   }

  standardize_variable_names(df)
}

