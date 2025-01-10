#' Get Column Key based on the names in a data frame
#'
#' @param data A data frame
#' @param path The path or url for the column key.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/Lookup-Tables/master/style/
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

  if(!file.exists(file.path(path,"Lookup_Coloration.csv")) || path=="offline")
    path<-file.path(get_local_lookup_path(),"style//")

  #Join up the files
  column_key<-read_and_join_experiment(column_key,
                                       "Lookup_Column_Key.csv",
                                       path=path,
                                       directory="",
                                       by="column",
                                       new_var_checked=FALSE,
                                       case_sensitive = FALSE
  )

  #Set empty string coloration.keys equal to na
  column_key$coloration.key[column_key$coloration.key==""]<-NA
  return(column_key)
}


#' Prepare Labels And Colors
#'
#' @param data the data frame to be joined
#' @param var the variables (column names) for which to prepare colors; by default all will be done
#' @param na_replaced if TRUE, replace NA values with var before adding colors
#' @param path The location of the lookup file
#'
#' @return A new data frame built on the variable var. It will
#' include colors, and order, and proper name labels.
#'
#' @details This function applies standard colors and orders to a
#' column var in a data frame data. Colors and order are drawn from pre-existing lookup tables.
#' When values are missing or wrong, these tables must be manually updated.
#' This function is badly optimized, reading in multiple csvs every time.
#' It is intend for use in data preparation source code and not to be used in a
#' real time web environment.
#'
#' @examples FullData<-standardize_variable_names(Path,
#'   FullData)
#'
#' @export
prepare_labels_and_colors<-function(data
                                    ,var=NULL
                                    ,na_replaced=TRUE
                                    ,path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/"
                                    #                                  ,VAR.override.coloration=NA
                                    ,missing_allowed=FALSE
)
{


  data<-as.data.frame(data)


  #Confirm that the category is even available in the data set.
  if(!is.null(var)){
    if(!var %in% names(data)){
      stop(paste(var,"column is not found in data."))
    }
  }

  if(!file.exists(file.path(path,"Lookup_Coloration.csv")) || path=="offline")
    path<-file.path(get_local_lookup_path(),"style//")
  #Read in coloration
  coloration<-read.csv(

    file.path(path,"Lookup_Coloration.csv"),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE,encoding="UTF-8"
  )
  if(min(nchar(coloration$RGB[!is.na(coloration$RGB)]))!=7) stop("Malformed hex code in RGB")

  if(ncol(coloration)!=12) stop("Mismatched number of columns in coloration.txt.")

  #Fix oddities involving coloration text, and handle accented characters.

  coloration$variable <- gsub("\\\\n","\n",coloration$variable,useBytes = TRUE)#iconv, from="UTF-8", to="LATIN1")
  coloration$Label <- gsub("\\\\n","\n",coloration$Label,useBytes = TRUE)#incov, from="UTF-8", to="LATIN1")

  #Translate the category name into the appropriate coloration.key
  #This is used because we have more category names than coloration.key
  column_key<-get_column_key(data,path=path)

  #If a column has been passed
  if(!is.null(var)){
    column_key<-subset(column_key, column==var)

    #Should adjust this to give proper errors for multiple vars
    #when only one is missing
    if(any(is.na(column_key$coloration.key))){
      if(missing_allowed)
        return(NA)
      else
        stop(paste(var,"is missing from Lookup_column_key.csv"))
    }

  }
  else {
    column_key<-subset(column_key, !is.na(coloration.key) & coloration.key!="Exclude")
  }

  if(nrow(column_key)==0) stop("No matching columns")

  names.data<-NULL
  for(v in (1:nrow(column_key))){
    if(na_replaced==TRUE){
      data<-replace_nas_with_unlabeled(data,column_key$column[v])
    }

    #Limit the lookup table to those series that match the variable
    labels_category_data<-subset(coloration, coloration.key==
                                   column_key$coloration.key[v] )

    if(nrow(labels_category_data)==0) stop(paste("No matching levels for:",column_key$column[v]))

    if(any(is.na(labels_category_data$Label)))
      stop(paste("NA(s) in label field:",paste(labels_category_data %>% dplyr::filter(is.na(Label)) %>% dplyr::select(variable,coloration.key)),
           collapse=", "),collapse="; ")

    #Error checking for duplicates in lookup_coloration.csv
    if(anyDuplicated(labels_category_data$variable)>0){
      print(labels_category_data$variable[
        duplicated(labels_category_data$variable)])
      stop(paste("Lookup_Coloration.csv has"
                 ,sum(duplicated(labels_category_data$variable))
                 ,"duplicate value(s) for category="
                 ,column_key$coloration.key[v], ". See above for a list of missing labels")
      )
    }
    c<-as.character(column_key$column[v])
    k<-as.character(column_key$coloration.key[v])
    #Check for any values in the current field that are not assigned a color.
    values<-unique(data.frame(data)[,c])


    #Handle any case discrepancies.
    #Certain type of quote.
    # labels_category_data$variable<-gsub("\u0092","’",labels_category_data$variable)

    if(k=="Country"){
      #Manual fix in case excel is breaking this.
      labels_category_data$variable[labels_category_data$variable=="ÌÉland Islands"]<-"Åland Islands"
    }
    case_mismatch<-    toupper(labels_category_data$variable) %in% toupper(values)&
      !labels_category_data$variable %in% values
    labels_category_data$variable[case_mismatch]
    for(i in labels_category_data$variable[case_mismatch]){
      labels_category_data$variable[labels_category_data$variable==i]<-
        as.character(values[toupper(values)==toupper(labels_category_data$variable[labels_category_data$variable==i])])
    }





    NA.labels<-values[!values %in% labels_category_data$variable]



    if (length(NA.labels)>0){
      #Unlabeled is highly standardized, adding automatically
      if(length(NA.labels)==1 &
         !is.na(NA.labels[1])&
         NA.labels[1]=="Unlabeled")
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
                                               alpha=NA,
                                               text.color="default grey",
                                               text.RGB="#554449",
                                               abbreviation="Unlabeled"
                                    ))
      }
      else{
        if(missing_allowed)
          return(NA)
        else{
          #Otherwise create an error.
          print(as.character(NA.labels))
          stop(paste("Lookup_Coloration.csv is missing "
                     ,length(NA.labels)
                     ," label(s) for column="
                     ,c," & key=",k, ". See above for a list of missing labels.", sep="")

          )
        }
      }
    }

    labels_category_data<-subset(labels_category_data
                                 , variable %in% unique(data[,c]))


    #Order the names.data and then pass on the same order to the actual data in data
    labels_category_data$Display.Order<-as.numeric(as.character(labels_category_data$Display.Order))
    labels_category_data<-labels_category_data[order(labels_category_data$Display.Order),]
    labels_category_data$column<-c
    # warning(c)
    names.data<-rbind(names.data,labels_category_data)
  }
  names.data
}


#' Add Labels And Colors
#'
#' @param df the data frame to be joined
#' @param var The variables (column names) for which to add levels
#' @param coloration.key=NULL The coloration.key to apply, if null use an existing one
#' @param title The title to apply to apply in the column_key, for a new variable
#' @param share.title The share.title to apply in the column_key, for a new variable
#' @param share.title The share.title to apply in the column_key, for a new variable
#' @return A new data frame built on the variable var. It will
#' include colors, and order, and proper name labels.
#'
#' @details This function adds a new set of levels to the files used to track
#' standard colors and orders. It first checks whether the var and coloration.key
#' already exist in the column_key file, then it fills in any new levels to the
#' Lookup_coloration.csv file. Users will need to first have cloned the lookup-tables
#' repository. Typically manual adjustment of Lookup_coloration.csv will still be
#' necessary, but should be less tedious
#'
#'
#' @export

add_labels_and_colors<-function(df,
                                var,
                                coloration.key=NULL,
                                title="",
                                share.title="",
                                period.title=""
)
{

  df<-as.data.frame(df)

  #Confirm that the category is even available in the data set.
  if(!is.null(var)){
    if(!var %in% names(df)){
      stop(paste(var,"column is not found in df."))
    }
  }

  path<-file.path(get_local_lookup_path(),"style")

  if(!file.exists(file.path(path,"Lookup_Coloration.csv")))
    stop("Lookup_Coloration.csv not found. Have you cloned the lookups repository?")

  #Read in coloration
  coloration<-read.csv(
    file.path(path,"Lookup_Coloration.csv"),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE,encoding="UTF-8"
  )

  column_key_df<-read.csv(
    file.path(path,"Lookup_Column_Key.csv"),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE,encoding="UTF-8"
  )

  rbind_pos<-function(old_df,
                      pos,
                      add_df){
    if(nrow(old_df)<pos)
      stop(paste("pos",pos,"exceeds the number of rows in df",nrow(old_df)))
    else if(nrow(old_df)==pos)
      df<-rbind(old_df,add_df)
    else
      df<-rbind(old_df[1:pos,],
                add_df,
                old_df[(pos+1):nrow(old_df),])
    df
  }

  #First manage coloration.key
  if(var %in% column_key_df$column){
    if(is.null(coloration.key))
      coloration.key<-column_key_df$coloration.key[column_key_df$column==var]
    else if(column_key_df$coloration.key[column_key_df$column==var]!=coloration.key)
      stop(paste("coloration.key",coloration.key,"does not match current coloration.key",
                 column_key_df$coloration.key[column_key_df$column==var]))
  }
  else{
    if(is.null(coloration.key))
      stop("var is not presently in Lookup_Column_Key.csv, you must assign a coloration.key")
    if(!coloration.key %in% column_key_df$coloration.key){
      warning(paste("Adding new column key:", coloration.key))
      pos_ck<-nrow(column_key_df)
    }
    else
      pos_ck<-max(which(coloration.key == column_key_df$coloration.key))

    new_key<-data.frame(
      column=var,
      coloration.key=coloration.key,
      title=title,
      share.title=share.title,
      period.title=period.title,
      is.colon.split=FALSE
    )
    column_key_df<-rbind_pos(column_key_df,pos_ck,new_key)

    write_csv(column_key_df,
      file.path(path,"Lookup_Column_Key.csv"),
      quote="needed",na=""
    )

  }
  #Handle Lookup_coloration.csv
  levels_list<-unique(df[,var])

  new_levels<-data.frame(
    coloration.key=coloration.key,
    variable=levels_list,
    Label=levels_list,
    Display.Order=0,
    Color="DIIG Secondary, Tangelo",
    RGB="#e58846",
    shape="",
    size="",
    alpha="",
    text.color="default grey",
    text.RGB="#554449",
    abbreviation=""
  )
  #First handle an existing key
  if(any(coloration.key %in% coloration$coloration.key)){
    pos_lc<-max(which(coloration.key == coloration$coloration.key))
    old_levels<-coloration[coloration$coloration.key==coloration.key,]
    new_levels<-new_levels %>% filter(!variable %in% old_levels$variable)
    if(nrow(new_levels)==0){
      warning("No new levels to add")
      return()
    }
    coloration<-rbind_pos(coloration,pos_lc,new_levels)
  }
  else{ #For entirely new coloration.keys, add to the end of the file.
    coloration<-rbind(coloration,new_levels)
  }

  write_csv(coloration,
            file.path(path,"Lookup_Coloration.csv"),
            quote="needed",na=""
  )
}


#' Quickly assign yearly breaks to a chart
#'
#' @param start First year of break sequence
#' @param stop Last year of break sequence
#' @by Frequency of data breaks, e.g. 1 for every year, 5 for every 5 years
#' @fiscal_year A placeholder for future tuning by fiscal vs. calendar year
#' @partial_year If one year of incomplete data is included, specify it with this variable
#' @partial_label If one year of incomplete data is included, specify it with this variable
#'
#' @return A plot with added color and fill scales for the column passed
#'
#' @details Add year breaks at specified intervals for date data
#'
#' @examples date_x_year_breaks(2000,2023,2)
#'
#' @export
date_x_year_breaks<-function(start,stop,by,fiscal_year=TRUE,partial_year=NULL,partial_label="\nYTD"){
  if(is.null(partial_year))
    return(scale_x_date(breaks = as.Date(paste(seq(start,stop, by=by),"01","01",sep="-")),
                        date_labels = "'%y"))
  else {
    #List dates in sequence and the partial_year
    b<-as.Date(paste(c(seq(start,stop, by=by),partial_year),"01","01",sep="-"))
    #If the partial year shows up twice, remove it the duplicate
    b<-b[!duplicated(b)]
    l<-as.character(b)
    py<-l==paste(partial_year,"01","01",sep="-")
    l[!py]<-format(as.Date(l[!py]),"'%y")
    l[py]<-paste(format(as.Date(l[py]),"'%y"),partial_label,sep="")
    return(scale_x_date(breaks = b,labels=l)
    )

  }

  ToplinePricing+scale_x_continuous(breaks=c(seq(2000,2020, by=6),2023),
                                    labels=c(paste("'",substr(seq(2000,2020, by=6),3,4)),"'23\n(Q1-Q2)"))
}

#' Take existing data frame and associate colors with values
#'
#' @param plot The existing ggplot, needed to add more than one scale
#' @param labels_and_colors A csis360 lookup data.frame with factor information
#' @param var The name of the column, default to none for generic colors
#'
#' @return A plot with added color and fill scales for the column passed
#'
#' @details labels_and_colors is a data.frame produced by
#' the csis360 package, see prepare_labels_and_colors drawing
#' from lookup table that preassign labels, order, and color
#' to the expected values of a column.
#'
#' @examples plot<-add_preassigned_scales(plot,labels_and_colors,"pricing.mechanism.sum")
#'
#' @import ggplot2
#' @export
add_preassigned_scales<-function(
  plot,
  labels_and_colors,
  var="None"
  # reverse_color=FALSE #' @param reverse_color If True reverse the order of the factor
){

  #If a column name is passed and it is in labels_and_colors
  if(var!="None" & any(labels_and_colors$column==var)){
    if(is.character(plot$data[,var])) plot$data[,var]<-factor(plot$data[,var])

    #Rename factor to use label values
    oldname<-subset(labels_and_colors,column==var & (!Label %in% levels(plot$data[,var])&variable %in% levels(plot$data[,var])))$variable
    newname<-subset(labels_and_colors,column==var & (!Label %in% levels(plot$data[,var])&variable %in% levels(plot$data[,var])))$Label
    plot$data[,var]<-plyr::mapvalues(plot$data[,var],oldname,newname)
    var_label<-subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))
    var_label$Display.Order<-as.numeric(var_label$Display.Order)
    var_label<-var_label[order(var_label$Display.Order),]
    if (any(duplicated(var_label$Label))) stop("Duplicated label")
    # if(reverse_color) var_label<-var_label[order(-var_label$Display.Order),]
    plot$data[,var]<-factor(plot$data[,var],levels=var_label$Label)
    plot<-plot+scale_color_manual(
      values = subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$RGB,
      limits=c(subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$Label)
    )+scale_fill_manual(
      values = subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$RGB,
      # limits=c(subset(labels_and_colors,column==var)$variable),
      limits=c(subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$Label)
    )
  }
  else{
    #Drawing from the color blind palette from
    #http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
    cbPalette <- c( "#e58846", "#66c6cb", "#4c9361",
                    "#eec260", "#0054A4", "#BB4243", "#9eadd8","#999999")

    # BarPalette <- scale_fill_manual(
    #   values =  c( "#e58846", "#66c6cb", "#4c9361",
    #                "#eec260", "#0054A4", "#BB4243",
    #                "#9eadd8","#999999"))
    # c(
    # "#004165",
    # "#0065a4",
    # "#0095AB",
    # "#66c6cb",
    # "#75c596",
    # "#0faa91",
    # "#51746d",
    # "#607a81",
    # "#252d3a",
    # "#353535",
    # "#797979"))


    # LinePalette <- scale_color_manual(
    #   values =  c( "#e58846", "#66c6cb", "#4c9361",
    #                "#eec260", "#0054A4", "#BB4243",
    #                "#9eadd8","#999999"))

    #Palettte for 12 colors http://mkweb.bcgsc.ca/colorblind/palettes/12.color.blindness.palette.txt
    cbPalette12 <- scale_color_manual(
      values =  c( "#9F0162", "#009F81", "#FF5AAF ",
                   "#00FCCF", "#8400CD","#008DF9   ",
                   "#00C2F9","#FFB2FD ","#A40122 ",
                   "#E20134 ","#FF6E3A","#FFC33B"))

    # values = c(
    #   "#004165",
    #   "#75c596",
    #   "#b24f94",
    #   "#0095ab",
    #   "#0a8672",
    #   "#e22129",
    #   "#66c6cb",
    #   "#51746d",
    #   "#797979",
    #   "#788ca8",
    #   "#353535"))

    if(var!="None"){
      warning(paste(var,"not found in labels_and_colors"))
      if(length(levels(factor(plot$data[,var])))<=8)
        plot<-plot+scale_color_manual(
          values = cbPalette
        )+scale_fill_manual(
          values = cbPalette
        )
    }
    # else if(length(levels(factor(plot$data[,var])))<=12){
    #   plot<-plot+scale_color_manual(
    #     values = cbPalette12
    #   )+scale_fill_manual(
    #     values = cbPalette12
    #   )
    #   warning("Too many levels to apply DIIG color blind palette")
    # }
    else(warning("Too many levels to apply color blind palette"))
  }
  return(plot)
}



#' Get Label
#'
#' @return character string with the label corresponding to a column
#'
#' @param var The names of the column, default of none for generic colors
#' @param column_key A csis360 lookup data.frame with column information
#' @param share If TRUE, calculates the share
#'
#'
#'
#'
#'
#' @export
get_label <- function(
  var,
  column_key,
  share = FALSE
){
  if(is.null(var)) stop("Null var passed to get_label.")
  if(var=="None") label<-""
  else{
    if(share==TRUE){
      title<-subset(column_key,column==var)$share.title
    }
    else
      title<-subset(column_key,column==var)$title
    label<-if_else(is.na(title),var,title)
  }
  return(label)

}




#' Renames a factor level to user-specified name, in the passed data frame
#'
#' @param data The data frame in which to rename the value
#' @param input Shiny input object
#'
#' @return A data frame with the factor level renamed
#'
#'
#'
#'
#' @export
rename_value <- function(
  data,    # the data frame in which to rename the value
  input    # shiny input object
){
  levels(data[[input$edit_var]])[levels(data[[
    input$edit_var]]) == input$edit_value] <- input$rename_value_txt

  return(data)
}


#' Extract a legend https://stackoverflow.com/questions/43366616/ggplot2-legend-only-in-a-plot
#' Alternate unused approach From https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @param a.gplot a ggplot
#'
#' @return Returns a legend
#'
#'
#'
#'
#'
#' @import ggplot2
#' @export
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



#' Return default caption text.
#'
#' @return A label that can be added to a graph.
#'
#' @export
get_caption<-function(
){
  c<-labs(caption="Source: FPDS; CSIS analysis.")
  return(c)
}

#' Add line breaks to a string in a factor
#'
#' @param string A string
#' @param nwrap The number of characters between returns
#'
#' @return String with carriage breaks
#'
#' @export
# Helper function for string wrapping.
# Default 20 character target width.
factor_wrap <- function(f, nwrap=20) {
  # https://stackoverflow.com/questions/37174316/how-to-fit-long-text-into-ggplot2-facet-titles
  string_wrap<- function(string, nwrap=20) {
    paste(strwrap(string, width=nwrap), collapse="\n")
  }
  string_wrap <- Vectorize(string_wrap)
  if(!is.factor(f)){
    warning("Converting f to a factor")
    f<-factor(f)
  }
  levels(f)<-string_wrap(levels(f))
  f
}


