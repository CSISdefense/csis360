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

  #Read in coloration
  coloration<-read.csv(

    paste(path,"Lookup_Coloration.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE,encoding="UTF-8"
  )
  if(min(nchar(coloration$RGB[!is.na(coloration$RGB)]))!=7) stop("Malformed hex code in RGB")

  if(ncol(coloration)!=12) stop("Mismatched number of columns in coloration.txt.")

  #Fix oddities involving coloration text, and handle accented characters.
  coloration$variable <- gsub("\\\\n","\n",coloration$variable)#iconv, from="UTF-8", to="LATIN1")
  coloration$Label <- gsub("\\\\n","\n",coloration$Label)#incov, from="UTF-8", to="LATIN1")

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
    warning(c)
    names.data<-rbind(names.data,labels_category_data)
  }
  names.data
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

  BarPalette <- scale_fill_manual(
    values = c(
      "#004165",
      "#0065a4",
      "#0095AB",
      "#66c6cb",
      "#75c596",
      "#0faa91",
      "#51746d",
      "#607a81",
      "#252d3a",
      "#353535",
      "#797979"))

  LinePalette <- scale_color_manual(
    values = c(
      "#004165",
      "#75c596",
      "#b24f94",
      "#0095ab",
      "#0a8672",
      "#e22129",
      "#66c6cb",
      "#51746d",
      "#797979",
      "#788ca8",
      "#353535"))
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
    #This doesn't work yet
    if(var!="None")
      warning(paste(var,"not found in labels_and_colors"))
    # plot<-plot+scale_color_manual(
    #   values = LinePalette$values
    #  )+scale_fill_manual(
    #   values = BarPalette$values
    # )
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
    label<-ifelse(is.na(title),var,title)
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
  c<-labs(caption="Source: FPDS; CSIS analysis")
  return(c)
}
