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
#' @examples plot<-get_preasssigned_scales(plot,labels_and_colors,"pricing.mechanism.sum")
#'
#' @import ggplot2
#' @export
add_preassigned_scales<-function(
  plot,
  labels_and_colors,
  var="None"
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
    plot<-plot+scale_color_manual(
      values = subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$RGB,
      # limits=c(subset(labels_and_colors,column==var)$variable),
      limits=c(subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$Label)
    )+scale_fill_manual(
      values = subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variable %in% levels(plot$data[,var])))$RGB,
      # limits=c(subset(labels_and_colors,column==var)$variable),
      limits=c(subset(labels_and_colors,column==var & (Label %in% levels(plot$data[,var])|variables %in% levels(plot$data[,var])))$Label)
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

#' Returns data in the appropriate format for the user-specified plot
#'
#' @param data The data to format for the plot, as a tibble
#' @param x_var x-axis
#' @param y_var y-axis
#' @param breakout TESTTESTTESTTESTTEST
#' @param aggregate aggregation function; defaults to sum
#'
#' @return A tibble of formatted data
#'
#' @details
#'
#'
#'
#' @export
group_data_for_plot <-function(
  data,   # data to format for the plot, as a tibble
  x_var,
  y_var,
  breakout,
  aggregate ="sum"
  #
  # Returns:
  #   a tibble of formatted data
){
  # account for potential spaces in breakout and x_var
  # note that this doesn't test for whether quotes already exist

  if(grepl(" ", x_var)) x_var <- paste0("`", x_var, "`")
  if(length(breakout) >= 1){
    if(grepl(" ", breakout[1])) breakout[1] <- paste0("`", breakout[1], "`")
  }
  if(length(breakout) == 2){
    if(grepl(" ", breakout[2])) breakout[2] <- paste0("`", breakout[2], "`")
  }

  # aggregate to the level of [fiscal year x breakout]
  # the evaluation for dplyr::summarize_ was a pain in the ass to figure out;
  # see stack overflow at https://tinyurl.com/z82ywf3

  if(aggregate=="sum"){
    if(length(breakout) == 0){
      data %<>%
        group_by_(x_var) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(y_var)))
    } else {
      data %<>%
        group_by_(.dots = c(x_var, breakout)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(y_var)))
    }
  } else if (aggregate=="mean"){
    if(length(breakout) == 0){
      data %<>%
        group_by_(x_var) %>%
        summarize_(
          agg_val = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(y_var)))
    } else {
      data %<>%
        group_by_(.dots = c(x_var, breakout)) %>%
        summarize_(
          agg_val = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(y_var)))
    }
  } else (stop(paste,"group_data_for_plot does not know how to handle aggregate = ",aggregate))

  names(data)[which(names(data) == "agg_val")] <- y_var
  return(data)
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


#' Extract a legend
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

#Extract a legend
# https://stackoverflow.com/questions/43366616/ggplot2-legend-only-in-a-plot
# Alternate unused approach From https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs


get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

