#' Get Preassigned Scales
#'
#' @return plot with added color and fill scales for the column passed.
#'
#' @section labels_and_colors is a data.frame produced by
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
      values = subset(labels_and_colors,column==var)$RGB,
      limits=c(subset(labels_and_colors,column==var)$variable),
      labels=c(subset(labels_and_colors,column==var)$Label)
    )+scale_fill_manual(
      values = subset(labels_and_colors,column==var)$RGB,
      limits=c(subset(labels_and_colors,column==var)$variable),
      labels=c(subset(labels_and_colors,column==var)$Label)
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

format_data_for_plot <- function(
  # Returns data in the appropriate format for the user-specified plot
  #
  # Args:
  data,   # data to format for the plot, as a tibble
  fy_var,          # name of fiscal year variable, as string
  input           # shiny input object
  #
  # Returns:
  #   a tibble of formatted data
){

  shown_data <- data

  breakouts <- c(input$color_var, input$facet_var)
  breakouts <- breakouts[breakouts != "None"]

  # account for potential spaces in breakouts and fy_var
  if(grepl(" ", fy_var)) fy_var <- paste0("`", fy_var, "`")
  if(length(breakouts) >= 1){
    if(grepl(" ", breakouts[1])) breakouts[1] <- paste0("`", breakouts[1], "`")
  }
  if(length(breakouts) == 2){
    if(grepl(" ", breakouts[2])) breakouts[2] <- paste0("`", breakouts[2], "`")
  }
  # filter by year - see https://tinyurl.com/lm2u8xs
  shown_data %<>%
    filter_(paste0(fy_var, ">=", as.character(input$year[1]), "&", fy_var,
                   "<=", as.character(input$year[2])))

  # aggregate to the level of [fiscal year x breakouts]
  # the evaluation for dplyr::summarize_ was a pain in the ass to figure out;
  # see stack overflow at https://tinyurl.com/z82ywf3

  if(length(breakouts) == 0){
    shown_data %<>%
      group_by_(fy_var) %>%
      summarize_(
        sum_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var)))
  } else {
    shown_data %<>%
      group_by_(.dots = c(fy_var, breakouts)) %>%
      summarize_(
        sum_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var)))
  }

  names(shown_data)[which(names(shown_data) == "sum_val")] <- input$y_var

  #
  # NOTE: NAs replaced with 0 here; potential data quality issue
  #
  shown_data[is.na(shown_data)] <- 0

  # calculate shares if share checkbox is checked
  if(input$y_total_or_share == "As Share" & input$color_var != "None"){

    # share_vars indicates which columns are being used to calculate the shares.
    # If there's only one breakout, it's set to -1:
    # "everything except fiscal year."
    # With two breakouts, it's set to c(-1, -2):
    # "everything except fiscal year and the facet variable."
    share_vars <- c(-1, -length(breakouts))

    # spread the shares breakout variable across multiple columns
    shown_data %<>%
      spread_(input$color_var, input$y_var)

    #
    # NOTE: NAs replaced with 0 here; potential data quality issue
    #
    shown_data[is.na(shown_data)] <- 0

    # calculate a total for each row - i.e. the total for the shares breakout
    # variable for each fiscal year,
    # or for each [fiscal year x facet variable] combo
    shown_data$total <- rowSums(shown_data[share_vars])

    # divide each column by the total column, to get each column as shares
    shown_data[share_vars] <-
      sapply(shown_data[share_vars], function(x){x / shown_data$total})
    shown_data %<>% select(-total)

    # gather the data back to long form
    shown_data <- gather_(
      data = shown_data,
      key_col = input$color_var,
      value_col = input$y_var,
      gather_cols = names(shown_data[share_vars])
    )
  }

  # For the case where the user displays shares not broken out by any variable.
  # This is going to make a very boring chart of 100% shares,b
  # but it's handled here to avoid displaying an error.
  if(input$y_total_or_share == "As Share" & input$color_var == "None"){
    shown_data %<>%
      mutate(total = 1)
    shown_data <- shown_data[which(names(shown_data) != input$y_var)]
    names(shown_data)[which(names(shown_data) == "total")] <- input$y_var
  }

  # return the ggplot-ready data
  return(shown_data)
}


rename_value <- function(
  # Renames a factor level to user-specified name, in the passed data frame
  #
  # Args:
  data,    # the data frame in which to rename the value
  input    # shiny input object
  #
  # Returns: a data frame with the factor level renamed
){
  levels(data[[input$edit_var]])[levels(data[[
    input$edit_var]]) == input$edit_value] <- input$rename_value_txt

  return(data)
}



build_plot_from_input <- function(
  # Adds a geom layer to a ggplot object based on user input.
  # Intended to handle ggplot settings that depend on user input.
  # Settings that apply universally should be added in server.R
  #
  # Args:
  plot_data,    # tibble of formatted data for the ggplot
  input        # shiny input object
  #
  # Returns:
  #   A ggplot object including user-specified geom layer
){

  mainplot <- ggplot(data = plot_data)

  # add a line layer, broken out by color if requested
  if(input$chart_geom == "Line Chart"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var)
        ))
    } else {
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var),
          color = as.name(input$color_var)
        )) +
        guides(color = guide_legend(override.aes = list(size = 1)))+
        theme(legend.key = element_rect(fill = "white"))
    }
  }

  # add a bar layer, broken out by color if requested
  if(input$chart_geom == "Bar Chart"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var)
        ),
        stat = "identity")
    } else {
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var),
          fill = as.name(input$color_var)
        ),
        stat = "identity")
    }
  }

  # add faceting if requested, and x-axis labeling
  if(input$facet_var != "None"){
    mainplot <- mainplot +
      facet_wrap(as.formula(paste0("~ `",input$facet_var, "`"))) +
      theme(strip.background = element_rect(fill = "white")) +
      scale_x_continuous(
        breaks = function(x) {seq(input$year[1], input$year[2], by = 2)},
        labels = function(x){str_sub(as.character(x), -2, -1)}
      )
  } else {
    mainplot <- mainplot +
      scale_x_continuous(
        breaks = function(x){seq(input$year[1], input$year[2], by = 1)},
        labels = function(x){str_sub(as.character(x), -2, -1)}
      )
  }

  # add y-axis labeling
  if(input$y_total_or_share == "As Share"){
    mainplot <- mainplot + scale_y_continuous(labels = scales::percent) +
      ylab(label = paste("Share of", input$y_var))
  } else {
    mainplot <- mainplot + scale_y_continuous(
      labels = function(x){
        sapply(x, function(y){
          if(is.na(y)) return("NA")
          y_lab <- "yuge"
          if(abs(y) < 1e15) y_lab <- paste0(round(y/1e12), "T")
          if(abs(y) < 1e14) y_lab <- paste0(round(y/1e12, 1), "T")
          if(abs(y) < 1e13) y_lab <- paste0(round(y/1e12, 2), "T")
          if(abs(y) < 1e12) y_lab <- paste0(round(y/1e9), "B")
          if(abs(y) < 1e11) y_lab <- paste0(round(y/1e9, 1), "B")
          if(abs(y) < 1e10) y_lab <- paste0(round(y/1e9, 2), "B")
          if(abs(y) < 1e9) y_lab <- paste0(round(y/1e6), "M")
          if(abs(y) < 1e9) y_lab <- paste0(round(y/1e6, 1), "M")
          if(abs(y) < 1e7) y_lab <- paste0(round(y/1e6, 2), "M")
          if(abs(y) < 1e6) y_lab <- paste0(round(y/1000), "k")
          if(abs(y) < 1e5) y_lab <- paste0(round(y/1000, 1), "k")
          if(abs(y) < 1e4) y_lab <- paste0(round(y/1000, 2), "k")
          if(abs(y) < 1000) ylab <- as.character(round(y))
          if(abs(y) < 100) y_lab <- as.character(round(y,1))
          if(abs(y) < 10) y_lab <- as.character(round(y,2))
          return(y_lab)
        })
      }
    )
  }

  # return the plot to server.R
  return(mainplot)
}
