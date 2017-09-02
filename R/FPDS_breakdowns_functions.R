################################################################################
# Functions for FPDS breakdowns 2.0 Shiny App - March 2017
#
################################################################################


populate_ui_var_lists <- function(
  # Fills the ui menus with appropriate variables from the tibble passed to it
  #
  # Args:
  data_source,    # tibble from which to populate the ui menus
  session = getDefaultReactiveDomain()  # shiny app session
  ){

  # get the class for each variable (except fiscal year)
  var_class <- sapply(data_source, class)

  # put numeric variables in the y_var list
  numerics <- names(data_source)[
    (var_class == "numeric" | var_class == "integer") & colnames(data_source)!="Fiscal.Year"]
  updateSelectInput(session, "y_var",
                    choices = numerics,
                    selected = "Action.Obligation.2016")

  # put categorical variables in the color_var and facet_var lists
  categories <- names(data_source)[var_class == "factor"]
  categories <- c("None", categories)
  updateSelectInput(session, "color_var", choices = categories)
  updateSelectInput(session, "facet_var", choices = categories)
}

format_data_for_plot <- function(
  # Returns data in the appropriate format for the user-specified plot
  #
  # Args:
  incoming_data,   # data to format for the plot, as a tibble
  fy_var,          # name of fiscal year variable, as string
  input,           # shiny input object
  session = getDefaultReactiveDomain()  # shiny app session
  #
  # Returns:
  #   a tibble of formatted data
  ){

  shown_data <- incoming_data

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



build_plot_from_input <- function(
  # Adds a geom layer to a ggplot object based on user input.
  # Intended to handle ggplot settings that depend on user input.
  # Settings that apply universally should be added in server.R
  #
  # Args:
  plot_data,    # tibble of formatted data for the ggplot
  input,        # shiny input object
  session = getDefaultReactiveDomain() # shiny app session
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


populate_edit_var <- function(
  # Populates the edit_var element on the edit page, based on the current data

  # Args:
  current_data,    # the current data for the app
  input,           # shiny input object
  session = getDefaultReactiveDomain() # shiny app session
  ){

  # insert the variable selection list
  insertUI(
    selector = "#edit_var_placeholder",
    ui = tags$div(
      selectInput(
        inputId = "edit_var",
        label = "Variables",
        choices = names(current_data),
        selected = names(current_data)[1],
        multiple = FALSE,
        selectize = FALSE,
        size = length(names(current_data))
      ),
      id = "edit_var_select"
    )
  )


  # update the variable renaming text box
  updateTextInput(
    session,
    inputId = "rename_var_txt",
    value = names(current_data)[1]
  )

}


create_edit_values_list <- function(
  # creates the list of values available for editing, when the user changes the
  # variable they are examining
  #
  # Args:
  current_data,  # current data frame in the app
  input,         # shiny input object
  session = getDefaultReactiveDomain()  # shiny session object
  ){


  edit_var_class <- class(unlist(
    current_data[which(names(current_data) == input$edit_var)]
  ))

  if(edit_var_class != "factor") {
    values_shown <- "*Not a Category Variable*"

    insertUI(
      selector = "#edit_value_placeholder",
      ui = tags$div(
        selectInput(
          inputId = "edit_value",
          label = "Values",
          choices = values_shown,
          multiple = FALSE,
          selectize = FALSE,
          size = 2
        ),
        id = "edit_value_select"
      )
    )
  } else {
    values_shown <- levels(unlist(
      current_data[which(names(current_data) == input$edit_var)]))

    insertUI(
      selector = "#edit_value_placeholder",
      ui = tags$div(
        selectInput(
          inputId = "edit_value",
          label = "Values",
          choices = values_shown,
          multiple = FALSE,
          selectize = FALSE,
          size = length(values_shown)
        ),
        id = "edit_value_select"
      )
    )
  }

  # update the rename text box
  updateTextInput(
    session,
    inputId = "rename_value_txt",
    value = values_shown[1]
  )

}


clear_edit_ui <- function(
  # removes the variable and value selection selectInputs from the Edit Data tab
  #
  # Args:
  input,    # shiny input object
  session = getDefaultReactiveDomain()  # shiny session object
  ){

  removeUI(
    selector = "#edit_value_select",
    multiple = TRUE,
    immediate = TRUE
  )

  removeUI(
    selector = "#edit_var_select",
    multiple = TRUE,
    immediate = TRUE
  )

}

drop_from_frame <- function(
  # filters out and drops factor levels from a factor in a data frame
  #
  # Args:
  passed_frame,    # the data frame, as an object
  passed_var,   # the name of the variable, as a string
  passed_levels,    # the name of the levels to drop, as a string
  session = getDefaultReactiveDomain()    # shiny session object
  #
  # Returns:
  #   The data frame with the factor level removed
){
  # stack overflow: https://tinyurl.com/mtys7xo
  passed_frame %<>%
    filter_(lazyeval::interp(~!val %in% passed_levels, val = as.name(passed_var)))

  passed_frame[[passed_var]] <- fct_drop(passed_frame[[passed_var]])

  return(passed_frame)
}




update_title <- function(
  # populates the title field with a dynamic title, if appropriate
  #
  # Args:
  passed_data,   # the data used in the plot
  input,    # shiny input object
  user_title,   # "None" unless the user has manually entered a title
  session = getDefaultReactiveDomain()   # shiny session object
  #

){
  if(user_title != "None") {
    updateTextInput(session, "title_text", value = user_title)
    return()
    }

  title <- input$y_var
  if(input$color_var != "None"){
    if(input$facet_var != "None"){
      title <- paste(
        title, "by", input$color_var, "and", input$facet_var)
    } else {
      title <- paste(title, "by", input$color_var)
    }
  } else if(input$facet_var != "None"){
    title <- paste(title, "by", input$facet_var)
  }

  # check for a single-level filter
  cats <- names(passed_data)[sapply(passed_data, class) == "factor"]
  for(i in seq_along(cats)){
    if(length(unique(passed_data[[i]])) == 1){
      title <- paste(unlist(unique(passed_data[[i]])), title)
    }
  }

  if(input$y_total_or_share == "As Total") title <- paste("Total", title)
  if(input$y_total_or_share == "As Share") title <- paste("Share of", title)

  updateTextInput(session, "title_text", value = title)

}
