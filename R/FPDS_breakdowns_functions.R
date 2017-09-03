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
