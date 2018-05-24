################################################################################
# Functions for FPDS breakdowns 2.0 Shiny App - March 2017
#
################################################################################

#' Fills the ui menus with appropriate variables from the tibble passed to it
#' @param data_source A tibble from which to populate the ui menus
#' @param session=getDefaultReactiveDomain()  The shiny app session
#'
#'
#'
#'
#'
#' @import
#' @export
populate_ui_var_lists <- function(
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
                    selected = "Action.Obligation.2017")

  # put categorical variables in the color_var and facet_var lists
  categories <- names(data_source)[var_class == "factor"]
  categories <- c("None", categories)
  updateSelectInput(session, "color_var", choices = categories)
  updateSelectInput(session, "facet_var", choices = categories)
}




#' Populates the edit_var element on the edit page, based on the current data
#' @param current_data The current data frame for the app
#' @param input  Shiny input object
#' @param session=getDefaultReactiveDomain() The shiny app session
#'
#'
#'
#'
#' @import
#' @export
populate_edit_var <- function(
  current_data,    # the current data frame for the app
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

#' Creates the list of values available for editing, when the user changes the variable they are examining
#' @param current_data The current data frame for the app
#' @param input  Shiny input object
#' @param session=getDefaultReactiveDomain() The shiny app session
#'
#'
#'
#'
#' @import
#' @export
create_edit_values_list <- function(
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


#' Removes the variable and value selection selectInputs from the Edit Data tab
#' @param input  Shiny input object
#' @param session=getDefaultReactiveDomain() The shiny app session
#'
#'
#'
#'
#' @import
#' @export
clear_edit_ui <- function(
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


#' Filters out and drops factor levels from a factor in a data frame
#' @param passed_frame A data frame, as an object
#' @param passed_var  A variable, as a string
#' @param passed_levels The levels to drop, as a string
#' @param session=getDefaultReactiveDomain() The shiny app session
#'
#' @return The data frame with the factor level removed
#'
#'
#'
#'
#' @import
#' @export
drop_from_frame <- function(
  passed_frame,    # the data frame, as an object
  passed_var,   # the name of the variable, as a string
  passed_levels,    # the name of the levels to drop, as a string
  session = getDefaultReactiveDomain()    # shiny session object
  #
){
  # stack overflow: https://tinyurl.com/mtys7xo
  passed_frame %<>%
    filter_(lazyeval::interp(~!val %in% passed_levels, val = as.name(passed_var)))

  passed_frame[[passed_var]] <- fct_drop(passed_frame[[passed_var]])

  return(passed_frame)
}



#' Populates the title field with a dynamic title, if appropriate
#' @param passed_data The data used in the plot
#' @param input  Shiny input object
#' @param user_title "None" unless the user has manually entered a title
#' @param session=getDefaultReactiveDomain() The shiny app session
#'
#'
#'
#'
#' @import
#' @export
update_title <- function(
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
