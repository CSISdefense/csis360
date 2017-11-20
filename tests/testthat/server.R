################################################################################
# FPDS breakdowns 2.0 app - March 2017
#
# server.R
################################################################################

library(shiny)
library(magrittr)
library(forcats)
library(Cairo)
library(shinyBS)
#library(diigtheme1)
library(stringr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(data.table)
library(csis360)
library(gridExtra)



shinyServer(function(input, output, session) {
  options(scipen = 99)
  options(shiny.maxRequestSize=1000*1024^2)
  # read data
  load(system.file("extdata",
    "2016_unaggregated_FPDS.Rda",
    package = "csis360"))

  original_data<-full_data
  # original_data <- read_csv("2016_unaggregated_FPDS.csv")

  # in case user renames the data-frame choosing variables
  vars <- reactiveValues(
    fiscal_year = "Fiscal.Year",
    user_title = "None")

  # create working copies of the data for user modification, while retaining
  # the original data in case the user wants to reset to it
  current_data <- original_data
  changed_data <- original_data

  # fill the variable lists in the ui with variables from current_data
  populate_ui_var_lists(current_data)

  mainplot <- reactive({
    # Builds a ggplot based on user settings, for display on the main panel.
    # Reactive binding will cause the ggplot to update when the user changes any
    # relevant setting.
    #
    # Returns:
    #   a fully built ggplot object

    # get appropriately formatted data to use in the plot
    total_data <- format_data_for_plot(data=current_data,
      share=FALSE,
      fy_var=vars$fiscal_year,
      start_fy=input$year[1],
      end_fy=input$year[2],
      y_var=input$y_var,
      color_var=input$color_var,
      facet_var=input$facet_var,
      labels_and_colors=labels_and_colors)
    share_data <- format_data_for_plot(data=current_data,
      share=TRUE,
      fy_var=vars$fiscal_year,
      start_fy=input$year[1],
      end_fy=input$year[2],
      y_var=input$y_var,
      color_var=input$color_var,
      facet_var=input$facet_var,
      labels_and_colors=labels_and_colors)

    # build plot with user-specified geoms
    if(input$chart_geom == "Double Stacked"){
      # make the stacked plot
      # produce the single bar plot and line plot
      bar_plot <-  build_plot(data=total_data,
        chart_geom="Bar Chart",
        share=FALSE,
        x_var=vars$fiscal_year,
        y_var=input$y_var,
        color_var=input$color_var,
        facet_var=input$facet_var,
        labels_and_colors=labels_and_colors,
        column_key=column_key,
        legend=FALSE,
        caption=FALSE)

      line_plot <- build_plot(data=share_data,
        chart_geom="Line Chart",
        share=TRUE,
        x_var=vars$fiscal_year,
        y_var=input$y_var,
        color_var=input$color_var,
        facet_var=input$facet_var,
        labels_and_colors=labels_and_colors,
        column_key=column_key)
      # lay the stacked plots
      lay <- rbind(c(1,1,1),
        c(1,1,1),
        c(1,1,1),
        c(2,2,2),
        c(2,2,2))
      grid.arrange(bar_plot,
        line_plot,
        layout_matrix = lay)

    } else {
      # make the bar plot or line plot (total or share)
      # set the dataset for plot
      if(input$y_total_or_share == "As Share"){
        plot_data <- share_data
      } else {plot_data <- total_data}
      # build bar plot or line plot
      mainplot <- build_plot(data=plot_data,
        chart_geom=input$chart_geom,
        share=FALSE,
        x_var=vars$fiscal_year,
        y_var=input$y_var,
        color_var=input$color_var,
        facet_var=input$facet_var,
        labels_and_colors=labels_and_colors,
        column_key=column_key)

      if(input$show_title == TRUE){
        mainplot <- mainplot + ggtitle(input$title_text)
      }

      # return the built plot
      mainplot
    } # END OF ELSE(bar or line plot)

  })

  # calls mainplot(), defined above, to create a plot for the plot output area
  output$plot <- renderPlot({
    mainplot()
  })

  # runs the download data button on the edit page
  output$download_current <- downloadHandler(
    filename = "edited_data_view.csv",
    content = function(file){
      write_csv(changed_data, file)
    }
  )

  # runs the download plot data button
  output$download_plot <- downloadHandler(
    filename = "plot_data.csv",
    content = function(file){
      if(input$chart_geom == "Double Stacked") {
        plotdata <- format_data_for_plot(data=current_data,
          share=FALSE,
          fy_var=vars$fiscal_year,
          start_fy=input$year[1],
          end_fy=input$year[2],
          y_var=input$y_var,
          color_var=input$color_var,
          facet_var=input$facet_var,
          labels_and_colors=labels_and_colors)

        sharedata <-   format_data_for_plot(data=current_data,
          share=TRUE,
          fy_var=vars$fiscal_year,
          start_fy=input$year[1],
          end_fy=input$year[2],
          y_var=input$y_var,
          color_var=input$color_var,
          facet_var=input$facet_var,
          labels_and_colors=labels_and_colors)

        joinkey <- names(sharedata)[1:ncol(sharedata)-1]
        plot_data <- left_join(plotdata, sharedata, by=joinkey)
        names(plot_data)[ncol(plot_data)] <- paste(input$y_var, ".Sharamout")
      } else{
        format_data_for_plot(data=current_data,
          share=ifelse(input$y_total_or_share == "As Share",TRUE,FALSE),
          fy_var=vars$fiscal_year,
          start_fy=input$year[1],
          end_fy=input$year[2],
          y_var=input$y_var,
          color_var=input$color_var,
          facet_var=input$facet_var,
          labels_and_colors=labels_and_colors)
      }
      write_csv(plot_data, file)
    }
  )

  # runs the download PNG button
  output$download_image <- downloadHandler(
    filename = "plot_image.png",
    content = function(file){
      ggsave(
        filename = file,
        plot = mainplot(),
        width = input$save_plot_width,
        height = input$save_plot_height,
        units = "in")
    }
  )

  # populate and depopulate ui elements when the user changes tabs
  observeEvent(input$current_tab, {
    if(input$current_tab == "Edit Data"){
      populate_edit_var(current_data, input)
      create_edit_values_list(current_data, input)
    } else {
      clear_edit_ui(input)
      populate_ui_var_lists(current_data)
      changed_data <<- current_data
    }
  })


  # change ui elements when the user selects a different variable in the edit tab
  observeEvent(input$edit_var, {
    # change the variable rename text box
    updateTextInput(
      session,
      inputId = "rename_var_txt",
      value = input$edit_var
    )
    # delete previous values edit box
    removeUI(selector = "#edit_value_select")
    # make a new values edit box
    create_edit_values_list(changed_data, input)
  })


  # drop values from all frames at user request
  observeEvent(input$drop_value_btn, {

    changed_data <<- changed_data %>%
      drop_from_frame(input$edit_var, input$edit_value)

    # update edit_value list to reflect dropped value
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(changed_data, input)
  })


  # discard all factor levels except the selected level, when the user clicks
  # the "keep" button
  observeEvent(input$keep_value_btn, {

    dropped <- unique(changed_data[[input$edit_var]])
    dropped <- dropped[dropped != input$edit_value]

    changed_data <<- changed_data %>%
      drop_from_frame(input$edit_var, dropped)

    # update edit_value list to reflect dropped value
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(changed_data, input)
  })

  # apply data changes on click of "apply changes" button
  observeEvent(input$apply_changes_btn, {
    current_data <<- changed_data
    updateTabsetPanel(
      session,
      inputId = "current_tab",
      selected = "Charts"
    )
    update_title(current_data, input, vars$user_title)
  })

  # discard data changes on click of "discard changes" button
  observeEvent(input$discard_btn, {
    changed_data <<- current_data
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(current_data, input)
  })

  # restore orginal data on click of "restore original data" button
  observeEvent(input$restore_btn, {
    changed_data <<- original_data
    current_data <<- original_data
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(current_data, input)
    update_title(current_data, input, vars$user_title)
    removeUI(selector = "#edit_var_select")
    populate_edit_var(changed_data, input)


  })

  # update title depending on variable selection
  observeEvent(input$color_var, {
    update_title(current_data, input, vars$user_title)
  })

  observeEvent(input$facet_var, {
    update_title(current_data, input, vars$user_title)
  })


  # tells the app to stop dynamically changing the title, when the lock title
  # button is activated
  observeEvent(input$lock_title, {
    if(input$lock_title) vars$user_title <- input$title_text
    if(!input$lock_title){
      vars$user_title <- "None"
      update_title(current_data, input, vars$user_title)
    }
  })

  # renames the selected variable to whatever is in the text box, when the user
  # clicks the variable rename button
  observeEvent(input$rename_var_btn, {
    if(input$rename_var_txt != "") {
      names(changed_data)[names(changed_data) == input$edit_var] <<-
        input$rename_var_txt

      if(input$edit_var == vars$fiscal_year) {
        vars$fiscal_year <- input$rename_var_txt
      }

      removeUI(selector = "#edit_var_select")
      populate_edit_var(changed_data, input)
      removeUI(selector = "#edit_value_select")
      create_edit_values_list(changed_data, input)
    }
  })

  # renames the selected factor level to whatever is in the text box, when
  # the user clicks the factor level rename button
  observeEvent(input$rename_value_btn, {
    if(input$rename_value_txt != "" &
        input$edit_value != "*Not a Category Variable*") {

      changed_data <<- rename_value(changed_data, input)

      if(input$edit_var == vars$fiscal_year) {
        vars$fiscal_year <- input$rename_var_txt
      }

      removeUI(selector = "#edit_value_select")
      create_edit_values_list(changed_data, input)
    }
  })

  # propagates the selected factor level's name to the rename textbox
  observeEvent(input$edit_value, {
    updateTextInput(
      session,
      inputId = "rename_value_txt",
      value = input$edit_value
    )
  })

  # accepts file upload
  observeEvent(input$csv_btn, {
    if(is.null(input$file_upload)) return(NULL)

    original_data <<- fread(
      input$file_upload$datapath,
      stringsAsFactors = TRUE,
      data.table = FALSE)

    vars$fiscal_year <- names(original_data)[1]

    if("Action.Obligation" %in% tolower(colnames(original_data))){
      sum_index <-
        which(tolower(colnames(original_data)) == "Action.Obligation")

      original_data <- deflate(original_data,
        fy_var = vars$fiscal_year,
        money_var = colnames(original_data)[sum_index]
      )

    }

    current_data <<- original_data
    changed_data <<- original_data

    clear_edit_ui(input)
    populate_edit_var(current_data, input)
    create_edit_values_list(current_data, input)

  })



})
