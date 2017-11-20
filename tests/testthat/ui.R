################################################################################
# FPDS breakdowns 2.0 app - March 2017
# V4 Add stacked plots and drawdown period lines - Oct 2017
# ui.R
################################################################################

library(shiny)
library(shinyBS)
library(dplyr)
library(ggplot2)

shinyUI(fluidPage(

  sidebarLayout(
    sidebarPanel(
      titlePanel("FPDS Charts"),
      wellPanel(
        sliderInput(
          inputId ="year",
          label = "Years",
          min = 2000,
          max = 2016,
          sep = "",
          value = c(2000,2016)
        )
      ),
      wellPanel(
        radioButtons(
          inputId = "chart_geom",
          label = NULL,
          choices = c("Bar Chart", "Line Chart","Double Stacked","Period Stacked"),
          selected = "Line Chart"
        ),
        selectInput(
          inputId = "color_var",
          label = "Breakout",
          choices = "None",
          selected = "None",
          width = "100%",
          selectize = TRUE
        ),
        selectInput(
          inputId = "facet_var",
          label = "Facet",
          choices = "None",
          selected = "None",
          width = "100%",
          selectize = TRUE
        )
      ),
      wellPanel(
        selectInput(
          inputId = "y_var",
          label = "Y Variable",
          choices = "Action.Obligation.2016",
          selected = "Action.Obligation.2016",
          width = "100%",
          selectize = TRUE
        ),
        conditionalPanel(
          condition = "input.chart_geom == 'Bar Chart' | input.chart_geom == 'Line Chart'",
          radioButtons(
            inputId = "y_total_or_share",
            label = NULL,
            choices = c("As Total", "As Share"),
            selected = "As Total"
          )
        )
      ),
      wellPanel(
        radioButtons(
          inputId = "show_period",
          label = "Show Drawdown Periods",
          choices = c("Yes", "No"),
          selected = "No"
        )
      )
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "current_tab",
        tabPanel(
          "Charts",
          plotOutput("plot"),
          br(),
          br(),
          fluidRow(
            column(
              width = 5,
              wellPanel(
                downloadButton(
                  outputId = "download_image",
                  label = "Save Plot as PNG (300 DPI)",
                  width = "100%"
                ),
                br(),
                br(),
                fluidRow(
                  column(
                    width = 6,
                    p("Height (inches):")
                  ),
                  column(
                    width = 6,
                    numericInput(
                      inputId = "save_plot_height",
                      label = NULL,
                      value = 6,
                      min = 0.5,
                      max = 20.5,
                      step = 0.5
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    p("Width (inches):")
                  ),
                  column(
                    width = 6,
                    numericInput(
                      inputId = "save_plot_width",
                      label = NULL,
                      value = 6,
                      min = 0.5,
                      max = 20.5,
                      step = 0.5
                    )
                  )
                )
              ),
              downloadButton(
                outputId = "download_plot",
                label = "Download Plotted Data",
                width = "100%"
              )
            ),
            column(
              width = 7,
              wellPanel(
                textInput(
                  inputId = "title_text",
                  label = "Plot title"
                ),
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput(
                      inputId = "show_title",
                      label = "Show title",
                      value = FALSE,
                      width = "100%"
                    )
                  ),
                  column(
                    width = 6,
                    bsButton(
                      inputId = "lock_title",
                      label = "Lock",
                      width = "100%",
                      type = "toggle"
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel("Edit Data",
          fluidRow(
            column(
              width = 6,
              bsButton(
                inputId = "apply_changes_btn",
                label = "Apply Data Changes",
                style = "warning",
                size = "large",
                block = TRUE
              ),
              bsButton(
                inputId = "discard_btn",
                label = "Discard Data Changes",
                style = "primary",
                block = TRUE
              ),
              bsButton(
                inputId = "restore_btn",
                label = "Restore Original Data",
                style = "primary",
                size = "default",
                block = TRUE
              ),
              br(),
              tags$div(id = "edit_var_placeholder"),
              textInput(
                inputId = "rename_var_txt",
                label = NULL
              ),
              bsButton(
                inputId = "rename_var_btn",
                label = "Rename",
                style = "info"
              ),
              br(),
              br(),
              downloadButton(
                outputId = "download_current",
                label = "Download this view"
              )
            ),
            column(
              width = 6,
              bsButton(
                inputId = "csv_btn",
                label = "Switch to Uploaded Data",
                style = "success",
                size = "large",
                block = TRUE
              ),
              fileInput(
                "file_upload",
                "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
              ),
              tags$div(id = "edit_value_placeholder"),
              textInput(
                inputId = "rename_value_txt",
                label = NULL
              ),
              fluidRow(
                column(
                  width = 5,
                  bsButton(
                    inputId = "rename_value_btn",
                    label = "Rename",
                    style = "info",
                    block = TRUE
                  ),
                  bsButton(
                    inputId = "color_value_btn",
                    label = "Color",
                    block = TRUE
                  )
                ),
                column(
                  width = 5,
                  bsButton(
                    inputId = "drop_value_btn",
                    label = "Drop",
                    block = TRUE
                  ),
                  bsButton(
                    inputId = "keep_value_btn",
                    label = "Keep",
                    block = TRUE
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "move_up_value_btn",
                    label = "△",
                    block = TRUE
                  ),
                  bsButton(
                    inputId = "move_down_value_btn",
                    label = "▽",
                    block = TRUE
                  )
                )
              )
            )
          )
        )
      )
    )
  )
))
