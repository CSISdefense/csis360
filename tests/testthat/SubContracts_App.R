################################################################################
# Sequestration App
################################################################################
rm(list = ls())
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)
library(Cairo)
library(forcats)

################################################################################
# Read in and clean up data
################################################################################

# read in data
load("subcontract_full_data.RData")



# rename MilitaryHealth to have a space
#levels(full_data$SubCustomer.sum)[5] <- "Military Health"
# full_data$SubCustomer[full_data$Customer == "MilitaryHealth"] <- "Other DoD"

# make Big Five the first category (so it displays at the top of the legend)
# full_data$VendorSize <- relevel(full_data$VendorSize, "Big Five")

# save FY as 2 digits instead of 4, for better visual scale
#full_data$FY <- factor(substring(as.character(full_data$FY), 3, 4))


################################################################################
# Visual settings for user interface
################################################################################


# c("AllPrimes",
#               "PrimeReportInFSRS",
#               "SubReportInFSRS")

# here's the ui section - visual settings for the plot + widgets

ui <- fluidPage(


  ####CSS Import of Google Font "Open Sans" for body
  tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Open+Sans');

                  body {
                  font-family: 'Open Sans',  sans-serif;
                  font-weight: 500;
                  line-height: 1.1;
                  color: #554449;
                  }

                  ")),
  tags$head(
    tags$style(HTML("body{background-color: #fcfcfc;}"))),
  tags$div(HTML("<div class='fusion-secondary-header'>
                <div class='fusion-row'>
                <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:20px;'><a href='http://csis.org/program/international-security-program' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
                </div>
                </div>")),
  tags$style(HTML(".fusion-secondary-header {border-bottom: 3px solid #6F828F}")),
  br(),
  ####Copy below to change slider color
  tags$style(HTML(".irs-bar {background: #63c5b8}")),
  tags$style(HTML(".irs-bar {border-top: 1px #63c5b8}")),
  tags$style(HTML(".irs-bar {border-bottom: 1px #63c5b8}")),
  tags$style(HTML(".irs-single, .irs-to, .irs-from {background: #628582}")),
  #tags$style(HTML(".irs-slider {background: black}")),
  #  tags$style(HTML(".irs-grid-pol {display: absolute;}")),
  tags$style(HTML(".irs-max {color: #554449}")),
  tags$style(HTML(".irs-min {color: #554449}")),
  tags$style(HTML(".irs-bar-edge {border: 1px #63c5b8}")),
  tags$style(HTML(".irs-bar-edge {border-color: 1px #63c5b8}")),
  tags$style(HTML(".irs-bar-edge {border-color: 1px #63c5b8}")),
  ####

  fluidRow(

    # left column - column sizes should add up to 12, this one is 3 so
    # the other one will be 9
    column(3, align = 'center',
           br(),

           # year slider
           sliderInput('year', "Year Range:",
                       min = 2008, max = 2016,
                       value = c(2008,2016),
                       ticks = FALSE,
                       step = 1, width = '100%', sep = ""),

           selectInput("PlatformPortfolio","Platform Portfolio",
                       labels_and_colors$variable[labels_and_colors$column=="PlatformPortfolio"],
                       multiple = TRUE,
                       selectize = FALSE,
                       selected = labels_and_colors$variable[labels_and_colors$column=="PlatformPortfolio"],
                       width = '100%'),
#           selectInput("IsSubContract","Sub or Prime Contract",
#                       IsSubContract,
#                       multiple = TRUE,
#                       selectize = FALSE,
#                       selected = IsSubContract,
#                       width = '100%'),
          selectInput("SubCustomer.sum", "Customer",
                       labels_and_colors$variable[labels_and_colors$column=="SubCustomer.sum"],
                       multiple = TRUE,
                       selectize = FALSE,
                       selected = labels_and_colors$variable[labels_and_colors$column=="SubCustomer.sum"],
                       width = '100%')),

    # left column - column sizes should add up to 12, this one is 9 so
    # the other one will be 3
    column(9, align = "center",
           div(
             style = "position:relative",
             plotOutput("plot")
                      #  hover = hoverOpts(id = "plot_hover", delay = 30))
           )   #uiOutput("hover_info")
    )
 )
)


  # end of ui section

# server function starts

server <- function(input, output, session){


  ################################################################################
  # Subset data based on user input
  ################################################################################

  dataset <- reactive({
    #Test to prevent a confusing error code if we got the input wrong
    if(!is.data.frame(full_data))
      stop("full_data most be a data frame or tibble")

    ## subset by year, based on year slider ##

    # input$year[1] is the user-selected minimum year
    # input$year[2] is the user-selected maximum year
    # as.numeric(levels(FY))[FY] is just FY, converted from a factor to
    # a numeric variable


    shown <- filter(full_data, Fiscal.Year >= input$year[1] & Fiscal.Year <= input$year[2])


    ## subset data based on which categories the user selected ##

    # the selectInput widget holds the selected choices as a vector of
    # strings. This code checks whether the each observation is in the
    # selected categories, and discards it if isn't in all three.  The %in%
    # operator is a nice way to avoid typing lots of conditional tests all
    # strung together
    shown <- filter(shown, PlatformPortfolio %in% input$PlatformPortfolio &
#                      IsSubContract %in% input$IsSubContract &
                      SubCustomer.sum %in% input$SubCustomer.sum
    )

#    shown <- shown %>%
#      group_by(Fiscal.Year, Faceting) %>%
#      dplyr::summarise(Amount = sum(PrimeOrSubTotalAmount.2016))

    #shown <- shown %>%
    #  group_by(FY) %>%
    #  mutate(Percent = Amount / sum(Amount))

#    shown <- shown %>%
#      filter(!is.na(Faceting))


    #shown$VendorSize <- fct_reorder(
    #  shown$VendorSize,
    #  (shown$Percent * (shown$FY == input$year[2])) ,
    #  mean,
    #  na.rm = TRUE,
    #  .desc = TRUE)

    # return the subsetted dataframe to whatever called dataset()
    return(shown)

    # end of dataset() function
  })


  ################################################################################
  # Build the plot for output
  ################################################################################

  plotsettings <- reactive({

    # # calculate breaks for x axis
    # xbreaks <- rev(seq(
    #   from = input$year[2],
    #    to = input$year[1],
    #    by = -1 * ceiling((input$year[2] - input$year[1]) / 7)))
    #
    # xlabels <- as.character(xbreaks)
    shown<-dataset()


    shown_group<-group_data_for_plot(
      shown,
      "Fiscal.Year",
      "PrimeOrSubTotalAmount.2016",
      c("Faceting","IsFSRSreportable")
    )
    shown_top <-shown_group %>%
      group_by(Fiscal.Year, Faceting) %>%
      dplyr::summarise(PrimeOrSubTotalAmount.2016 = sum(PrimeOrSubTotalAmount.2016))

    shown_prime <- subset(shown_group,Faceting %in% c("PrimeNotReportInFSRS","PrimeReportInFSRS" )) %>%
      group_by(Fiscal.Year) %>%
      dplyr::summarise(PrimeOrSubTotalAmount.2016 = sum(PrimeOrSubTotalAmount.2016))

    shown_prime$Faceting<-"AllPrime"


    shown_reportable <- subset(shown_group,Faceting %in% c("PrimeNotReportInFSRS","PrimeReportInFSRS" ) &
                               IsFSRSreportable==1) %>%
      group_by(Fiscal.Year) %>%
      dplyr::summarise(PrimeOrSubTotalAmount.2016 = sum(PrimeOrSubTotalAmount.2016))

    shown_reportable$Faceting<-"Reportable"


    # ggplot call
    overview_plot <- ggplot(data = subset(shown_top,Faceting %in% c("SubReportInFSRS","PrimeReportInFSRS" )),
                aes(x = Fiscal.Year, y = PrimeOrSubTotalAmount.2016, color = Faceting)) +
      geom_line(size = 1) +
      ylab("Contract Obligations by whether in FSRS or is Subcontract") +
      # scale_y_log10()
    csis360::get_plot_theme()+
      geom_line(data = shown_prime,aes(color=Faceting))+
      geom_line(data = shown_reportable,aes(color=Faceting))

    percent_top <- shown_group  %>%
      group_by(Faceting, Fiscal.Year) %>%
      dplyr::summarise(PrimeOrSubTotalAmount.2016 = sum(PrimeOrSubTotalAmount.2016)) %>%
      reshape2::dcast(Fiscal.Year~Faceting,value.var="PrimeOrSubTotalAmount.2016" )
    percent_top$PrimeReportInFSRS[is.na(percent_top$PrimeReportInFSRS)]<-0
    percent_top$AllPrime<-percent_top$PrimeNotReportInFSRS+
      percent_top$PrimeReportInFSRS
    percent_top$PercentReported<-percent_top$PrimeReportInFSRS/
      percent_top$AllPrime
    percent_top$PercentSubAward<-percent_top$SubReportInFSRS/
      percent_top$AllPrime
    names(shown_reportable)[names(shown_reportable)=="PrimeOrSubTotalAmount.2016"]<-
      "PrimeReportable"
    percent_top<-plyr::join(percent_top,shown_reportable)
    percent_top$PercentReportable<-percent_top$PrimeReportable/
      percent_top$AllPrime

    # browser()
    percent_top<-reshape2::melt(percent_top,
                                id.vars="Fiscal.Year",
                      measure.vars=c("PercentReported","PercentSubAward","PercentReportable"),
                      variable.name="FSRS.series",
                      value.name="percent"
    )



    # ggplot call
    percent_plot <- ggplot(data = percent_top,
                            aes(x = Fiscal.Year, y = percent,
                                color=FSRS.series)) +
      geom_line(size = 1) +
      ylab("Market Share of Contrats Reporting in FSRS") +
      # scale_y_log10()
      csis360::get_plot_theme()+
      scale_y_continuous(labels = scales::percent)

      all_prime<-subset(shown,Faceting %in% c("PrimeNotReportInFSRS","PrimeReportInFSRS" ))
      all_prime$Faceting<-"AllPrime"
      reportable_prime<-subset(shown,Faceting %in% c("PrimeNotReportInFSRS","PrimeReportInFSRS" )&
                      IsFSRSreportable==1)
      reportable_prime$Faceting<-"PrimeReportable"
  prime_and_reportable<-rbind(all_prime,reportable_prime)

    prime_plot<- ggplot(data = prime_and_reportable,
                aes(x=Fiscal.Year,
                    y=PrimeOrSubTotalAmount.2016,
                    fill = Pricing.Mechanism.sum)) +
      geom_bar(width=.7,stat="identity") +
      ggtitle("Contract Obligations by whether in FSRS or is Subcontract")
    prime_plot<-add_preassigned_scales(prime_plot,labels_and_colors,"Pricing.Mechanism.sum")+
      #     # "AllPrimes" = "#33FF66",
      #     "PrimeNotReportInFSRS" =  "#33FF66",
      #     "PrimeReportInFSRS" =  "#0066FF",
      #     "SubReportInFSRS" = "#FF6699")) +
      csis360::get_plot_theme() +
      scale_x_continuous(breaks = seq(input$year[1], input$year[2], by = 1),
        labels = function(x) {substring(as.character(x), 3, 4)})+
      facet_wrap(~Faceting)+

      theme(legend.position = "none")+
      xlab("Fiscal Year") +
      ylab("DoD Contract Obligated Amount in billion $")
    ##############################################################################facet above





    in_fsrs_plot <- ggplot(data = subset(shown,Faceting %in% c("SubReportInFSRS","PrimeReportInFSRS" )),
      aes(x=Fiscal.Year,
        y=PrimeOrSubTotalAmount.2016,
        fill = Pricing.Mechanism.sum)) +
      geom_bar(width=.7,stat="identity") +
      facet_wrap(~ Faceting, ncol = 2,
        drop = TRUE) +
      #Fix these!
      scale_x_continuous(breaks = seq(input$year[1], input$year[2], by = 1),
        labels = function(x) {substring(as.character(x), 3, 4)})

    in_fsrs_plot<-add_preassigned_scales(in_fsrs_plot,labels_and_colors,"Pricing.Mechanism.sum")+
      # scale_fill_manual(
        # values = structure(as.character(Pricing.Mechanism.sum$ColorRGB), names = as.character(Pricing.Mechanism.sum$Label)))+
      # c(
      #     # "AllPrimes" = "#33FF66",
      #     "PrimeNotReportInFSRS" =  "#33FF66",
      #     "PrimeReportInFSRS" =  "#0066FF",
      #     "SubReportInFSRS" = "#FF6699")) +
      csis360::get_plot_theme() +

      xlab("Fiscal Year") +
      ylab("DoD Contract Obligated Amount in billion $") +
      theme(plot.caption = element_text(
        size = 12, face = "bold", color = "#554449", family = "Open Sans"
      )) +
      labs(caption = "Source: FPDS; CSIS analysis", size = 30, family= "Open Sans")
    ##############################################################################facet above
    lay <- rbind(c(1,2),
                 c(3,3),
                 c(4,4))
    grid.arrange(overview_plot,
                 percent_plot,
                 prime_plot,
                 in_fsrs_plot, layout_matrix = lay)

  })



  ################################################################################
  # Output the built plot and start the app
  ################################################################################


  output$plot <- renderPlot({
          plotsettings()},height = 700)

#  output$CSVDownloadBtn <- downloadHandler(
#    filename = paste('CSIS-Contract-Obligations-by-Vendor-Size-', Sys.Date(),'.csv', sep=''),
#    content = function(file) {
#      writedata <- dataset()
#      writedata$Percent <- writedata$Percent * 100
#      write.csv(writedata, file)
#    }
#  )


  # run full data download button
#  output$FullDownloadBtn <- downloadHandler(
#    filename = paste('CSIS.Contract Obligations by Vendor Size.', Sys.Date(),'.csv', sep=''),
#    content = function(file) {
#      writedata <- top_data
#      writedata <- select(writedata, FY, VendorSize, Customer, Category,
#                          PlatformPortfolio, Amount)
#      write.csv(writedata, file)
#    }
#  )

  # run displayed data download button
  #output$CSVDownloadBtn <- downloadHandler(
  #    filename = paste('DoD contract shares ', Sys.Date(),'.csv', sep=''),
  #    content = function(file) {
  #        writedata <- dataset()
  #        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
  #        writedata$Percent <- writedata$Percent * 100
  #        writedata <- select(writedata, FY, VendorSize, Amount, Percent)
  #        write.csv(writedata, file)
  #    }
  #)

  ##############################################################################
  # Give details when user hovers the plot
  # See https://gitlab.com/snippets/16220
  ##############################################################################


#  output$hover_info <- renderUI({
#    hover <- input$plot_hover

#    if(is.null(hover)) return(NULL)

#    switch(
#      input$Chart,
#      "Line" = {
#        point <- nearPoints(dataset(), hover, xvar = "FY", yvar = "Percent",
#                            threshold = (150 / (input$year[2] - input$year[1])) + 10,
#                            maxpoints = 1, addDist = TRUE)
#      },
#      "Bar" = {
#        point <- nearPoints(dataset(), hover, xvar = "FY", yvar = "Amount",
#                            threshold = 200,
#                            maxpoints = 1, addDist = TRUE)
#      }
#    )

#    if(nrow(point) == 0) return(NULL)

#    if(input$Chart == "Bar"){
#      year <- round(hover$x)
#      if(year < input$year[1] | year > input$year[2]) return(NULL)
#      if(hover$y < 0) return(NULL)

#      hov_amount <- dataset() %>%
#        filter(FY == year & VendorSize == point$VendorSize) %>%
#        .$Amount %>%
#        unlist

#      hov_percent <- dataset() %>%
#        filter(FY == year & VendorSize == point$VendorSize) %>%
#        .$Percent %>%
#        unlist

#      if(hover$y > hov_amount) return(NULL)
#    } else {
#      year <- point$FY
#      hov_amount <- point$Amount
#      hov_percent <- point$Percent
#    }

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
#    left_pct <- (hover$x - hover$domain$left) /
#      (hover$domain$right - hover$domain$left)
#    top_pct <- (hover$domain$top - hover$y) /
#      (hover$domain$top - hover$domain$bottom)

#     calculate distance from left and bottom side of the picture in pixels
#    left_px <- hover$range$left + left_pct *
#      (hover$range$right - hover$range$left)
#    top_px <- hover$range$top + top_pct *
#      (hover$range$bottom - hover$range$top)

    # Use HTML/CSS to change style of tooltip panel here
#    style <- paste0(
#      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
#      "left:", left_px + 2, "px; top:", top_px + 2, "px;")

#    wellPanel(
#      style = style,
#      p(HTML(paste0("<b> Fiscal Year: </b>", year, "<br/>",
#                    "<b> Vendor Size: </b>", point$VendorSize, "<br/>",
#                    "<b> Share: </b>", round(hov_percent*100,1), "%<br/>",
#                    "<b> Amount: </b> $",
#                    round(hov_amount,2),  " Billion")))
#    )
#  })
  # end of the server function
}



# starts the app
shinyApp(ui= ui, server = server)




