#' Create a function to jitter the binary outcome (p89)
#'
#' @param a column with values of 0 or 1
#' @param jitt amount of horizontal and vertical jitter, value between 0 and .5; defaults to .05
#'
#' @return Takes a binary variable and randomly spreads it within 0 and 1
#'
#' @details From Andrew Gelman and Jennifer Hill. For use in graphs
#' to show the quantity of variable that otherwise would be stuck
#' on a single lines at 0 and 1. This is different from geom_jitter
#' because it keeps all the data within the possible range of the variable,
#' not allowing values greater than 1 or less than 0.
#'
#' @examples jitter_binary(contract$b_CBre)
#'
#' @import ggplot2
#' @export
jitter_binary<-function(a, jitt = 0.05){
  if_else(a==0,runif(length(a),0,jitt),runif(length(a),1-jitt,1))
}



#' Get Plot Theme
#'
#' @return Thematic elements that can be added to a plot.
#'
#' @details A standard set of style elements that is applied
#' across csis360 plots by default. These can be overriden,
#' by adding different theme elements after adding this to
#' your plot.
#'
#' @examples FullData<-standardize_variable_names(Path,
#'   FullData)
#'
#' @import ggplot2
#' @import sysfonts
#' @export
get_plot_theme<-function(erase_legend_title=TRUE,blank_x_lines=TRUE){
  #Make sure Open Sans is available.
  # if(!"Open Sans" %in% sysfonts::font_families()) sysfonts::font_add_google("Open Sans")
  t<-theme(
    panel.background = element_rect(fill = "#F4F4F4"),
    strip.background = element_rect(fill ="#E0E0E0"),
    plot.background = element_rect(fill = "white", color="white"),
    panel.grid.major.y = element_line(size=.1, color="gray"),
    panel.grid.minor.y = element_line(size=.1, color="lightgray"),
    plot.margin = margin(t=0,r=0.25,b=0.1,l=0.1,"inches")
    )
  if(blank_x_lines==TRUE){
  t<-t+theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.ticks = element_blank()#,
    # text=element_text(family="Open Sans")
  )
  } else{
    t<-t+theme(
    panel.grid.major.x = element_line(size=.1, color="gray"),
    panel.grid.minor.x = element_line(size=.1, color="lightgray")
    )
  }
  t<-t+theme(plot.title = element_text(
    family = "Open Sans",
    color = "#554449",
    face="bold",
    margin=margin(0.1,0,0.1,0,"inch"),
    hjust = 0.5))
  t<-t+theme(axis.text.x = element_text(
    family = "Open Sans"#,
    # margin = margin(0,0,0,0)
    ))
  t<-t+theme(axis.text.y = element_text(
    family = "Open Sans",
    color ="#554449",
    margin = margin(0,5,0,0))) +
    theme(axis.title.x = element_text(
      face = "bold",
      color = "#554449",
      family = "Open Sans",
      # vjust = 7#,This was what was causing the x-axis numbering to overlap with the plot.
      margin = margin(5,0,0,5)))
  t<-t+theme(axis.title.y = element_text(
    face = "bold",
    color = "#554449",
    family = "Open Sans",
    margin = margin(0,15,0,0)))
  if(erase_legend_title==TRUE)
    t<-t+ theme(legend.title = element_blank())
  t<-t+ theme(legend.text = element_text(
                family = "Open Sans",
                color ="#554449",
                margin = margin(2,2,2,2)),
              legend.position = 'bottom',
              legend.background = element_rect(fill = "white"),
              legend.margin=margin(0,0,0,0)
          # legend.margin = margin(t=-0.75, unit="cm")
  )+
    theme(plot.caption = element_text(size=8,
                                      family = "Open Sans",
                                      face = "italic",
                                      vjust= -0.15,
                                      color = "#003366"))
  return(t)
}



#' Adds a geom layer to a ggplot object based on user input.
#'
#' @param data A tibble of formatted data for the ggplot
#' @param chart_geom choose from Line Chart or Bar Chart; defaults to Line Chart
#' @param share If TRUE, calculates the share
#' @param x_var The name of fiscal year variable, as string
#' @param y_var The name of variable to plot on y-axis
#' @param color_var The name of the coloration variable, as string
#' @param facet_var The name of facet variable, as string
#' @param second_var The name of the secondary facet variable, as string
#' @param legend If TRUE, includes a legend
#' @param caption If TRUE, includes a source caption
#' @param labels_and_colors A csis360 lookup data.frame with factor information
#' @param column_key A csis360 lookup data.frame with column information
#' @param format If TRUE, summarize the data.frame
#' @param ytextposition If TRUE, add ytextposition to allow for geom_text overlays.
#' @param alpha_var Variable for setting the transparency of bars or line type of lines, coded to be used with year to ate.
#' @param invert_bool Used to create population pyramid or import/export charts, specifies when to show data in the negative space of the y-axis.
#' @param suppress_x_var_newline For categorical x-axis variables, remove any new line from the character string
#' @param hide_unlabeled If true, check that unlabeled variables are 1% of the date or less and if so hide them.
#'
#'
#' @return A ggplot object including user-specified geom layer
#'
#' @details Intended to handle ggplot settings that depend on user input.
#'Settings that apply universally should be added in server.R.
#'
#'
#' @import ggplot2
#' @import stringr
#' @import scales
#' @import sysfonts
#' @export
build_plot <- function(
  data,
  chart_geom = "Line Chart",
  share = FALSE,
  x_var=NULL,
  y_var="None", #Name of variable to plot on y-axis
  color_var="None",       # name of coloration variable, as string
  facet_var="None",        # name of facet variable, as string
  second_var=NULL,        # name of facet variable, as string
  legend=TRUE, #Include a legend
  caption=TRUE, #Include a source caption
  labels_and_colors=NULL,
  column_key=NULL,
  format=FALSE,
  ytextposition=FALSE,
  first_color_on_bottom=TRUE,
  alpha_var=NULL,
  invert_bool=NULL,
  suppress_x_var_newline=TRUE,
  hide_unlabeled=FALSE
){
  if(all(!is.null(second_var),facet_var==second_var | second_var=="None")) second_var<-NULL
  #To add, check for missing labels and colors
  if(format==TRUE)
    data <- format_data_for_plot(data=data,
                                 share=share,
                                 fy_var=x_var,
                                 # start_fy=NULL,
                                 # end_fy=NULL,
                                 y_var=y_var,
                                 color_var=color_var,
                                 facet_var=facet_var,
                                 second_var=second_var,
                                 alpha_var=alpha_var,
                                 invert_bool=invert_bool,
                                 labels_and_colors=labels_and_colors,
                                 add_ytextposition=ytextposition,
                                 suppress_x_var_newline=suppress_x_var_newline)

  #Nested if because its evaluating the second statement even if the first is false.
  if(color_var!="None")
    if(all(is.na(data[,color_var]))) stop("Missing color_var after formatting")
  if(facet_var!="None")
    if(all(is.na(data[,facet_var]))) stop("Missing facet_var after formatting")
  if(ytextposition==TRUE & format==FALSE) stop("ytextposition requires format=TRUE")
  if(all(is.na(data[,x_var]))) stop("Missing x_var after formatting")

  if(hide_unlabeled){
    y_var_total=sum(data[,y_var],na.rm=TRUE)
    labeled_total<-data[!is.na(data[,x_var]),]
    if(!is.Date(data[,x_var]))
      labeled_total<-labeled_total[labeled_total[,x_var]!="Unlabeled",]
    if(color_var!="None")
      labeled_total<-labeled_total[!is.na(data[,color_var])& labeled_total[,color_var]!="Unlabeled",]
    if(facet_var!="None")
      labeled_total<-labeled_total[!is.na(labeled_total[,facet_var])& labeled_total[,facet_var]!="Unlabeled",]
    if(!is.null(second_var))
      labeled_total<-labeled_total[!is.na(labeled_total[,second_var])& labeled_total[,second_var]!="Unlabeled",]
    if(!is.null(alpha_var))
      labeled_total<-labeled_total[!is.na(labeled_total[,alpha_var])& labeled_total[,alpha_var]!="Unlabeled",]
    if(sum(labeled_total[,y_var],na.rm = TRUE)<0.99 * y_var_total)
      stop("More than 1 percent is unlabeled")
    data<-labeled_total
    rm(labeled_total)
  }
  #Legacy bug fix. The sorting in some labels_and_colors is off  because display.order was a factor/character, not a number.
  if(!is.null(labels_and_colors) & !is.numeric(labels_and_colors$Display.Order)){
    labels_and_colors$Display.Order<-as.numeric(as.character(labels_and_colors$Display.Order))
    labels_and_colors<-labels_and_colors[order(labels_and_colors$column,labels_and_colors$Display.Order),]
  }
  #Primarily for bar plots, we want the first in order on the bottom so it is easier to track movements.
  if(first_color_on_bottom & !is.null(labels_and_colors)){
    labels_and_colors$Display.Order[labels_and_colors$column==color_var]<-
      -1*labels_and_colors$Display.Order[labels_and_colors$column==color_var]
    labels_and_colors<-labels_and_colors[order(labels_and_colors$column,labels_and_colors$Display.Order),]
  }
  data<-as.data.frame(data)
  mainplot <- ggplot(data = data)
  #Assume 1st row if no x_var provided.
  if(is.null(x_var)) x_var<-names(data)[1]

  # add a line layer, broken out by color if requested
  if(chart_geom == "Line Chart"){
    #There must be a better way to do this than 4 branches to cover variable nulls
    #Line plots need duplication, otherwise the YTD year is left out because it's only one point.
    if(!is.null(alpha_var)){
      if(alpha_var=="YTD" & x_var %in% c("Fiscal_Year","Mixed_Year","dFYear")){
        max_full_year<-max(data[data$YTD=="Full Year",x_var])
        data<-rbind(data,
                    data[data[,x_var]==max_full_year,] %>%
                      mutate(YTD="YTD"))
      }
        #Need to renew the data link here.
        if(color_var!="None"){
          data$coloralpha<-paste(data[,color_var],data[,alpha_var])
          group_var<-"coloralpha"
        }
        mainplot <- ggplot(data = data)
    }
    if(color_var == "None"){
      if(is.null(alpha_var))
        mainplot <- mainplot +
          geom_line(aes_q(
            x = as.name(x_var),
            y = as.name(y_var)))
      else{
        mainplot <- mainplot +
          geom_line(aes_q(
            x = as.name(x_var),
            y = as.name(y_var),
            group = as.name(alpha_var),
            linetype= as.name(alpha_var)
          ))
        if(alpha_var=="YTD"){
          mainplot<-mainplot+scale_linetype_discrete(breaks=c(1,3),labels=c("Full Year","YTD"))+
            theme(axis.text.x = element_text(margin = margin(t = 1, unit = "pt")))
        }
      }
    } else {
      if(is.null(alpha_var)){
        mainplot <- mainplot +
          geom_line(aes_q(
            x = as.name(x_var),
            y = as.name(y_var),
            color = as.name(color_var)
          ))+
          guides(color = guide_legend(override.aes = list(size = 1)))+
          theme(legend.key = element_rect(fill = "white"))
      }
      else{
        mainplot <- mainplot +
          geom_line(aes_q(
            x = as.name(x_var),
            y = as.name(y_var),
            color = as.name(color_var),
            # alpha = as.name(alpha_var)
            linetype= as.name(alpha_var)
          ))+
          guides(color = guide_legend(override.aes = list(linewidth = 1)))+
          theme(legend.key = element_rect(fill = "white"))
        if(alpha_var=="YTD"){
          mainplot<-mainplot+scale_linetype_discrete(breaks=c(1,3),labels=c("Full Year","YTD"))+
            theme(axis.text.x = element_text(margin = margin(t = 1, unit = "pt")))
        }
      }
    }
  }

  # add a bar layer, broken out by color if requested
  else if(chart_geom == "Bar Chart"){
    #There must be a better way to do this
    if(color_var == "None"){
      if(is.null(alpha_var))
        mainplot <- mainplot +
          geom_bar(aes_q(
            x = as.name(x_var),
            y = as.name(y_var)),
            stat = "identity")
      else{
        mainplot <- mainplot +
          geom_bar(aes_q(
            x = as.name(x_var),
            y = as.name(y_var),
            alpha = as.name(alpha_var)
          ),
          stat = "identity")
        if(alpha_var=="YTD"){
          mainplot<-mainplot+scale_alpha_ordinal(labels=c("Full Year","YTD"),range=c(1,0.5))+
            theme(axis.text.x = element_text(margin = margin(t = 1, unit = "pt")))
        }
      }
    } else {
      if(is.null(alpha_var))
        mainplot <- mainplot +
          geom_bar(aes_q(
            x = as.name(x_var),
            y = as.name(y_var),
            fill = as.name(color_var)
          ),
          stat = "identity")
      else{
        mainplot <- mainplot +
          geom_bar(aes_q(
            x = as.name(x_var),
            y = as.name(y_var),
            fill = as.name(color_var),
            alpha = as.name(alpha_var)
          ),
          stat = "identity")
        if(alpha_var=="YTD"){
          mainplot<-mainplot+scale_alpha_ordinal(labels=c("Full Year","YTD"),range=c(1,0.5))+
            theme(axis.text.x = element_text(margin = margin(t = 1, unit = "pt")))
        }
      }
    }
    if(!is.null(invert_bool)){
      mainplot<-mainplot+geom_hline(yintercept=0, colour = 'black', size=0.5, linetype='solid')
    }
  }


  else if(chart_geom=="Scatter Plot"){
    if(!is.null(second_var)) stop("Have not yet implemented second_var for scatter plot")
    if(color_var == "None"){
      mainplot<-mainplot+geom_point(
        aes_string(x_var,
                   y_var),
        position=position_jitter(width=0.15,height=0.15))
    }
    else{
      label<-subset(labels_and_colors,column==color_var)
      label<-label[,colnames(label) %in% c("Label","abbreviation")]
      colnames(label)[colnames(label)=="Label"]<-color_var
      data<-left_join(data,label)
      mainplot<-ggplot(data)+geom_text(
        aes_string(x_var,
                   y_var,
                   color=color_var,
                   label="abbreviation"),
        position=position_jitter(width=0.2,height=0.2))

    }
  }
  else if (chart_geom=="Histogram"){
    if(!is.null(second_var)) stop("Have not yet implemented second_var for histogram")
    #If no color, color by what you use to facet
    if(color_var=="None" & facet_var!="None") color_var<-facet_var
    if(color_var == "None"){

      if(is.numeric(data[,colnames(data)==x_var])&
        min(data[,colnames(data)==x_var])>=1 & max(data[,colnames(data)==x_var])<=7){
        mainplot<-mainplot+geom_histogram(aes_string(x=x_var),
                                          breaks=c(1:7),binwidth=1)
      }
      else{
        mainplot<-mainplot+geom_histogram(aes_string(x=x_var),binwidth=1)
      }
    }
    else{
      if(is.numeric(data[,colnames(data)==x_var])&
         min(data[,colnames(data)==x_var])>=1 & max(data[,colnames(data)==x_var])<=7){
        mainplot<-mainplot+geom_histogram(aes_string(x=x_var,fill=color_var),
                                          breaks=c(1:7))
      }
      else{
        mainplot<-mainplot+geom_histogram(aes_string(x=x_var,fill=color_var),binwidth=1)
      }
    }
  }
  else if (chart_geom=="Box and Whiskers"){
    #If no color, color by what you use to facet
    if(!is.null(second_var)) stop("Have not yet implemented second_var for box and whiskers")
    # browser()
    if(color_var == "None" | facet_var==color_var){
      data$Overall<-"Overall"
      if(facet_var!="None"){
      mainplot<-ggplot(data)+geom_boxplot(aes_string(y=y_var,
                                               x="Overall",
                                               fill=facet_var
                                               )
                                    )
      }
      else{
        mainplot<-ggplot(data)+geom_boxplot(aes_string(y=y_var,
                                                       x="Overall"
        )
        )
      }

    } else{
      mainplot<-mainplot+geom_boxplot(aes_string(y=y_var,
                                                 x=color_var,
                                                 fill=color_var)
                                      )

    }
  }
  else stop(paste("Unrecognized chart_geom",chart_geom))


  # add faceting if requested, and x-axis labeling
  if(facet_var != "None"){
    if(chart_geom!="Histogram"){
      if(is.null(second_var))
        mainplot <- mainplot +
          facet_wrap(as.formula(paste0("~ `",facet_var, "`"))) +
          theme(strip.background = element_rect(fill = "white"))
      else{
        mainplot <- mainplot +
          facet_grid(as.formula(paste0("`",facet_var, "` ~ `", second_var, "`"))) +
          theme(strip.background = element_rect(fill = "white"))
      }
      # theme(strip.background = element_rect(colour = "#554449", fill = "white", linewidth=0.5),
      #       panel.border = element_rect(colour = "#554449", fill=NA, linewidth=0.5))
    }
    else{
      mainplot <- mainplot +
        facet_wrap(as.formula(paste0("~ `",facet_var, "`")),ncol=1) +
        theme(strip.background = element_rect(fill = "white"))

    }
  }

  #For now, assuming numeric var_x means a year. Here we limit the data to only what
  # is covered in the year range passed by shiny, if any.
  if(chart_geom %in% c("Line Chart","Bar Chart"))
  if(is.numeric(data[,x_var])){
    start_fy<-min(data[,colnames(data)==x_var])
    end_fy<-max(data[,colnames(data)==x_var])
    # if(facet_var != "None"){
    #   mainplot<-mainplot+scale_x_continuous(
    #     breaks = function(x) {seq(start_fy, end_fy, by = 2)},
    #     labels = function(x){str_sub(as.character(x), -2, -1)}
    #   )
    # }
    # else{
    #   mainplot <- mainplot +
    #     scale_x_continuous(
    #       breaks = function(x){seq(start_fy, end_fy, by = 1)},
    #       labels = function(x){str_sub(as.character(x), -2, -1)}
    #     )
    # }
  }
  # add x-axis labeling
  if(any(class(data[,x_var])=="Date")){
    mainplot<-mainplot+scale_x_date(
      # breaks=scales::date_breaks("2 years"),
      #                 c(seq(
      #                     as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
      #                     as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
      #                     by=tick.marks)),
      labels=scales::date_format("'%y")
      #                 paste("'",format(as.Date(as.character(
      #                     c(seq(
      #                         as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
      #                         as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
      #                         by=tick.marks))
      #                 ),"%Y"),"%y"),sep="")
    )
  }

  # add y-axis labeling
  if(share == TRUE){
    mainplot <- mainplot + scale_y_continuous(labels = scales::percent) +
      ylab(label = paste("Share of", y_var))
  } else {
    # #If any entries are above 10 billion, change the scale to billions
    # if(max(shown_data[,y_var])>=1e10)
    #   mainplot<-mainplot+scale_y_continuous(label = unit_format(unit = "B", scale = 1e-9))
    # #If any entries are above 10 million, change the scale to millions
    # if(max(shown_data[,y_var])>=1e7)
    #   mainplot<-mainplot+scale_y_continuous(label = unit_format(unit = "M", scale = 1e-6))

    mainplot <- mainplot + scale_y_continuous(
      labels = label_units
    )
  }

  if(color_var!="None"){
    mainplot<-add_preassigned_scales(
      mainplot,
      labels_and_colors,
      var=color_var
      # reverse_color = reverse_color
    )
  }

  if(!is.null(column_key)){
    if(chart_geom=="Box and Whiskers")
      mainplot<-mainplot+labs(
        x=get_label(color_var,column_key),
        y=get_label(y_var,column_key,share)
      )
    else{
      mainplot<-mainplot+labs(
        x=get_label(x_var,column_key),
        y=get_label(y_var,column_key,share)
      )
    }
    if(chart_geom=="Histogram") mainplot<-mainplot+ylab("Count") #No y_var in a histogram.
  }

  # if(ytextposition){
  #   mainplot <- mainplot +
  #     geom_text(aes_string(label=y_var,
  #                            as.formula(paste0("format(round(",
  #                                           y_var,
  #                                           "3),  scientific=FALSE, trim=TRUE, big.mark=",")")),
  #                   x=x_var,
  #                   #                     format(round(y.variable,3),  scientific=FALSE, trim=TRUE, big.mark=",")
  #                   #                   format(y.variable, digits=1, drop0trailing=TRUE, trim=TRUE, big.mark=",")
  #                   #apply(y.variable,VariableNumericalFormat)
  #                   y="ytextposition"
  #     ),
  #     # size=geom.text.size,
  #     hjust=0.5,
  #     vjust=0.5
  #     #,color=color.list This doesn't work yet
  #     )
  # }

  # add overall visual settings to the plot
  mainplot <- mainplot +  get_plot_theme()
  #diigtheme1:::diiggraph()

  if(legend==FALSE)
    mainplot<-mainplot+theme(legend.position = "none")

  if(caption==TRUE)
    #+labs(y="",
    mainplot<-mainplot+labs(caption = "Source: FPDS and CSIS analysis."
    )


  # return the plot to server.R
  return(mainplot)
}


#' @title A legacy function for graphs that allows primary and secondary facets and
#'
#' @param VAR.color.legend.label Label for the fill legend
#' @param VAR.main.label Main title for the graph
#' @param VAR.X.label X-axis label
#' @param VAR.Y.label Y-axis label
#' @param VAR.Coloration A csis360 lookup data.frame with factor information
#' @param VAR.long.DF A data frame of formatted data for the ggplot
#' @param ncol Number of columns for a facet_wrap, doesn't apply if there's a secondary facet
#' @param VAR.x.variable The name of fiscal year variable, as string
#' @param VAR.y.variable The name of variable to plot on y-axis
#' @param VAR.y.series The name of the coloration variable, as string
#' @param VAR.facet.primary The name of the primary facet variable, as string. Defaults to NA.
#' @param VAR.facet.secondary The name of the secondary facet variable, as string. Defaults to NA.
#' @param MovingAverage Hww many units to include in a moving average, defaults to 1, i.e. no average.
#' @param MovingSides How to center the moving average.
#' @param DataLabels Whether to draw data labels on the graph, default=NA or no.
#' @param share If TRUE, calculates the share
#' @param caption If TRUE, includes a source caption, default is false.
#' @param legend If TRUE, includes a legend, default is false.
#' @param geom.text.size Size multipllier for text on the graph.
#'.
#'
#' @return A ggplot object following user-specified parameters.
#'
#' @details This should really be integrated into build_plot
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyverse
#' @import scales
#' @export
LatticePlotWrapper_csis360<-function(VAR.color.legend.label
                             ,VAR.main.label
                             ,VAR.X.label
                             ,VAR.Y.label
                             ,VAR.Coloration
                             ,VAR.long.DF
                             ,VAR.ncol=NA
                             ,VAR.x.variable
                             ,VAR.y.variable
                             ,VAR.y.series
                             ,VAR.facet.primary=NA
                             ,VAR.facet.secondary=NA
                             ,MovingAverage=1
                             ,MovingSides=1
                             ,DataLabels=NA
                             ,caption=FALSE
                             ,legend=FALSE
                             ,geom.text.size=2.5
                             #                       ,VAR.override.coloration=NA
){
  #     debug(PrepareLabelsAndColors)

  if("Graph" %in% names(VAR.long.DF)){
    VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    VAR.long.DF[,VAR.y.series]
  }
  if(is.na(VAR.y.series)) VAR.y.series<-VAR.facet.primary

  #Prepare labels for the category variable
  #   if(is.na(VAR.override.coloration)){
  labels.category.DF<-subset(VAR.Coloration,column==VAR.y.series)
  if(nrow(labels.category.DF)==0) stop(paste(VAR.y.series,"is missing from VAR.coloration."))
  VAR.long.DF<-as.data.frame(VAR.long.DF)
  VAR.long.DF[,VAR.y.series]<-factor(VAR.long.DF[,VAR.y.series],
                               levels=labels.category.DF$variable)

  #Input protection for when the y.series column is entirely na.
  if(all(is.na(VAR.long.DF[,VAR.y.series]))) stop(paste(VAR.y.series,"is entirely NA."))


  #Ordered Discrete X variables
  labels.x.DF<-subset(VAR.Coloration,column==VAR.x.variable)
  if(nrow(labels.x.DF)>0){
    VAR.long.DF<-as.data.frame(VAR.long.DF)
    VAR.long.DF[,VAR.x.variable]<-factor(VAR.long.DF[,VAR.x.variable],
                                         levels=labels.x.DF$variable)
  }
  #   }
  #   else{
  #     labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
  #                                       ,VAR.long.DF
  #                                       ,VAR.override.coloration
  #     )
  #   }

  # color.list<-c(labels.category.DF$RGB)
  # names(color.list)<-c(labels.category.DF$variable)



  old.theme<-theme_set(theme_grey())
  if(!is.na(VAR.facet.primary)){
    labels.primary.DF<-subset(VAR.Coloration,column==VAR.facet.primary)
    if(nrow(labels.primary.DF)>0){
      VAR.long.DF[,VAR.facet.primary]<-factor(VAR.long.DF[,VAR.facet.primary],
                                  levels=c(labels.primary.DF$variable),
                                  labels=c(labels.primary.DF$Label),
                                  ordered=TRUE)
    }

  }

  if(is.na(VAR.facet.primary)){
    VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                           , by=list(VAR.long.DF[,VAR.x.variable]
                                     ,VAR.long.DF[,VAR.y.series]
                           )
                           ,FUN = "sum"
                           ,na.rm =TRUE
    )
    names(VAR.long.DF)<-c("x.variable","category","y.variable")

    if(is.numeric(MovingAverage) & MovingAverage>1){
      VAR.long.DF$y.raw<-VAR.long.DF$y.variable
      VAR.long.DF<-ddply(VAR.long.DF,
                         .(category),
                         .fun=TransformFilter,
                         MovingAverage,
                         MovingSides
      )
      VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
    }

    VAR.long.DF<-VAR.long.DF %>% dplyr::group_by(x.variable) %>%
      dplyr::mutate(
                       ytextposition=sum(y.variable)-cumsum(y.variable)+0.33*y.variable,
                       cs=cumsum(y.variable),
                       rcs=sum(y.variable)-cumsum(y.variable))#.(Fiscal.Year)


  }
  #Reduce the number of rows by aggregating to one row per unique entry in the VAR.facet.primary column.
  else if(is.na(VAR.facet.secondary)){
    # VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
    #                        , by=list(VAR.long.DF[,VAR.x.variable]
    #                                  ,VAR.long.DF[,VAR.y.series]
    #                                  ,VAR.long.DF[,VAR.facet.primary]
    #                        )
    #                        ,FUN = "sum"
    #                        ,na.rm =TRUE
    # )
    VAR.long.DF<-VAR.long.DF %>% group_by(!! as.name(VAR.x.variable),
                                          !! as.name(VAR.y.series),
                                          !! as.name(VAR.facet.primary)) %>%
      dplyr::summarise_(
        y.variable = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(VAR.y.variable)))

    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.x.variable]<-"x.variable"
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.y.series]<-"category"
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.facet.primary]<-"primary"
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.y.variable]<-"y.variable"
    if(VAR.y.series==VAR.facet.primary){VAR.long.DF$primary<-VAR.long.DF$category}

    if(is.numeric(MovingAverage) & MovingAverage>1){
      VAR.long.DF$y.raw<-VAR.long.DF$y.variable
      VAR.long.DF<-ddply(VAR.long.DF,
                         .(category,primary),
                         .fun=TransformFilter,
                         MovingAverage,
                         MovingSides
      )
      VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
    }

    #For CumSum to work, make sure factor reordering happens first.
    VAR.long.DF<-VAR.long.DF %>%
                       dplyr::group_by(x.variable,primary) %>%
                       dplyr::mutate(
                       ytextposition=sum(y.variable)-cumsum(y.variable)+0.33*y.variable,#.(Fiscal.Year)
                       # ytextposition=cumsum(y.variable)-0.5*y.variable,
    cs=cumsum(y.variable),
    rcs=sum(y.variable)-cumsum(y.variable))#.(Fiscal.Year)

  }
  else{
    labels.secondary.DF<-subset(VAR.Coloration,column==VAR.facet.secondary)
    if(nrow(labels.secondary.DF)>0){
      VAR.long.DF[,VAR.facet.secondary]<-factor(VAR.long.DF[,VAR.facet.secondary]
                                                ,levels=c(labels.secondary.DF$variable)
                                                ,labels=c(labels.secondary.DF$Label)
                                                ,ordered=TRUE)
    }

    VAR.long.DF<-VAR.long.DF %>% dplyr::group_by(!! as.name(VAR.x.variable),
                                           !! as.name(VAR.y.series),
                                           !! as.name(VAR.facet.primary),
                                           !! as.name(VAR.facet.secondary)) %>%
      dplyr::summarise_(
        y.variable = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(VAR.y.variable)))

    #   aggregate(VAR.long.DF[,VAR.y.variable]
    #                        , by=list(VAR.long.DF[,VAR.x.variable]
    #                                  ,VAR.long.DF[,VAR.y.series]
    #                                  ,VAR.long.DF[,VAR.facet.primary]
    #                                  ,VAR.long.DF[,VAR.facet.secondary]
    #                        )
    #                        ,FUN = "sum"
    #                        ,na.rm =TRUE
    # )
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.x.variable]<-"x.variable"
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.y.series]<-"category"
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.facet.primary]<-"primary"
    colnames(VAR.long.DF)[colnames(VAR.long.DF)==VAR.facet.secondary]<-"secondary"



    if(is.numeric(MovingAverage) & MovingAverage>1){
      VAR.long.DF$y.raw<-VAR.long.DF$y.variable
      VAR.long.DF<-ddply(VAR.long.DF,
                         .(category,primary,secondary),
                         .fun=TransformFilter,
                         MovingAverage,
                         MovingSides
      )
      VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
    }



    #For CumSum to work, make sure factor reordering happens first.
    VAR.long.DF<-VAR.long.DF %>%
                       dplyr::group_by(x.variable,primary,secondary) %>%
                       dplyr::mutate(
                         ytextposition=sum(y.variable)-cumsum(y.variable)+0.33*y.variable,#.(Fiscal.Year)
    # ytextposition=cumsum(y.variable)-0.5*y.variable,
    cs=cumsum(y.variable),
    rcs=sum(y.variable)-cumsum(y.variable))#.(Fiscal.Year)



    rm(labels.secondary.DF)

  }




  original<-ggplot(
    aes_string(x="x.variable"
               , y="y.variable"
               , fill="category")
    , data=VAR.long.DF

  )+ geom_bar(stat="identity")+
    xlab(VAR.X.label)+
    ylab(VAR.Y.label)+
    ggtitle(VAR.main.label, subtitle = NULL)+
    get_plot_theme(erase_legend_title = FALSE)





  tick.marks<-2
  print.figure<-original


  if(class(VAR.long.DF$x.variable)=="Date"){
    print.figure<-print.figure+scale_x_date(
      breaks=scales::date_breaks("2 years"),
      #                 c(seq(
      #                     as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
      #                     as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
      #                     by=tick.marks)),
      labels=scales::date_format("'%y")
      #                 paste("'",format(as.Date(as.character(
      #                     c(seq(
      #                         as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
      #                         as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
      #                         by=tick.marks))
      #                 ),"%Y"),"%y"),sep="")
    )
  }

  #   print.figure<-print.figure+geom_bar(
  #     colour="black",
  #     stat = "identity",
  #     property= "identity"
  #   )

  #, labels=c(labels.category.DF$Label) Section labels don't work with facets.
  #  http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/

  print.figure<-print.figure+scale_fill_manual(
    VAR.color.legend.label
    ,  values=c(labels.category.DF$RGB)
    , limits=c(labels.category.DF$variable)
    , labels=c(labels.category.DF$Label)

  )

  #Don't add numbers at all if there's over 10 facets or 500 rows
  if(isTRUE(DataLabels) |
     (is.na(DataLabels) &
      length(levels(VAR.long.DF$primary))<=10 & nrow(VAR.long.DF)<200
     )){
    #Drop the labeling detail for crowded graphs.
    NumericalDetail<-1
    if(nrow(VAR.long.DF)>50){ NumericalDetail<-0 }
    print.figure<-print.figure+
      geom_text(aes(label=VariableNumericalFormat(y.variable,NumericalDetail)
                    #                     format(round(y.variable,3),  scientific=FALSE, trim=TRUE, big.mark=",")
                    #                   format(y.variable, digits=1, drop0trailing=TRUE, trim=TRUE, big.mark=",")
                    #apply(y.variable,VariableNumericalFormat)
                    ,y=ytextposition
      )
      ,hjust=0.5
      ,vjust=0
      ,size=geom.text.size
      #,color=color.list This doesn't work yet
      )
  }

  if(!is.na(VAR.facet.primary)){
    if(is.na(VAR.facet.secondary)){
      if(!is.na(VAR.ncol)){
        print.figure<-print.figure+facet_wrap(~ primary
                                              ,ncol=VAR.ncol
                                              #                                           , labeller=Label_Wrap
                                              #                                           , scales="fixed", space="free_y"
        )+scale_y_continuous(labels=comma)
      }
      else{
        print.figure<-print.figure+facet_wrap(~ primary
                                              #                                           ,ncol=VAR.ncol
                                              #                                           , labeller=Label_Wrap
                                              #                                           , scales="fixed", space="free_y"
        )+scale_y_continuous(labels=comma)
      }
      # +scale_y_continuous(expand=c(0,0.75)+
      #     )


    }
    else{
      print.figure<-print.figure+facet_grid(primary ~ secondary
                                            # , labeller=Label_Wrap
                                            , scales="free_y" #The scales actually do stay fixed
                                            , space="free_y"#But only because the space is free
      )+scale_y_continuous(expand=c(0,0.75)
                           ,labels=comma
      )+theme(strip.text.y=element_text(family="Open Sans",face="bold",angle=0)
      )

    }
  }

  #

  if(legend==FALSE)
    print.figure<-print.figure+theme(legend.position="none")

  print.figure<-print.figure
    theme(strip.text.x=element_text(face="bold"),
          axis.title.y=element_text(angle=90),
          legend.title=element_text(hjust=0)
          )
  #     theme(legend.key.width=unit(0.1,"npc"))
  #   print.figure<-facetAdjust(print.figure,"down")

    #If any entries are above 10 billion, change the scale to billions
    if(max(VAR.long.DF[,"y.variable"])>=1e10)
      print.figure<-print.figure+scale_y_continuous(label = unit_format(unit = "B", scale = 1e-9))
    #If any entries are above 10 million, change the scale to millions
    else if(max(VAR.long.DF[,"y.variable"])>=1e7)
      print.figure<-print.figure+scale_y_continuous(label = unit_format(unit = "M", scale = 1e-6))

if (caption==TRUE)
  print.figure<-print.figure+labs(caption = "Source: FPDS; CSIS analysis")

  print.figure


}


#' Extract a legend https://stackoverflow.com/questions/43366616/ggplot2-legend-only-in-a-plot
#' Alternate unused approach From https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @param filename Output file
#' @param gg A ggplot
#' @param width Output height as per ggsave
#' @param height Output height as per ggsave
#' @param units Output measurement unit as per ggsave
#' @param size New text size
#' @param caption_fraction Ratio of caption to the rest of text
#' @param path for output, defaults to NA for legacy reasons
#' @param second_path for output, typically to save to sharepoint
#' @return Does not return, outptus to file
#'
#' @import ggplot2
#' @export
ggsave600dpi<-function(filename,gg,width,height,units="in",size=12,lineheight=0.13,
                       caption_fraction=10/12,path=NA,
                       second_path=NA,...){

  if(!is.na(path)) path.file<-file.path(path,filename)
  else path.file<-filename
  ggsave(path.file, gg+
         theme(text=element_text(size=size,lineheight=lineheight),
               legend.spacing.x = unit(0.1, 'cm'),
               plot.caption = element_text(size=round(size * caption_fraction,0))
               ),
                 # font("xy.title", size = 45) +
                 # font("xy.text", size = 45) +
                 # font("legend.text", size = 45) +
                 # theme(text = element_text(size = 45+
                width=width, height= height, units=units,dpi=600,...)
  if(!is.na(second_path))
    ggsave600dpi(filename,gg,width,height,units,size,lineheight,
                           caption_fraction,path=second_path,...)
}


#' Automatically put an axis in a standardized unit.
#'
#' @param x A column of data
#'
#' @return Data in a standardized form.
#'
#'
#'
#'
#'
#' @import ggplot2
#' @export
label_units<-function(x){
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
