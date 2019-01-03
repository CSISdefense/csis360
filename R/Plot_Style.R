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
  ifelse(a==0,runif(length(a),0,jitt),runif(length(a),1-jitt,1))
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
#' @export
get_plot_theme<-function(){

  t<-theme(
    panel.background = element_rect(fill = "#F4F4F4"),
    strip.background = element_rect(fill ="#E0E0E0"),
    plot.background = element_rect(fill = "white", color="white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="gray"),
    panel.grid.minor.y = element_line(size=.1, color="lightgray"),
    axis.ticks = element_blank()
  )
  t<-t+theme(plot.title = element_text(
    family = "Open Sans",
    color = "#554449",
    face="bold",
    margin=margin(20,0,20,0),
    hjust = 0.5))
  t<-t+theme(axis.text.x = element_text(
    family = "Open Sans",
    # vjust = 7#,This was what was causing the x-axis numbering to overlap with the plot.
    margin = margin(0,0,0,0)
    ))
  t<-t+theme(axis.text.y = element_text(
    family = "Open Sans",
    color ="#554449",
    margin = margin(0,5,0,0))) +
    theme(axis.title.x = element_text(
      face = "bold",
      color = "#554449",
      family = "Open Sans",
      margin = margin(15,0,0,60)))
  t<-t+theme(axis.title.y = element_text(
    face = "bold",
    color = "#554449",
    family = "Open Sans",
    margin = margin(0,15,0,0)))
  t<-t+ theme(legend.title = element_blank())+
    theme(legend.text = element_text(
      family = "Open Sans",
      color ="#554449",
      margin = margin(2,2,2,2))) +
    theme(legend.position = 'bottom',
          legend.background = element_rect(fill = "white"),
          legend.margin = margin(t=-0.75, unit="cm")
          # +
    # theme(legend.spacing.x = unit(1.0, 'cm'))
    )+
    theme(plot.caption = element_text(size=8,
                                      face = "italic",
                                      vjust= -0.15,
          family = "Open Sans",
          color ="#003366"))
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
#' @param facet_var="None" The name of facet variable, as string
#' @param legend If TRUE, includes a legend
#' @param caption If TRUE, includes a source caption
#' @param labels_and_colors A csis360 lookup data.frame with factor information
#' @param column_key A csis360 lookup data.frame with column information
#'
#'
#'
#'
#' @return A ggplot object including user-specified geom layer
#'
#' @details Intended to handle ggplot settings that depend on user input.
#'Settings that apply universally should be added in server.R.
#'
#' @examples
#'
#' @import ggplot2
#' @import stringr
#' @export
build_plot <- function(
  data,
  chart_geom = "Line Chart",
  share = FALSE,
  x_var=NULL,
  y_var, #Name of variable to plot on y-axis
  color_var="None",       # name of coloration variable, as string
  facet_var="None",        # name of facet variable, as string
  legend=TRUE, #Include a legend
  caption=TRUE, #Include a source caption
  labels_and_colors=NULL,
  column_key=NULL
){
  data<-as.data.frame(data)
  mainplot <- ggplot(data = data)
  #Assume 1st row if no x_var provided.
if(is.null(x_var)) x_var<-names(data)[1]

  # add a line layer, broken out by color if requested
  if(chart_geom == "Line Chart"){
    if(color_var == "None"){
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(x_var),
          y = as.name(y_var)
        ))
    } else {
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(x_var),
          y = as.name(y_var),
          color = as.name(color_var)
        )) +
        guides(color = guide_legend(override.aes = list(size = 1)))+
        theme(legend.key = element_rect(fill = "white"))
    }
  }

  # add a bar layer, broken out by color if requested
  else if(chart_geom == "Bar Chart"){
    if(color_var == "None"){
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(data)[1]),
          y = as.name(y_var)
        ),
        stat = "identity")
    } else {
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(x_var),
          y = as.name(y_var),
          fill = as.name(color_var)
        ),
        stat = "identity")
    }
  }


  else if(chart_geom=="Scatter Plot"){
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


  # add faceting if requested, and x-axis labeling
  if(facet_var != "None"){
    if(chart_geom!="Histogram"){
    mainplot <- mainplot +
      facet_wrap(as.formula(paste0("~ `",facet_var, "`"))) +
      theme(strip.background = element_rect(fill = "white"))
    # theme(strip.background = element_rect(colour = "#554449", fill = "white", size=0.5),
    #       panel.border = element_rect(colour = "#554449", fill=NA, size=0.5))
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

  # add y-axis labeling
  if(share == TRUE){
    mainplot <- mainplot + scale_y_continuous(labels = scales::percent) +
      ylab(label = paste("Share of", y_var))
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

  mainplot<-add_preassigned_scales(
    mainplot,
    labels_and_colors,
    var=color_var
  )
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


  # add overall visual settings to the plot
  mainplot <- mainplot +  get_plot_theme()
  #diigtheme1:::diiggraph()

  if(legend==FALSE)
    mainplot<-mainplot+theme(legend.position = "none")

  if(caption==TRUE)
    #+labs(y="",
    mainplot<-mainplot+labs(caption = "Source: FPDS; CSIS analysis"
    )


  # return the plot to server.R
  return(mainplot)
}

