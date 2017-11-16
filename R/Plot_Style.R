#' Get Plot Theme
#'
#' @return Thematic elements that can be added to a plot.
#'
#' @section A standard set of style elements that is applied
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
      #panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color="white"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size=.1, color="lightgray"),
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
    vjust = 7,
    margin = margin(0,0,0,0)))
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
    margin = margin(0,15,0,0))) +
  theme(legend.text = element_text(
    family = "Open Sans",
    color ="#554449"))
t<-t+theme(legend.title = element_blank()) +
  theme(legend.position = 'bottom') +
  theme(legend.background = element_rect(fill = "white")
  )
        return(t)
}




build_plot <- function(
  # Adds a geom layer to a ggplot object based on user input.
  # Intended to handle ggplot settings that depend on user input.
  # Settings that apply universally should be added in server.R
  #
  # Args:
  data,    # tibble of formatted data for the ggplot
  chart_geom = "Line Chart",
  share = FALSE, #True or false as to whether to calculate the share
  x_var,          # name of fiscal year variable, as string
  y_var, #Name of variable to plot on y-axis
  color_var="None",       # name of coloration variable, as string
  facet_var="None",        # name of facet variable, as string
  labels_and_colors=NULL,
  column_key=NULL
  #
  # Returns:
  #   A ggplot object including user-specified geom layer
){
  mainplot <- ggplot(data = data)

  # add a line layer, broken out by color if requested
  if(chart_geom == "Line Chart"){
    if(color_var == "None"){
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(data)[1]),
          y = as.name(y_var)
        ))
    } else {
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(data)[1]),
          y = as.name(y_var),
          color = as.name(color_var)
        )) +
        guides(color = guide_legend(override.aes = list(size = 1)))+
        theme(legend.key = element_rect(fill = "white"))
    }
  }

  # add a bar layer, broken out by color if requested
  if(chart_geom == "Bar Chart"){
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
  start_fy<-min(data[,colnames(data)==x_var])
  end_fy<-max(data[,colnames(data)==x_var])

  # add faceting if requested, and x-axis labeling
  if(facet_var != "None"){
    mainplot <- mainplot +
      facet_wrap(as.formula(paste0("~ `",facet_var, "`"))) +
      theme(strip.background = element_rect(fill = "white")) +
      scale_x_continuous(
        breaks = function(x) {seq(start_fy, end_fy, by = 2)},
        labels = function(x){str_sub(as.character(x), -2, -1)}
      )
  } else {
    mainplot <- mainplot +
      scale_x_continuous(
        breaks = function(x){seq(start_fy, end_fy, by = 1)},
        labels = function(x){str_sub(as.character(x), -2, -1)}
      )
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
      mainplot<-mainplot+labs(
        x=get_label(x_var,column_key),
        y=get_label(y_var,column_key)
        )
  }
  # return the plot to server.R
  return(mainplot)
}
