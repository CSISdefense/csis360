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

t<-theme(panel.border = element_blank(),
      panel.background = element_rect(fill = "white"),
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
