#' Deflation using GitHub-based CSV file
#'
#' @param data_to_deflate A data frame
#' @param money_var The quoted name of the dollar-value variable
#' @param fy_var The quoted name of the fiscal year variable
#' @param deflator_file The quoted file name of the deflators to use;
#' must be a CSV with the columns "FY" and "Deflator."
#' @param path The path or url for the deflator_file CSV.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/hamre_lookups/master
#'
#' @return Returns a data frame with the money_var deflated, otherwise identical
#' to the original data frame
#'
#' @section Warning: This function should be used __in data processing only__,
#' not in a live app.  It reads an external file from GitHub,
#' which will slow down an app substantially if done repeatedly.
#'
#' @examples RDTE_data <- deflate(
#'   data_to_deflate = RDTE_data,
#'   money_var = "Millions",
#'   fy_var = "fiscal_year")
#'
#' @import dplyr
#' @export
deflate <- function(
  data_to_deflate,
  money_var = "Amount",
  fy_var = "Fiscal.Year",
  deflator_file = "2016_deflators_actuals.csv",
  path = "https://raw.githubusercontent.com/CSISdefense/hamre_lookups/master/"){

  cat(paste("\n Applying\n", deflator_file, "\n from\n", path, "\n"))
  deflators_retrieved <- readr::read_csv(paste0(path, deflator_file))

  deflators <- deflators_retrieved$Deflator
  names(deflators) <- deflators_retrieved$FY

  data_to_deflate[[money_var]] <- as.numeric(as.character(
    data_to_deflate[[money_var]])) /
    deflators[as.character(data_to_deflate[[fy_var]])]

  return(data_to_deflate)
}



#' Create a palette for ggplot2 use, using lookup using GitHub-based CSV file
#'
#' @param data_for_lookup A data frame
#' @param category_vars A vector containing the quoted names of the variables
#' for which to set colors.  Any category variable that may be used as
#' a breakout should be included.  By default, includes all factor or character
#' variables.
#' @param lookup_file The quoted file name of the color lookup to use;
#' must be a CSV with the columns "variable" and "hexvalue".
#' @param path The path or url for the data_for_lookup CSV.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/hamre_lookups/master
#'
#' @return Returns a named vector of hex values, suitable for use with ggplot
#' functions such as scale_color_manual() and scale_fill_manual()
#'
#' @section Warning: This function should be ideally be used in data
#' processing, to create a colorset vector that will be saved in a .Rda file
#' for app use.  If you want to use it directly in an app, make sure it
#' only runs once - over the full data set - at app startup.  It reads an
#' external (GitHub) file and is not optimized for speed.
#'
#' @examples RDTE_data <- deflate(
#'   data_to_deflate = RDTE_data,
#'   money_var = "Millions",
#'   fy_var = "fiscal_year")
#'
#' @export
lookup_colors <- function(
  data_for_lookup,
  category_vars = c(all_factors(), all_characters()),
  lookup_file = "lookup_coloration_table.csv",
  path = "https://raw.githubusercontent.com/CSISdefense/hamre_lookups/master/"){

  cat(paste("\n Applying\n", deflator_file, "\n from\n", path, "\n"))
  deflators_retrieved <- readr::read_csv(paste0(path, deflator_file))

  deflators <- deflators_retrieved$Deflator
  names(deflators) <- deflators_retrieved$FY

  data_to_deflate[[money_var]] <- as.numeric(as.character(
    data_to_deflate[[money_var]])) /
    deflators[as.character(data_to_deflate[[fy_var]])]

  return(data_to_deflate)
}
