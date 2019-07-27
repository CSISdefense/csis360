# These are special purpose standardization functions that only make sense when
# used with established CSIS data. They standardize names and colors using
# lookup tables prepared specifically for these variables.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:       'Ctrl + Shift + E'
#   Test Package:        'Ctrl + Shift + T'


#***********************Standardize Variable Names
#' Standardize variable names
#'
#' @param data the data frame to be joined
#' @param path the location of the lookup file
#' @param var the variable names to standardize; by default all will be done
#'
#' @return Data with standardized variable names.
#'
#' @details This function is designed to prepare CSIS data files for lookup
#' application. It primarily smooths out variation between different ways we've
#' written SQL statements. It relies on a pre-existing table of variant variable names.
#' The variable names are matched against that table in a case insensitive manner,
#' though no other procedural standardization is applied at this time.
#'
#' @examples FullData<-standardize_variable_names(
#'   FullData,
#'   Path)
#'
#' @export
standardize_variable_names<- function(data,
                                      path = "https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/",
                                      var = NULL
){
  if(!is.null(var) & any(!var %in% colnames(data)))
    stop(paste(var," is not present in colnames(data)."))


  if(is.data.frame(path))
    stop("path parameter is a data frame, it should be a file path, e.g. 'https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/'.")

  if(!is.data.frame(data))
    stop("data parameter is a not data frame, it should be.")

  #Remove nonsense characters sometimes added to start of the input file
  data<-remove_bom(data)

  #Consider removing non-alphanumerics _s .s etc.

  if(is.null(var)) var<-colnames(data)

  #***Standardize variable names
  NameList<-read.csv(
    paste(
      path,
      "Lookup_StandardizeVariableNames.csv",sep=""),
    header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )


  #     NameList<-subset(NameList,toupper(Original) %in% toupper(colnames(data)))
  for(x in 1:nrow(NameList)){
    #Limits it to names in var, if passed. By default, all will be covered.
    if(toupper(NameList$Original[[x]]) %in% toupper(var)){
      colnames(data)[toupper(colnames(data))==toupper(NameList$Original[[x]])]<-
        NameList$Replacement[[x]]
    }
  }

  data
}



#' Returns data in the appropriate format for the user-specified plot
#'
#' @param data The data to format for the plot, as a tibble
#' @param x_var x-axis
#' @param y_var y-axis
#' @param breakout TESTTESTTESTTESTTEST
#' @param aggregate aggregation function; defaults to sum
#'
#' @return A tibble of formatted data
#'
#' @details
#'
#'
#'
#' @export
group_data_for_plot <-function(
  data,   # data to format for the plot, as a tibble
  x_var,
  y_var,
  breakout,
  aggregate ="sum"
  #
  # Returns:
  #   a tibble of formatted data
){
  # account for potential spaces in breakout and x_var
  # note that this doesn't test for whether quotes already exist

  if(grepl(" ", x_var)) x_var <- paste0("`", x_var, "`")
  if(length(breakout) >= 1){
    if(grepl(" ", breakout[1])) breakout[1] <- paste0("`", breakout[1], "`")
  }
  if(length(breakout) >= 2){
    if(grepl(" ", breakout[2])) breakout[2] <- paste0("`", breakout[2], "`")
  }
  if(length(breakout) == 3){
    if(grepl(" ", breakout[3])) breakout[3] <- paste0("`", breakout[3], "`")
  }

  data<-data %>% filter(!is.na(!! as.name(y_var)))

  # aggregate to the level of [fiscal year x breakout]
  # the evaluation for dplyr::summarize_ was a pain in the ass to figure out;
  # see stack overflow at https://tinyurl.com/z82ywf3

  if(aggregate=="sum"){
    if(length(breakout) == 0){
      data %<>%
        dplyr::group_by(!! as.name(x_var)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(y_var)))
    } else {
      data %<>%
        dplyr::group_by_(.dots = c(x_var, breakout)) %>%
        summarize_(
          agg_val = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(y_var)))
    }
  } else if (aggregate=="mean"){
    if(length(breakout) == 0){
      data %<>%
        dplyr::group_by(as.name(!! as.name(x_var))) %>%
        summarize_(
          agg_val = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(y_var)))
    } else {
      data %<>%
        group_by_(.dots = c(x_var, breakout)) %>%
        summarize_(
          agg_val = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(y_var)))
    }
  } else (stop(paste,"group_data_for_plot does not know how to handle aggregate = ",aggregate))

  names(data)[which(names(data) == "agg_val")] <- y_var
  return(data)
}


#' Returns data in the appropriate format for the user-specified plot
#'
#' @param   data    A data frame to format for the plot, as a tibble
#' @param   fy_var  The fiscal year variable, as string
#' @param   y_var   The variable to be plotted on the y-axis
#' @param   share   If TRUE, calculates the share as a percentage
#' @param   start_fy Start fiscal year
#' @param   end_fy  End fiscal Year
#' @param   color_var Coloration variable, as string
#' @param   facet_var Facet variable, as string
#' @param   second_var Facet variable, as string
#' @param   labels_and_colors A csis360 lookup data.frame with factor information
#' @param   group If TRUE aggregate
#' @param   drop_missing_labels If TRUE, drop levels to avoid residual levels from labels_and_colors.
#'
#' @return Returns a tibble of formatted data
#'
#' @export
format_data_for_plot <- function(data, fy_var, y_var, share = FALSE, start_fy = NA, end_fy = NA,
                                 color_var="None",
                                 facet_var="None",
                                 second_var=NULL,
                                 labels_and_colors=NULL,
                                 group=TRUE,
                                 drop_missing_labels=TRUE){

  shown_data <- data
  if(all(!is.null(second_var),facet_var==second_var | second_var=="None")) second_var<-NULL

  breakout <- c(color_var, facet_var, second_var)
  breakout <- breakout[breakout != "None"]
  breakout <- breakout[!is.null(breakout)]

  if(group){
    shown_data<-group_data_for_plot(
      shown_data,
      fy_var,
      y_var,
      breakout
    )
  }

  if(!is.na(start_fy) & !is.na(end_fy)){
    # filter by year - see https://tinyurl.com/lm2u8xs
    if(is.numeric(shown_data[,fy_var])){
      shown_data <-shown_data  %>%
        filter_(paste0(fy_var, ">=", as.character(start_fy), "&", fy_var,
                       "<=", as.character(end_fy)))
    } else{
      shown_data <-shown_data  %>%
        filter_(paste0("between(year(",fy_var, "),", as.character(start_fy),
                       ",", as.character(end_fy),")"))
    }

  }


  #
  # NOTE: NAs replaced with 0 here; potential data quality issue
  #
  shown_data[is.na(shown_data)] <- 0

  # calculate shares if share checkbox is checked
  if(share == TRUE){
    if (color_var != "None"){

      # share_vars indicates which columns are being used to calculate the shares.
      # If there's only one breakout, it's set to -1:
      # "everything except fiscal year."
      # With two breakout, it's set to c(-1, -2):
      # "everything except fiscal year and the facet variable."
      facet_list <- c(facet_var,second_var)
      facet_list <- facet_list[!facet_list %in% c("None",color_var)]


      if(length(facet_list)==0)
        share_vars <- c(-1)
      else if (length(facet_list)==1)
        share_vars <- c(-1,-2)
      else
        share_vars <- c(-1,-2, -3)

      # spread the shares breakout variable across multiple columns
      shown_data<-shown_data %>%
        spread(color_var, y_var)

      #
      # NOTE: NAs replaced with 0 here; potential data quality issue
      #
      shown_data[is.na(shown_data)] <- 0

      # calculate a total for each row - i.e. the total for the shares breakout
      # variable for each fiscal year,
      # or for each [fiscal year x facet variable] combo
      shown_data$total <- rowSums(shown_data[share_vars])

      # divide each column by the total column, to get each column as shares
      shown_data[share_vars] <-
        sapply(shown_data[share_vars], function(x){x / shown_data$total})
      shown_data <- shown_data %>% dplyr::select(-total)

      # gather the data back to long form
      shown_data <- gather_(
        data = shown_data,
        key_col = color_var,
        value_col = y_var,
        gather_cols = names(shown_data[share_vars])
      )
    }

    # For the case where the user displays shares not broken out by any variable.
    # This is going to make a very boring chart of 100% shares,b
    # but it's handled here to avoid displaying an error.
    if(color_var == "None"){
      shown_data<-shown_data %>%
        mutate(total = 1)
      shown_data <- shown_data[which(names(shown_data) != y_var)]
      names(shown_data)[which(names(shown_data) == "total")] <- y_var
    }
  }

  shown_data<-as.data.frame(shown_data)
  if(!is.null(labels_and_colors)){
    if(color_var!="None"){
      shown_data[,colnames(shown_data)==color_var]<-
        ordered(shown_data[,colnames(shown_data)==color_var],
                levels=subset(labels_and_colors,column==color_var)$variable,
                labels=subset(labels_and_colors,column==color_var)$Label)
    }
    if(facet_var!="None" & color_var != facet_var){
      shown_data[,colnames(shown_data)==facet_var]<-
        ordered(shown_data[,colnames(shown_data)==facet_var],
                levels=subset(labels_and_colors,column==facet_var)$variable,
                labels=subset(labels_and_colors,column==facet_var)$Label
        )
    }
    if(all(!is.null(second_var), color_var != second_var)){
      shown_data[,colnames(shown_data)==second_var]<-
        ordered(shown_data[,colnames(shown_data)==second_var],
                levels=subset(labels_and_colors,column==second_var)$variable,
                labels=subset(labels_and_colors,column==second_var)$Label
        )
    }
    #If x-axis variable is a factor
    if(is.factor(shown_data[,colnames(shown_data)==fy_var]) & fy_var %in% labels_and_colors$column){
      shown_data[,colnames(shown_data)==fy_var]<-
        ordered(shown_data[,colnames(shown_data)==fy_var],
                levels=subset(labels_and_colors,column==fy_var)$variable,
                labels=c(subset(labels_and_colors,column==fy_var)$Label)
        )
    }
    if(drop_missing_labels==TRUE)
      shown_data<-droplevels(shown_data)
  }

  # return the ggplot-ready data
  return(shown_data)
}


#' Returns data in the appropriate format for the user-specified plot
#'
#' @param   data data frame
#' @param   period_var The variable with the period designations, grouped into those periods
#' @param   y_var The name of variable to plot on y-axis
#' @param   breakout Facet and/or color; everything that is to be grouped by for retention
#' @param   labels_and_colors A csis360 lookup data.frame with factor information
#'
#' @return Returns the average of the year entries across each period
#'
#' @export
format_period_average <- function(
  data,
  period_var, #The variable with the period designations, one per entry
  y_var,
  breakout, #Facet and/or color
  labels_and_colors
)
{
  breakout <- breakout[breakout != "None"]


  data<-group_data_for_plot(
    data,
    period_var,
    y_var,
    breakout,
    aggregate="mean"
  )



  data

}


#######Log setting 0s and negatives to NA
#' Set non-positive values to na and then log.
#'
#' @param x A list of numbers
#'
#' @return The list of number logs, with 0 and negative set to NA
#'
#' @details This is a function to use when the data should never
#' be 0s or negatives. It saves the step of setting them to na.
#'
#' @examples x<-c(0,2,3,-4); transform_contract(x)
#'
#' @export
na_non_positive_log<-function(x){
  x[x<=0]<-NA
  log(x)
}


#***********************Standardize Variable Names
#' Transform contract names
#'
#' @param contract A contract dataset
#'
#' @return contract dataset ready for statistical analysis.
#'
#' @details This function is designed to prepare CSIS data files for lookup
#' application. It primarily smooths out variation between different ways we've
#' written SQL statements. It relies on a pre-existing table of variant names.
#' The var names are matched against that table in a case insensitive manner,
#' though no other procedural standardization is applied at this time.
#'
#' @examples transform_contract(def)
#'
#' @import dplyr
#' @import lubridate
#' @import tidyverse
#' @import Hmisc
#' @export
transform_contract<-function(
  contract
){

  contract<-standardize_variable_names(contract)
  if("Action_Obligation" %in% colnames(contract))
    contract$Action_Obligation <-  as.numeric(contract$Action_Obligation)
  if("Number.Of.Actions" %in% colnames(contract))
    contract$Number.Of.Actions %<>% as.numeric()

  create_naics2<-function(NAICS){
    NAICS2<-substring(NAICS,1,2)
    NAICS2[NAICS2 %in% c('31','32','33')]<-'31-33'
    NAICS2[NAICS2 %in% c('44','45')]<-'44-45'
    NAICS2[NAICS2 %in% c('48','49')]<-'48-49'
    NAICS2<-factor(NAICS2)
    NAICS2
  }


  # contract$pNewWorkUnmodifiedBaseAndAll<-as.numeric(as.character(contract$pNewWorkUnmodifiedBaseAndAll))
  #Newwork and change
  # contract$pNewWork3Sig<-round(
  # contract$pNewWorkUnmodifiedBaseAndAll,3)
  cap<-function(column,cap){
    column[column>cap]<-cap
    column
  }


  #Customer
  if(!"Is.Defense" %in% colnames(contract) & "Who" %in% colnames(contract)){
    contract$Is.Defense<-as.character(contract$Who)
    contract$Is.Defense[contract$Is.Defense %in%
                          c("Air Force","Army",
                            "Navy","Other DoD","Uncategorized"  )
                        ]<-"Defense"
    contract$Is.Defense<-factor(contract$Is.Defense)

    contract$Who[contract$Who=="Uncategorized"]<-NA

    #b_ODoD
    contract$b_ODoD<-contract$Who
    levels(contract$b_ODoD)<- list("1"=c("Other DoD"),
                                   "0"=c("Air Force","Army","Navy"))

    contract$b_ODoD<-as.integer(as.character(contract$b_ODoD))

    contract$ODoD<-contract$Who
    levels(contract$ODoD)<- list("Military Departments"=c("Air Force","Army","Navy"),
                                 "Other DoD"=c("Other DoD"))





  }

  #SumOfisChangeOrder
  if("SumOfisChangeOrder" %in% colnames(contract))
    contract$qNChg <- Hmisc::cut2(contract$SumOfisChangeOrder,c(1,2,3))


  #PSR_What
  if("PSR_What" %in% colnames(contract)){
    contract$PSR_What<-factor(paste(as.character(contract$PSR),
                                    as.character(contract$What),sep="."))
    contract$PSR_What[contract$PSR_What=="Unlabeled"]<-NA
  }


  #b_Term
  if("Term" %in% colnames(contract)){
    contract$b_Term<-ifelse(contract$Term %in% c("Terminated",1),1,NA)
    contract$b_Term[contract$Term %in% c("Unterminated",0)]<-0
    #Create a jittered version of Term for display purposes
    #Unlike geom_jitter, this caps values at 0 and 1
    contract$j_Term<-jitter_binary(contract$b_Term)
  }

  #Ceiling Breach
  #b_CBre
  if("CBre" %in% colnames(contract)){
    contract$b_CBre<-ifelse(contract$CBre=="Ceiling Breach",1,NA)
    contract$b_CBre[contract$CBre=="None"]<-0
    #Create a jittered version of CBre for display purposes
    #Unlike geom_jitter, this caps values at 0 and 1
    contract$j_CBre<-jitter_binary(contract$b_CBre)
  }

  #ChangeOrderBaseAndAllOptionsValue
  if ("ChangeOrderBaseAndAllOptionsValue" %in% colnames(contract) ){
    contract$pChangeOrderBaseAndAllOptionsValue<-contract$ChangeOrderBaseAndAllOptionsValue/
      contract$UnmodifiedContractBaseAndAllOptionsValue
    contract$pChangeOrderBaseAndAllOptionsValue[
      is.na(contract$pChangeOrderBaseAndAllOptionsValue) & contract$SumOfisChangeOrder==0]<-0
    contract$pChangeOrderBaseAndAllOptionsValue<-as.numeric(as.character(contract$pChangeOrderBaseAndAllOptionsValue))
    contract$pChange3Sig<-round(
      contract$pChangeOrderBaseAndAllOptionsValue,3)
    contract$qCrai <- Hmisc::cut2(
      contract$pChangeOrderBaseAndAllOptionsValue,c(
        -0.001,
        0.001,
        0.15)
    )
  }


  #ChangeOrderCeilingGrowth
  if("ChangeOrderCeilingGrowth" %in% colnames(contract)){
    contract$p_CBre<-(contract$ChangeOrderCeilingGrowth/
                        contract$UnmodifiedContractBaseAndAllOptionsValue)+1
    contract$p_CBre[
      is.na(contract$p_CBre) & contract$b_CBre==0]<-1

    #lp_CBre
    contract$lp_CBre<-na_non_positive_log(contract$p_CBre)
    #ln_CBre
    contract$ln_CBre<-na_non_positive_log(contract$ChangeOrderCeilingGrowth)

  }
  if ("NewWorkUnmodifiedBaseAndAll" %in% colnames(contract) ){
    contract$pNewWorkUnmodifiedBaseAndAll<-contract$NewWorkUnmodifiedBaseAndAll/
      contract$UnmodifiedContractBaseAndAllOptionsValue
    contract$pNewWorkUnmodifiedBaseAndAll[
      is.na(contract$pNewWorkUnmodifiedBaseAndAll) & contract$SumOfisChangeOrder==0]<-0
    contract$pNewWorkUnmodifiedBaseAndAll<-as.numeric(as.character(contract$pNewWorkUnmodifiedBaseAndAll))
    contract$pChange3Sig<-round(
      contract$pNewWorkUnmodifiedBaseAndAll,3)
  }



  if ("UnmodifiedContractBaseAndAllOptionsValue" %in% colnames(contract) ){
    #Deflate the dolla figures
    contract<-deflate(contract,
                      money_var = "Action_Obligation",
                      # deflator_var="OMB.2019",
                      fy_var="StartFY"
    )
    contract<-deflate(contract,
                      money_var = "UnmodifiedContractBaseAndAllOptionsValue",
                      # deflator_var="OMB.2019",
                      fy_var="StartFY"
    )
    #l_Ceil

    if(!"l_Ceil" %in% colnames(contract))
      contract$l_Ceil<-na_non_positive_log(contract$UnmodifiedContractBaseAndAllOptionsValue.OMB20_GDP18)

    # lowroundedcutoffs<-c(15000,100000,1000000,30000000)
    highroundedcutoffs<-c(15000,100000,1000000,10000000,75000000)
    # contract$qLowCeiling <- Hmisc::cut2(contract$UnmodifiedContractBaseAndAllOptionsValue.OMB20_GDP18,cuts=lowroundedcutoffs)
    contract$qHighCeiling <- Hmisc::cut2(contract$UnmodifiedContractBaseAndAllOptionsValue.OMB20_GDP18,cuts=highroundedcutoffs)
    rm(highroundedcutoffs)#lowroundedcutoffs,


    if (all(levels(contract$qHighCeiling)[1:5]==c("[0.00e+00,1.50e+04)",
                                          "[1.50e+04,1.00e+05)",
                                          "[1.00e+05,1.00e+06)",
                                          "[1.00e+06,1.00e+07)",
                                          "[1.00e+07,7.50e+07)"))|
        all(levels(contract$qHighCeiling)[1:5]==c("[0.0e+00,1.5e+04)",
                                          "[1.5e+04,1.0e+05)",
                                          "[1.0e+05,1.0e+06)",
                                          "[1.0e+06,1.0e+07)",
                                          "[1.0e+07,7.5e+07)"))
    ){
      contract$qHighCeiling<-factor(contract$qHighCeiling,

                            levels=levels(contract$qHighCeiling),
                            labels=c("[0,15k)",
                                     "[15k,100k)",
                                     "[100k,1m)",
                                     "[1m,10m)",
                                     "[10m,75m)",
                                     "[75m+]"),
                            ordered=TRUE
      )
    }


    # if (all(levels(contract$qLowCeiling)[1:4]==c("[0.00e+00,1.50e+04)",
    #                                         "[1.50e+04,1.00e+05)",
    #                                         "[1.00e+05,1.00e+06)",
    #                                         "[1.00e+06,3.00e+07)"))){
    #   contract$qLowCeiling<-factor(contract$qLowCeiling,
    #
    #                                levels=c("[0.00e+00,1.50e+04)",
    #                                         "[1.50e+04,1.00e+05)",
    #                                         "[1.00e+05,1.00e+06)",
    #                                         "[1.00e+06,3.00e+07)",
    #                                         levels(contract$qLowCeiling)[5]),
    #                                labels=c("[0,15k)",
    #                                         "[15k,100k)",
    #                                         "[100k,1m)",
    #                                         "[1m,30m)",
    #                                         "[30m+]"),
    #                                ordered=TRUE
    #   )
    # }

    contract<-contract %>% group_by(qHighCeiling) %>%
      mutate(ceil.median.wt = median(UnmodifiedContractBaseAndAllOptionsValue.OMB20_GDP18))

    if (identical(levels(contract$qHighCeiling),c("[0,15k)",
                                                  "[15k,100k)",
                                                  "[100k,1m)",
                                                  "[1m,10m)",
                                                  "[10m,75m)",
                                                  "[75m+]"
    ))){
      contract$Ceil.Simple<-contract$qHighCeiling
      levels(contract$Ceil.Simple)<- list("0k - <100k"=c("[15k,100k)",
                                                         "[0,15k)"),
                                          "100k - <10m"=c("[1m,10m)",
                                                          "[100k,1m)"),
                                          "10m+"=c("[75m+]",
                                                   "[10m,75m)"))

      contract$Ceil.Big<-contract$qHighCeiling
      levels(contract$Ceil.Big)<- list("0k - <100k"=c("[15k,100k)",
                                                      "[0,15k)"),
                                       "100k - <10m"=c("[1m,10m)",
                                                       "[100k,1m)"),
                                       "10m - <75m"=c("[10m,75m)"),
                                       "75m+"=c("[75m+]"))

      contract$Ceil.1m<-contract$qHighCeiling
      levels(contract$Ceil.1m)<- list("0k - <1m"=c("[0,15k)",
                                                   "[15k,100k)",
                                                   "[100k,1m)"
      ),
      "1m - <10m"=c("[1m,10m)"),
      "10m - <75m"=c("[10m,75m)"),
      "75m+"=c("[75m+]"))
    } else if (identical(levels(contract$qHighCeiling),c("0 - <15k",
                                                         "15k - <100k",
                                                         "100k - <1m",
                                                         "1m - <10m",
                                                         "10m - <75m",
                                                         "75m+"
    ))){
      contract$Ceil.Simple<-contract$qHighCeiling
      levels(contract$Ceil.Simple)<- list("0k - <100k"=c("15k - <100k",
                                                         "0 - <15k"),
                                          "100k - <10m"=c("1m - <10m",
                                                          "100k - <1m"),
                                          "10m+"=c("75m+",
                                                   "10m - <75m"))

      contract$Ceil.Big<-contract$qHighCeiling
      levels(contract$Ceil.Big)<- list("0k - <100k"=c("15k - <100k",
                                                      "0 - <15k"),
                                       "100k - <10m"=c("1m - <10m",
                                                       "100k - <1m"),
                                       "10m - <75m"=c("10m - <75m"),
                                       "75m+"=c("75m+"))
      contract$Ceil.1m<-contract$qHighCeiling
      levels(contract$Ceil.1m)<- list("0k - <1m"=c("15k - <100k",
                                                   "0 - <15k",
                                                   "100k - <1m"),
                                      "1m - <10m"=c("1m - <10m"),
                                      "10m - <75m"=c("10m - <75m"),
                                      "75m+"=c("75m+"))
    }



  }




  if ("UnmodifiedCurrentCompletionDate" %in% colnames(contract) ){
    contract$UnmodifiedCurrentCompletionDate<-as.Date(contract$UnmodifiedCurrentCompletionDate)
  }


  #Rename standardization
  colnames(contract)[colnames(contract)=="Dur"]<-"qDuration"

  #l_Days
  if("UnmodifiedDays" %in% colnames(contract)){

    contract$UnmodifiedDays[contract$UnmodifiedDays<0]<-NA
    contract$capped_UnmodifiedDays <- ifelse(contract$UnmodifiedDays > 3650, 3650, contract$UnmodifiedDays)

    contract$l_Days<-na_non_positive_log(contract$UnmodifiedDays)
    contract$cl_Days<-arm::rescale(contract$l_Days)

    contract$capped_l_Days<-na_non_positive_log(contract$capped_UnmodifiedDays)
    contract$capped_cl_Days<-arm::rescale(contract$capped_l_Days)

    contract$UnmodifiedYearsFloat<-contract$UnmodifiedDays/365.25
    contract$UnmodifiedYearsCat<-floor(contract$UnmodifiedYearsFloat)

    #Break the count of days into four categories.
    if (!"qDuration" %in% colnames(contract)){

      contract$qDuration<-Hmisc::cut2(contract$UnmodifiedDays,cuts=c(61,214,366,732))
    }

    if (levels(contract$qDuration)[[2]]=="[   61,  214)"){
      contract$qDuration<-factor(contract$qDuration,

                                 levels=c("[    0,   61)",
                                          "[   61,  214)",
                                          "[  214,  366)",
                                          "[  366,  732)",
                                          levels(contract$qDuration)[5]),
                                 labels=c("[0 months,~2 months)",
                                          "[~2 months,~7 months)",
                                          "[~7 months-~1 year]",
                                          "(~1 year,~2 years]",
                                          "(~2 years+]"),
                                 ordered=TRUE
      )
    }

    contract$qDuration[contract$UnmodifiedYearsCat<0]<-NA


    contract$Dur.Simple<-contract$qDuration
    levels(contract$Dur.Simple)<- list(
      "<~1 year"=c("[0 months,~2 months)","[~2 months,~7 months)","[~7 months-~1 year]"),
      "(~1 year,~2 years]"="(~1 year,~2 years]",
      "(~2 years+]"="(~2 years+]")

  }


  #n_Fixed
  if("FxCb" %in% colnames(contract)){
    contract$n_Fixed<-contract$FxCb
    levels(contract$n_Fixed)<- list("1"=c("Fixed-Price","Fixed"),
                                    "0.5"=c("Combination or Other","Combo/Other"),
                                    "0"=c("Cost-Based","Cost"))
    levels(contract$FxCb)<- list("Fixed"=c("Fixed-Price","Fixed"),
                                 "Combo/Other"=c("Combination or Other","Combo/Other"),
                                 "Cost"=c("Cost-Based","Cost"))
    contract$n_Fixed<-as.numeric(as.character(contract$n_Fixed))

    #n_Incent
    contract$n_Incent<-contract$Fee
    levels(contract$n_Incent) <-
      list("1"=c("Incentive"),
           "0.5"=c("Combination"),
           "0"=c("Award Fee", "FFP or No Fee", "Fixed Fee", "Other Fee"))
    contract$n_Incent<-as.numeric(as.character(contract$n_Incent))

    #n_NoFee
    contract$n_NoFee<-contract$Fee
    levels(contract$n_NoFee) <-
      list("1"=c("FFP or No Fee"),
           "0.5"=c("Combination"),
           "0"=c("Award Fee", "Incentive", "Fixed Fee", "Other Fee"))
    contract$n_NoFee<-as.numeric(as.character(contract$n_NoFee))



    contract$Pricing<-as.character(contract$FxCb )
    summary(contract$Fee)
    summary(factor(contract$Pricing))
    contract$Pricing[contract$Pricing %in% c("Fixed","Fixed-Price") & contract$Fee=="FFP or No Fee"]<-"FFP"
    contract$Pricing[contract$Pricing %in% c("Fixed","Fixed-Price") & contract$Fee!="FFP or No Fee"]<-"Other FP"
    contract$Pricing[contract$Pricing %in% c("Cost","Cost-Based") & contract$Fee=="Other Fee"]<-"T&M/LH/FPLOE"
    contract$Pricing[contract$Pricing %in% c("Combo/Other")]<-"Combination or Other"
    contract$Pricing[contract$Pricing %in% c("Cost")]<-"Cost-Based"
    contract$Pricing<-factor(contract$Pricing,c("FFP","Other FP","Combination or Other",
                                                "Cost-Based","T&M/LH/FPLOE"))
    summary(contract$Fee)
    summary(factor(contract$Pricing))
    contract$PricingFee<-as.character(contract$Pricing)
    contract$PricingFee[contract$Fee=="Incentive"]<-"Incentive"
    # contract$PricingFee[contract$PricingFee %in% c("Other FP","FFP")] <-"Other FP"
    contract$PricingFee[contract$PricingFee %in% c("Cost-Based")] <-"Other CB"
    # contract$PricingFee<-factor(contract$PricingFee,c("Other FP","Incentive",
    #                                         "Combination or Other",
    #                                   "Other CB","T&M/LH/FPLOE"))
    contract$PricingFee<-factor(contract$PricingFee,c("FFP","Other FP","Incentive",
                                                      "Combination or Other",
                                                      "Other CB","T&M/LH/FPLOE"))
    summary(contract$PricingFee)

    # summary(factor(contract$PricingFee))
    contract$PricingUCA<-as.character(contract$PricingFee)
    contract$PricingUCA[is.na(contract$UCA)]<-NA
    # summary(factor(contract$PricingUCA))
    contract$PricingUCA[contract$UCA=="UCA"]<-"UCA"
    contract$PricingUCA<-factor(contract$PricingUCA,c("FFP","Other FP","Incentive",
                                                      "Combination or Other",
                                                      "Other CB","T&M/LH/FPLOE","UCA"))
    summary(contract$PricingUCA)


  }

  #Competition
  if("Comp" %in% colnames(contract)){
    #Right now comp is not actually a factor, so don't need to process it
    contract$b_Comp<-contract$Comp #Fix in Rdata, and add back comp
    levels(contract$b_Comp) <-
      list("0"="No Comp.",
           "1"="Comp.")
    contract$b_Comp<-as.integer(as.character(contract$b_Comp))

    #n_Comp
    contract$n_Comp<-contract$EffComp #Fix in Rdata, and add back comp
    levels(contract$n_Comp) <-
      list("0"="No Comp.",
           "0.5"="1 Offer",
           "1"="2+ Offers")
    # contract$n_Comp<-as.numeric(as.character(contract$n_Comp))




    contract$q_Offr<-Hmisc::cut2(contract$UnmodifiedNumberOfOffersReceived,c(2,3,5))
    levels(contract$q_Offr) <-
      list("1"=c("1","  1"),
           "2"=c("2","  2"),
           "3-4"=c("[  3,  5)"),
           "5+"=c("[  5,999]")
      )
    #Set number of offers =1 when there is a NA and no competition
    #This seems to be redundant, but no harm in it.
    contract$q_Offr[is.na(contract$q_Offr)&
                      !is.na(contract$b_Comp)&
                      contract$b_Comp==0
                    ]<-"1"
    contract$nq_Offr<-contract$q_Offr
    levels(contract$nq_Offr) <-
      list("1"=c("1"),
           "2"=c("2"),
           "3"=c("3-4"),
           "4"=c("5+"))



    contract$nq_Offr<-as.integer(as.character(contract$nq_Offr))
    contract$nq_Offr[contract$b_Comp==0 & !is.na(contract$b_Comp)]<-0
    contract$nq_Offr[is.na(contract$b_Comp)]<-NA
    contract$CompOffr<-factor(contract$nq_Offr)
    levels(contract$CompOffr) <-
      list("No Competition"="0",
           "1 offer"="1",
           "2 offers"="2",
           "3-4 offers"="3",
           "5+ offers"="4")


    #l_Offr
    contract$l_Offr<-na_non_positive_log(contract$UnmodifiedNumberOfOffersReceived)


    contract$cb_Comp<-arm::rescale(contract$b_Comp)
    contract$cn_Comp<-arm::rescale(contract$n_Comp)
    contract$cn_Offr<-arm::rescale(contract$nq_Offr)
    contract$cl_Offr<-arm::rescale(contract$l_Offr)

    #Urgency
    contract$b_Urg<-NA
    contract$b_Urg<-ifelse(contract$Urg=="Urgency Except.",1,NA)
    contract$b_Urg[contract$Urg=="Not Urgency"]<-0

    contract$NoComp<-NA
    contract$NoComp<-ifelse(contract$Urg=="Urgency Except.","Urgency",NA)
    contract$NoComp[contract$Urg=="Not Urgency"]<-"Other No"
    contract$NoComp[contract$b_Comp==1]<-"Any Comp."
    contract$NoComp<-factor(contract$NoComp,
                            c("Any Comp.","Other No","Urgency"))

    contract$NoCompOffr<-contract$CompOffr
    levels(contract$NoCompOffr) <-
      list("No Competition"="No Competition",
           "1 offer"="1 offer",
           "2-4 offers"=c("2 offers","3-4 offers"),
           "5+ offers"="5+ offers")
    contract$NoCompOffr<-as.character(contract$NoCompOffr)
    contract$NoCompOffr[is.na(contract$NoComp) |
                          contract$NoComp!="Any Comp."]<-
      as.character(contract$NoComp[is.na(contract$NoComp) |
                                     contract$NoComp!="Any Comp."])
    contract$NoCompOffr<-factor(contract$NoCompOffr,c(
      c("Other No",
        "Urgency",
        "1 offer",
        "2-4 offers",
        "5+ offers"
      )
    ))

    contract$Comp1or5<-contract$CompOffr
    levels(contract$Comp1or5)<-
      list("No Competition"="No Competition",
           "1 offer"="1 offer",
           "2-4 offers"=c("2 offers","3-4 offers"),
           "5+ offers"="5+ offers")
    summary(contract$Comp1or5)
  }



  if("Intl" %in% colnames(contract)){
    #b_Intl
    contract$Intl <- factor(contract$Intl,
                            c("Just U.S.", "Any International"))   #Manually remove "NA" from levels of variable Intl
    levels(contract$Intl)<- list("Just U.S."=c("Just U.S."),
                            "Any Intl."=c("Any Intl.","Any International"))


    contract$b_Intl<-contract$Intl
    contract$b_Intl[contract$b_Intl=="Unlabeled"]<-NA
    levels(contract$b_Intl) <-
      list("0"=c("Just U.S."),
           "1"=c("Any Intl.","Any International"))
    contract$b_Intl<-as.integer(as.character(contract$b_Intl))
  }

  if("UCA" %in% colnames(contract)){
    #b_UCA
    contract$b_UCA<-contract$UCA
    levels(contract$b_UCA) <-
      list("0"=c("Not UCA"),
           "1"=c("UCA"))
    contract$b_UCA<-as.integer(as.character(contract$b_UCA))

  }


  #
  # if(!"Is.Defense" %in% colnames(contract)){
  #   contract$Is.Defense<-as.character(contract$Who)
  #   contract$Is.Defense[contract$Is.Defense %in%
  #                                      c("Air Force","Army",
  #                                        "Navy","Other DoD","Uncategorized"  )
  #                                    ]<-"Defense"
  #   contract$Is.Defense<-factor(contract$Is.Defense)
  # }

  if("Veh" %in% colnames(contract)){
    levels(contract$Veh)[levels(contract$Veh)=="SINGLE AWARD IDC"]<-"S-IDC"
    levels(contract$Veh)[levels(contract$Veh)=="MULTIPLE AWARD IDC"]<-"M-IDC"
    levels(contract$Veh)[levels(contract$Veh)=="def_detail/Pur"]<-"Def/Pur"
    contract$Veh<-factor(contract$Veh,c("Def/Pur",
                                        "S-IDC",
                                        "M-IDC",
                                        "FSS/GWAC",
                                        "BPA/BOA"))

    #SIDV
    contract$SIDV<-contract$Veh
    levels(contract$SIDV) <-
      list("1"=c("S-IDC"),
           "0"=c("Def/Pur","M-IDC","FSS/GWAC","BPA/BOA"))
    contract$SIDV<-as.integer(as.character(contract$SIDV))

    #MIDV
    contract$MIDV<-contract$Veh
    levels(contract$MIDV) <-
      list("1"=c("M-IDC"),
           "0"=c("Def/Pur","S-IDC","FSS/GWAC","BPA/BOA"))
    contract$MIDV<-as.integer(as.character(contract$MIDV))

    #FSSGWAC
    contract$FSSGWAC<-contract$Veh
    levels(contract$FSSGWAC) <-
      list("1"=c("FSS/GWAC"),
           "0"=c("Def/Pur","S-IDC", "M-IDC","BPA/BOA"))
    contract$FSSGWAC<-as.integer(as.character(contract$FSSGWAC))

    #BPABOA
    contract$BPABOA<-contract$Veh
    levels(contract$BPABOA) <-
      list("1"=c("BPA/BOA"),
           "0"=c("Def/Pur","S-IDC", "M-IDC","FSS/GWAC"))
    contract$BPABOA<-as.integer(as.character(contract$BPABOA))

    if("Crisis" %in% colnames(contract)){
      #Crisis Dataset
      # contract$ARRA<-0
      # contract$ARRA[contract$MaxOfDecisionTree=="ARRA"]<-1
      # contract$Dis<-0
      # contract$Dis[contract$MaxOfDecisionTree=="Disaster"]<-1
      # contract$OCO<-0
      # contract$OCO[contract$MaxOfDecisionTree=="OCO"]<-1
      contract$Crisis<-factor(contract$Crisis)
      levels(contract$Crisis)[levels(contract$Crisis)=="Excluded"]<-"Other"
      contract$Crisis[is.na(contract$Crisis)]<-"Other"
      levels(contract$Crisis)[levels(contract$Crisis)=="Disaster"]<-"Dis"
      contract$Crisis<-factor(contract$Crisis,c("Other","ARRA","Dis","OCO"))
    }
  }

  #Calendar Year
  if("MinOfSignedDate" %in% colnames(contract)){
    contract$StartCY<-lubridate::year(contract$MinOfSignedDate)
  }


  #NAICS
  #Note that this must be placed a new in each repository.
  #In theory we could store a version in csis360, something to consider for the future.
  local_semi_clean_path<-"..\\data\\semi_clean\\"
  if(!dir.exists(local_semi_clean_path)& dir.exists("data\\semi_clean\\"))
    local_semi_clean_path<-"data\\semi_clean\\"
  else(stop("Don't know where local_semi_clean directory is"))

  if("NAICS" %in% colnames(contract) & "StartCY" %in% colnames(contract) ){
    naics.file<-NA
    #Vendor repository location
    if(file.exists("../output/naics_join.Rdata")) naics.file<-"../output/naics_join.Rdata"
    else if(file.exists("output/naics_join.Rdata")) naics.file<-"output/naics_join.Rdata"
    else if(file.exists(paste(local_semi_clean_path,"naics_join.Rdata",sep="")))
       naics.file<-paste(local_semi_clean_path,"naics_join.Rdata",sep="")
    else if(file.exists("../data/clean/naics_join.Rdata")) naics.file<-"../data/clean/naics_join.Rdata"
    else if(file.exists("data/clean/naics_join.Rdata")) naics.file<-"data/clean/naics_join.Rdata"
    if(!is.na(naics.file)){
      load(naics.file)

      # contract<-left_join(contract,NAICS_join, by=c("StartFY"="StartFY",
      #                                               "NAICS"="NAICS_Code"))

      contract$NAICS<-as.integer(as.character(contract$NAICS))
      contract$NAICS5<-as.integer(substr(contract$NAICS,1,5))
      contract$NAICS4<-as.integer(substr(contract$NAICS,1,4))
      contract$NAICS3<-as.integer(substr(contract$NAICS,1,3))
      contract$NAICS2<-create_naics2(contract$NAICS)

      #This critical NAICS6 split in 2 from 2012 to 2017 and would prevent analysis of 7% of obligations if not reunited.
      contract$NAICS[substr(contract$NAICS,1,5)==54171 &
                       !is.na(contract$NAICS)]<-54171

      if(!"def6_HHI_lag1" %in% colnames(contract))
        contract<-left_join(contract,NAICS6_join, by=c("StartCY"="CalendarYear",
                                                       "NAICS"="NAICS6"))

      if(!"def5_HHI_lag1" %in% colnames(contract))
        contract<-left_join(contract,NAICS5_join, by=c("StartCY"="CalendarYear",
                                                       "NAICS5"="NAICS5"))

      if(!"def4_HHI_lag1" %in% colnames(contract))
        contract<-left_join(contract,NAICS4_join, by=c("StartCY"="CalendarYear",
                                                       "NAICS4"="NAICS4"))

      if(!"def3_HHI_lag1" %in% colnames(contract))
        contract<-left_join(contract,NAICS3_join, by=c("StartCY"="CalendarYear",
                                                       "NAICS3"="NAICS3"))

      if(!"def2_HHI_lag1" %in% colnames(contract))
        contract<-left_join(contract,NAICS2_join, by=c("StartCY"="CalendarYear",
                                                       "NAICS2"="NAICS2"))



      #Remove 0s, they make no sense, source must be one contractors in field have 0 obligations, which is just missing data really
      contract$def6_HHI_lag1[contract$def6_HHI_lag1==0]<-NA
      contract$c_def6_HHI_lag1<-arm::rescale(contract$def6_HHI_lag1)

      contract$l_def6_HHI_lag1<-na_non_positive_log(contract$def6_HHI_lag1)
      contract$cl_def6_HHI_lag1<-arm::rescale(contract$l_def6_HHI_lag1)

      contract$def5_HHI_lag1[contract$def5_HHI_lag1==0]<-NA
      contract$c_def5_HHI_lag1<-arm::rescale(contract$def5_HHI_lag1)

      contract$l_def5_HHI_lag1<-na_non_positive_log(contract$def5_HHI_lag1)
      contract$cl_def5_HHI_lag1<-arm::rescale(contract$l_def5_HHI_lag1)

      contract$def4_HHI_lag1[contract$def4_HHI_lag1==0]<-NA
      contract$c_def4_HHI_lag1<-arm::rescale(contract$def4_HHI_lag1)

      contract$l_def4_HHI_lag1<-na_non_positive_log(contract$def4_HHI_lag1)
      contract$cl_def4_HHI_lag1<-arm::rescale(contract$l_def4_HHI_lag1)

      contract$def3_HHI_lag1[contract$def3_HHI_lag1==0]<-NA
      contract$c_def3_HHI_lag1<-arm::rescale(contract$def3_HHI_lag1)

      contract$l_def3_HHI_lag1<-na_non_positive_log(contract$def3_HHI_lag1)
      contract$cl_def3_HHI_lag1<-arm::rescale(contract$l_def3_HHI_lag1)

      contract$def2_HHI_lag1[contract$def2_HHI_lag1==0]<-NA
      contract$c_def2_HHI_lag1<-arm::rescale(contract$def2_HHI_lag1)

      contract$l_def2_HHI_lag1<-na_non_positive_log(contract$def2_HHI_lag1)
      contract$cl_def2_HHI_lag1<-arm::rescale(contract$l_def2_HHI_lag1)


      contract$capped_def6_ratio_lag1<-cap(contract$def6_ratio_lag1,1)
      contract$l_def6_ratio_lag1<-na_non_positive_log(contract$def6_ratio_lag1)
      contract$cl_def6_ratio_lag1<-arm::rescale(contract$def6_ratio_lag1)

      contract$capped_def5_ratio_lag1<-cap(contract$def5_ratio_lag1,1)
      contract$l_def5_ratio_lag1<-na_non_positive_log(contract$def5_ratio_lag1)
      contract$cl_def5_ratio_lag1<-arm::rescale(contract$def5_ratio_lag1)

      contract$capped_def4_ratio_lag1<-cap(contract$def4_ratio_lag1,1)
      contract$l_def4_ratio_lag1<-na_non_positive_log(contract$def4_ratio_lag1)
      contract$cl_def4_ratio_lag1<-arm::rescale(contract$def4_ratio_lag1)


      contract$capped_def3_ratio_lag1<-cap(contract$def3_ratio_lag1,1)
      contract$l_def3_ratio_lag1<-na_non_positive_log(contract$def3_ratio_lag1)
      contract$cl_def3_ratio_lag1<-arm::rescale(contract$def3_ratio_lag1)

      contract$capped_def2_ratio_lag1<-cap(contract$def2_ratio_lag1,1)
      contract$l_def2_ratio_lag1<-na_non_positive_log(contract$def2_ratio_lag1)
      contract$cl_def2_ratio_lag1<-arm::rescale(contract$def2_ratio_lag1)


      contract$l_def6_obl_lag1<-na_non_positive_log(contract$def6_obl_lag1)
      contract$cl_def6_obl_lag1<-arm::rescale(contract$l_def6_obl_lag1)
      contract$l_def5_obl_lag1<-na_non_positive_log(contract$def5_obl_lag1)
      contract$cl_def5_obl_lag1<-arm::rescale(contract$l_def5_obl_lag1)
      contract$l_def4_obl_lag1<-na_non_positive_log(contract$def4_obl_lag1)
      contract$cl_def4_obl_lag1<-arm::rescale(contract$l_def4_obl_lag1)
      contract$l_def3_obl_lag1<-na_non_positive_log(contract$def3_obl_lag1)
      contract$cl_def3_obl_lag1<-arm::rescale(contract$l_def3_obl_lag1)
      contract$l_def2_obl_lag1<-na_non_positive_log(contract$def2_obl_lag1)
      contract$cl_def2_obl_lag1<-arm::rescale(contract$l_def2_obl_lag1)



      contract$l_US6_avg_sal_lag1<-na_non_positive_log(contract$US6_avg_sal_lag1)
      contract$cl_US6_avg_sal_lag1<-arm::rescale(contract$l_US6_avg_sal_lag1)


      contract$l_US5_avg_sal_lag1<-na_non_positive_log(contract$US5_avg_sal_lag1)
      contract$cl_US5_avg_sal_lag1<-arm::rescale(contract$l_US5_avg_sal_lag1)


      contract$l_US4_avg_sal_lag1<-na_non_positive_log(contract$US4_avg_sal_lag1)
      contract$cl_US4_avg_sal_lag1<-arm::rescale(contract$l_US4_avg_sal_lag1)

      contract$l_US3_avg_sal_lag1<-na_non_positive_log(contract$US3_avg_sal_lag1)
      contract$cl_US3_avg_sal_lag1<-arm::rescale(contract$l_US3_avg_sal_lag1)

      contract$l_US2_avg_sal_lag1<-na_non_positive_log(contract$US2_avg_sal_lag1)
      contract$cl_US2_avg_sal_lag1<-arm::rescale(contract$l_US2_avg_sal_lag1)





    }

  }


  if("ProdServ" %in% colnames(contract)){
    contract$ProdServ[contract$ProdServ==""]<-NA
    contract$ProductOrServiceCode<-as.character(contract$ProdServ)
  }

  if("ProductOrServiceCode" %in% colnames(contract)){
    contract<-read_and_join( contract,
                             "ProductOrServiceCodes.csv",
                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                             directory="",
                             by="ProductOrServiceCode",
                             add_var=c("Simple",
                                       "ProductServiceOrRnDarea",
                                       "ProductOrServiceArea",
                                       "HostNation3Category",
                                       "CrisisProductOrServiceArea",
                                       "ProductOrServiceCodeText"
                             ),
                             new_var_checked=FALSE,
                             lookup_char_as_factor=TRUE)

    contract$ProductServiceOrRnDarea<-factor(contract$ProductServiceOrRnDarea)
    contract$ProductOrServiceArea<-factor(contract$ProductOrServiceArea)
    contract$HostNation3Category<-factor(contract$HostNation3Category)
    contract$CrisisProductOrServiceArea<-factor(contract$CrisisProductOrServiceArea)
    contract$ProductOrServiceCodeText<-factor(contract$ProductOrServiceCodeText)
  }

  #Office
  colnames(contract)[colnames(contract)=="Office"]<-"ContractingOfficeCode"
  if("ContractingOfficeCode" %in% colnames(contract)){

    contract<-read_and_join( contract,
                             "Office.ContractingOfficeCode.txt",
                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                             directory="office\\",
                             by="ContractingOfficeCode",
                             add_var=c("PlaceIntlPercent","CrisisPercent"),
                             new_var_checked=FALSE,
                             lookup_char_as_factor=TRUE)


    colnames(contract)[colnames(contract)=="PlaceIntlPercent"]<-"OffIntl"

    contract$OffPlace<-Hmisc::cut2(contract$OffIntl,c(0.01,0.50))
    levels(contract$OffPlace) <-
      list("US99"=c("[0.00,0.01)"),
           "Mixed"=c("[0.01,0.50)"),
           "Intl"=c("[0.50,1.00]"))

    colnames(contract)[colnames(contract)=="CrisisPercent"]<-"OffCri"
    contract$c_OffCri<-arm::rescale(contract$OffCri)

    if("Intl" %in% colnames(contract)){
      contract$Reach6<-factor(paste(contract$OffPlace,contract$Intl,sep="-"))
      levels(contract$Reach6) <-
        list( "US99-Dom"=c("US99-Just U.S."),
              "Mixed-Dom"=c("Mixed-Just U.S."),
              "Intl-Dom"=c("Intl-Just U.S."),
              "US99-Intl"=c("US99-Any International"),
              "Mixed-Intl"=c("Mixed-Any International"),
              "Intl-Intl"=c("Intl-Any International"))
      contract$Reach<-contract$Reach6

      levels(contract$Reach) <-
        list( "US50-Dom"=c("US99-Just U.S.","Mixed-Just U.S."),
              "Mixed-Dom"=c(),
              "Intl-Dom"=c("Intl-Just U.S."),
              "US50-Intl"=c("Mixed-Any International","US99-Any International"),
              "Intl-Intl"=c("Intl-Any International"))
    }

    colnames(contract)[colnames(contract)=="StartFY"]<-"fiscal_year"
    if(file.exists(paste(local_semi_clean_path,"Office.sp_OfficeHistoryCapacityLaggedConst.txt",sep=""))){
      contract<-read_and_join_experiment( contract,
                                          "Office.sp_OfficeHistoryCapacityLaggedConst.txt",
                                          path="",
                                          directory=local_semi_clean_path,
                                          by=c("ContractingOfficeCode","fiscal_year"),
                                          add_var=c("office_obligatedamount_1year",
                                                    "office_numberofactions_1year",
                                                    "office_PBSCobligated_1year",
                                                    "office_obligatedamount_7year"),
                                          new_var_checked=FALSE,
                                          create_lookup_rdata=TRUE,
                                          lookup_char_as_factor=TRUE
      )


      contract$office_numberofactions_1year[is.na(contract$office_numberofactions_1year)]<-0
      contract$office_obligatedamount_7year[is.na(contract$office_obligatedamount_7year) |
                                              contract$office_obligatedamount_7year<0]<-0
      contract$office_obligatedamount_1year[is.na(contract$office_obligatedamount_1year) |
                                              contract$office_obligatedamount_1year<0]<-0
      contract$office_PBSCobligated_1year[is.na(contract$office_PBSCobligated_1year)|
                                            contract$office_PBSCobligated_1year<0]<-0
      contract$pPBSC<-contract$office_PBSCobligated_1year/contract$office_obligatedamount_1year
      contract$pPBSC[contract$office_obligatedamount_1year==0]<-0
      contract$pPBSC[contract$pPBSC>1]<-1

      contract$office_numberofactions_1year[is.na(contract$ContractingOfficeCode)]<-NA
      contract$office_obligatedamount_7year[is.na(contract$ContractingOfficeCode)]<-NA
      contract$office_obligatedamount_1year[is.na(contract$ContractingOfficeCode)]<-NA
      contract$office_PBSCobligated_1year[is.na(contract$ContractingOfficeCode)]<-NA
      contract$pPBSC[is.na(contract$ContractingOfficeCode)]<-NA

      contract$l_OffCA<-log(contract$office_numberofactions_1year+1)
      contract$cl_OffCA<-arm::rescale(contract$l_OffCA)
      contract$l_OffVol<-log(contract$office_obligatedamount_7year+1)
      contract$cl_OffVol<-arm::rescale(contract$l_OffVol)

      # summary(contract$l_OffVol)
      # summary(contract$cl_OffVol)
      #
      contract$c_pPBSC<-arm::rescale(contract$pPBSC)
    }

    if("ProductOrServiceCode" %in% colnames(contract) &
       file.exists(paste(local_semi_clean_path,"Office.sp_ProdServOfficeHistoryLaggedConst.txt",sep=""))){

      contract<-read_and_join_experiment( contract,
                                          "Office.sp_ProdServOfficeHistoryLaggedConst.txt",
                                          path="",
                                          directory=local_semi_clean_path,
                                          by=c("ContractingOfficeCode","fiscal_year","ProductOrServiceCode"),
                                          add_var=c("office_psc_obligatedamount_7year"),
                                          new_var_checked=FALSE,
                                          col_types="ccddddc",
                                          create_lookup_rdata=TRUE)



      # summary(contract$office_psc_obligatedamount_7year)
      contract$office_psc_obligatedamount_7year[is.na(contract$office_psc_obligatedamount_7year)|
                                                  contract$office_psc_obligatedamount_7year<0]<-0


      contract$pOffPSC<-contract$office_psc_obligatedamount_7year/contract$office_obligatedamount_7year
      contract$pOffPSC[contract$office_obligatedamount_7year==0]<-0
      contract$pOffPSC[contract$pOffPSC>1]<-1
      # summary(contract$pOffPSC)
      contract$office_psc_obligatedamount_7year[is.na(contract$ContractingOfficeCode) |
                                                  is.na(contract$ProductOrServiceCode)]<-NA
      contract$pOffPSC[is.na(contract$ContractingOfficeCode) |
                         is.na(contract$ProductOrServiceCode)]<-NA

      contract$c_pOffPSC<-arm::rescale(contract$pOffPSC)
    }

    # summary(contract$l_OffVol)
    # summary(contract$cl_OffVol)
    #

    if("EntityID" %in% colnames(contract)){
      contract<-read_and_join_experiment( contract,
                                          "Office.sp_EntityIDofficeHistoryLaggedConst.txt",
                                          path="",
                                          directory=local_semi_clean_path,
                                          by=c("EntityID","ContractingOfficeCode","fiscal_year"),
                                          add_var=c("office_entity_paircount_7year","office_entity_numberofactions_1year",
                                                    "office_entity_obligatedamount_7year"),
                                          new_var_checked=FALSE,
                                          create_lookup_rdata=TRUE,
                                          lookup_char_as_factor=TRUE)

      # summary(contract$EntityID)
      # summary(contract$office_entity_numberofactions_1year)
      # summary(contract$office_entity_paircount_7year)
      # summary(contract$office_entity_obligatedamount_7year)

      contract$office_entity_numberofactions_1year[is.na(contract$office_entity_numberofactions_1year)&
                                                     !is.na(contract$EntityID)&!is.na(contract$ContractingOfficeCode)]<-0
      contract$office_entity_paircount_7year[is.na(contract$office_entity_paircount_7year)&
                                               !is.na(contract$EntityID)&!is.na(contract$ContractingOfficeCode)]<-0

      contract$office_entity_obligatedamount_7year[(is.na(contract$office_entity_obligatedamount_7year)|
                                                      contract$office_entity_obligatedamount_7year<0)&
                                                     !is.na(contract$EntityID)&!is.na(contract$ContractingOfficeCode)]<-0
      contract$pMarket<-contract$office_entity_obligatedamount_7year/contract$office_obligatedamount_7year
      contract$pMarket[contract$office_obligatedamount_7year==0 &
                         !is.na(contract$EntityID)&!is.na(contract$ContractingOfficeCode)]<-0
      contract$pMarket[contract$pMarket>1]<-1
      # summary(contract$pMarket)

      contract$c_pMarket<-arm::rescale(contract$pMarket)
      contract$l_pairCA<-log(contract$office_entity_numberofactions_1year+1)
      contract$cl_pairCA<-arm::rescale(contract$l_pairCA)
      contract$c_pairHist<-arm::rescale(contract$office_entity_paircount_7year)

    }



    colnames(contract)[colnames(contract)=="ContractingOfficeCode"]<-"Office"
    colnames(contract)[colnames(contract)=="fiscal_year"]<-"StartFY"
  }
  if(file.exists(paste(local_semi_clean_path,"Contract.sp_ContractExercisedOptions.txt",sep=""))){
    contract<-read_and_join_experiment( contract,
                                        "Contract.sp_ContractExercisedOptions.txt",
                                        path="",
                                        directory=local_semi_clean_path,
                                        by=c("CSIScontractID"),
                                        add_var=c("AnyUnmodifiedUnexercisedOptions"
                                                  ,"AnyUnmodifiedUnexercisedOptionsWhy"
                                                  ,"UnmodifiedBaseandExercisedOptionsValue"
                                                  ,"ExercisedOptions"),
                                        new_var_checked=FALSE,
                                        create_lookup_rdata=TRUE,
                                        lookup_char_as_factor=TRUE)

    # summary(contract$ExercisedOptions)
    # summary(contract$AnyUnmodifiedUnexercisedOptions)
    # summary(factor(contract$AnyUnmodifiedUnexercisedOptionsWhy))
    # summary(contract$UnmodifiedBaseandExercisedOptionsValue)
  }

  if("Crisis" %in% colnames(contract) &
     file.exists(paste(local_semi_clean_path,"ProductOrServiceCode.ProdServHistoryCFTEcoalesceLaggedConst.txt",sep=""))){
    # summary(contract$Crisis)
    contract$OCO_GF<-contract$Crisis
    levels(contract$OCO_GF)<-
      list("GF"=c("Other","ARRA","Dis"),
           "OCO"="OCO")
    # summary(contract$OCO_GF)
    colnames(contract)[colnames(contract)=="StartFY"]<-"fiscal_year"
    contract<-read_and_join( contract,
                             "ProductOrServiceCode.ProdServHistoryCFTEcoalesceLaggedConst.txt",
                             path="",
                             directory=local_semi_clean_path,
                             by=c("fiscal_year","OCO_GF","ProductOrServiceCode"),
                             add_var=c("CFTE_Rate_1year"),
                             new_var_checked=FALSE,
                             lookup_char_as_factor=TRUE)
    colnames(contract)[colnames(contract)=="fiscal_year"]<-"StartFY"
    # summary(contract$CFTE_Rate_1year)
    contract$l_CFTE<-log(contract$CFTE_Rate_1year)
    contract$cl_CFTE<-arm::rescale(contract$l_CFTE)
  }




  if("l_Ceil" %in% colnames(contract))
    contract$cl_Ceil<-arm::rescale(contract$l_Ceil)

  if("cl_Days" %in% colnames(contract))
    contract$cl_Days<-arm::rescale(contract$l_Days)




  if("TermNum" %in% colnames(contract))
    contract$TermNum<-as.integer(as.character(factor(contract$Term,
                                                     levels=c("Terminated","Unterminated"),
                                                     labels=c(1,0))))

  if("Action_Obligation" %in% colnames(contract)){
    contract$ObligationWT<-contract$Action_Obligation
    contract$ObligationWT[contract$ObligationWT<0]<-NA
  }

  if("Action_Obligation.Then.Year" %in% colnames(contract)){
    contract$ObligationWT_Then_Year<-contract$Action_Obligation.Then.Year
    contract$ObligationWT_Then_Year[contract$ObligationWT_Then_Year<0]<-NA
  }











  #Removing l_s just to reduce size. They can be derived easily.
  contract<-contract[!colnames(contract) %in% colnames(contract)[grep("^l_",colnames(contract))]]
  contract<-contract[!colnames(contract) %in% colnames(contract)[grep("^capped_l_",colnames(contract))]]


  contract
}

#' Update a sample using a larger data frame.
#'
#' @param smp A data frame of contracts ready for statistical analysis, which must contain CSIScontractID.
#' @param full A data frame of contracts with no key missing data and which must contain CSIScontractID.
#' @col Speific columns to add, if blank, add all in full missing from sample
#' @drop_and_replace If true, drop rows from sample missing from full. Then replace them with new rows from full.
#'
#' @return The updated sample
#'
#' @details This is a function that updates samples using an updated
#' version of the population, e.g. new columns, and adds them to
#' existing samples. This might be used if a new column has been added
#' from SQL or if NA values are found in a oolumn being used in.
#' This isn't appropriate if the larger being drawn from has changed
#' in make up, for example adding a new years data.
#'
#' @examples update_sample_col_CSIScontractID(smp,def[complete,],drop_and_replace=TRUE)
#'
#' @export
update_sample_col_CSIScontractID<-function(smp,
                                           full,
                                           col=NULL,
                                           drop_and_replace=FALSE){
  #If column(s) are specified
  if(!is.null(col)){
    toadd<-full[,colnames(full) %in% c("CSIScontractID",col)]
    smp<-smp[,!colnames(smp) %in% col]
  }
  #If no column(s) specified, add all missing columns.
  else{
    full<-full %>% group_by()
    toadd<-full[,!colnames(full) %in% colnames(smp) | colnames(full)=="CSIScontractID"]
  }

  if(drop_and_replace==FALSE){
    if(ncol(toadd)==1) stop("No columns to add")
    smp<-left_join(smp,toadd)
  }
  else{
    original_l<-nrow(smp)
    smp<-inner_join(smp,toadd, by="CSIScontractID")
    rm(toadd)
    missing_l<-original_l-nrow(smp)
    if(missing_l>0){
      full<-full[,colnames(full) %in% colnames(smp)]
      if(ncol(full)<ncol(smp)){
        print(paste(colnames(smp)[!colnames(smp) %in% colnames(full)]))
        stop("Full is missing columns present in sample")
      }
      full<-full[!full$CSIScontractID %in% smp$CSIScontractID,]
      smp<-dplyr::bind_rows(smp,full[sample(nrow(full),missing_l),])
      if(nrow(smp)!=original_l) stop("Mismatched rowcount. Too few in full? This shouldn't happen.")
      #
      warning(paste(missing_l, "rows removed and replaced due to absence from full"))
    }
  }

  smp
}
