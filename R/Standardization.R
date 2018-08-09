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


#' Prepare Labels And Colors
#'
#' @param data the data frame to be joined
#' @param var the variables (column names) for which to prepare colors; by default all will be done
#' @param na_replaced if TRUE, replace NA values with var before adding colors
#' @param path The location of the lookup file
#'
#' @return A new data frame built on the variable var. It will
#' include colors, and order, and proper name labels.
#'
#' @details This function applies standard colors and orders to a
#' column var in a data frame data. Colors and order are drawn from pre-existing lookup tables.
#' When values are missing or wrong, these tables must be manually updated.
#' This function is badly optimized, reading in multiple csvs every time.
#' It is intend for use in data preparation source code and not to be used in a
#' real time web environment.
#'
#' @examples FullData<-standardize_variable_names(Path,
#'   FullData)
#'
#' @export
prepare_labels_and_colors<-function(data
                                    ,var=NULL
                                    ,na_replaced=FALSE
                                    ,path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/style/"
                                    #                                  ,VAR.override.coloration=NA
                                    ,missing_allowed=FALSE
)
{
  if(na_replaced==TRUE){
    data<-replace_nas_with_unlabeled(data,var)
  }

  data<-as.data.frame(data)

  #Confirm that the category is even available in the data set.
  if(!is.null(var)){
    if(!var %in% names(data)){
      stop(paste(var,"column is not found in data."))
    }
  }

  #Read in coloration
  coloration<-read.csv(

    paste(path,"Lookup_Coloration.csv",sep=""),
    header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE,
    stringsAsFactors=FALSE
  )

  #Fix oddities involving coloration text
  coloration$variable <- gsub("\\\\n","\n",coloration$variable)
  coloration$Label <- gsub("\\\\n","\n",coloration$Label)


  #Translate the category name into the appropriate coloration.key
  #This is used because we have more category names than coloration.key
  column_key<-get_column_key(data)

  #If a column has been passed
  if(!is.null(var)){
    column_key<-subset(column_key, column==var)

    #Should adjust this to give proper errors for multiple vars
    #when only one is missing
    if(any(is.na(column_key$coloration.key))){
      if(missing_allowed)
        return(NA)
      else
        stop(paste(var,"is missing from Lookup_column_key.csv"))
    }

  }
  else {
    column_key<-subset(column_key, !is.na(coloration.key))
  }

  names.data<-NULL
  for(v in (1:nrow(column_key))){
    #Limit the lookup table to those series that match the variable
    labels_category_data<-subset(coloration, coloration.key==
                                   column_key$coloration.key[v] )

    if(anyDuplicated(labels_category_data$variable)>0){
      print(labels_category_data$variable[
        duplicated(labels_category_data$variable)])
      stop(paste("Lookup_Coloration.csv has"
                 ,sum(duplicated(labels_category_data$variable))
                 ,"duplicate value(s) for category="
                 ,column_key$coloration.key[v], ". See above for a list of missing labels")
      )
    }
    c<-as.character(column_key$column[v])
    #Check for any values in the current field that are not assigned a color.
    NA.labels<-subset(data,!(data.frame(data)[,c] %in% labels_category_data$variable))



    if (nrow(NA.labels)>0){
      #Unlabeled is highly standardized, adding automatically
      if(length(unique(NA.labels[,c]))==1 &
         !is.na(unique(NA.labels[1,c]))&
         unique(NA.labels[1,c])=="Unlabeled")
      {

        labels_category_data<-rbind(labels_category_data,
                                    data.frame(coloration.key=column_key$coloration.key[v],
                                               variable="Unlabeled",
                                               Label="Unlabeled",
                                               Display.Order= 999,
                                               Color="reddish gray",
                                               RGB= "#967878",
                                               shape=NA,
                                               size=NA,
                                               alpha=NA,
                                               text.color="default grey",
                                               text.RGB="#554449"
                                    ))
      }
      else{
        if(missing_allowed)
          return(NA)
        else{
          #Otherwise create an error.
          print(unique(NA.labels[,c]))
          stop(paste("Lookup_Coloration.csv is missing"
                     ,length(unique(NA.labels[,c]))
                     ,"label(s) for category="
                     ,c, ". See above for a list of missing labels.")

          )
        }
      }
    }

    labels_category_data<-subset(labels_category_data
                                 , variable %in% unique(data[,c]))


    #Order the names.data and then pass on the same order to the actual data in data
    labels_category_data<-labels_category_data[order(labels_category_data$Display.Order),]
    labels_category_data$column<-c
    names.data<-rbind(names.data,labels_category_data)
  }
  names.data
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
#' @param   labels_and_colors A csis360 lookup data.frame with factor information
#' @param   group If TRUE aggregate
#'
#' @return Returns a tibble of formatted data
#'
#' @export
format_data_for_plot <- function(data, fy_var, y_var, share = FALSE, start_fy = NA, end_fy = NA,
                                 color_var="None", facet_var="None", labels_and_colors=NULL, group=TRUE){

  shown_data <- data

  breakout <- c(color_var, facet_var)
  breakout <- breakout[breakout != "None"]

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
    shown_data %<>%
      filter_(paste0(fy_var, ">=", as.character(start_fy), "&", fy_var,
                     "<=", as.character(end_fy)))
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
      if(facet_var=="None" | facet_var == color_var)
        share_vars <- c(-1)
      else
        share_vars <- c(-1,-2)

      # spread the shares breakout variable across multiple columns
      shown_data %<>%
        spread_(color_var, y_var)

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
      shown_data %<>% select(-total)

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
      shown_data %<>%
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
                labels=c(subset(labels_and_colors,column==facet_var)$Label)
        )
    }
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
#' @export
transform_contract<-function(
  contract
){

  # contract$pNewWorkUnmodifiedBaseAndAll<-as.numeric(as.character(contract$pNewWorkUnmodifiedBaseAndAll))
  #Newwork and change
  # contract$pNewWork3Sig<-round(
  # contract$pNewWorkUnmodifiedBaseAndAll,3)



  #Customer
  if(!"Is.Defense" %in% colnames(contract) & "Who" %in% colnames(contract)){
    contract$Is.Defense<-as.character(contract$Who)
    contract$Is.Defense[contract$Is.Defense %in%
                          c("Air Force","Army",
                            "Navy","Other DoD","Uncategorized"  )
                        ]<-"Defense"
    contract$Is.Defense<-factor(contract$Is.Defense)


    #b_ODoD
    contract$b_ODoD<-contract$Who
    levels(contract$b_ODoD)<- list("1"=c("Other DoD"),
                                   "0"=c("Air Force","Army","Navy"))
    contract$b_ODoD[contract$b_ODoD=="Uncategorized"]<-NA
    contract$b_ODoD<-as.integer(as.character(contract$b_ODoD))


  }


  #PSR_What
  if("PSR_What" %in% colnames(contract)){
    contract$PSR_What<-factor(paste(as.character(contract$PSR),
                                    as.character(contract$What),sep="."))
    contract$PSR_What[contract$PSR_What=="Unlabeled"]<-NA
  }
  #b_CBre
  contract$b_CBre<-ifelse(contract$CBre=="Ceiling Breach",1,NA)
  contract$b_CBre[contract$CBre=="None"]<-0


  #Create a jittered version of CBre for display purposes
  #Unlike geom_jitter, this caps values at 0 and 1
  contract$j_CBre<-jitter_binary(contract$b_CBre)


  #b_Term
  contract$b_Term<-ifelse(contract$Term=="Terminated",1,NA)
  contract$b_Term[contract$Term=="Unterminated"]<-0

  #Create a jittered version of Term for display purposes
  #Unlike geom_jitter, this caps values at 0 and 1
  contract$j_Term<-jitter_binary(contract$b_Term)

  #n_CBre
  if ("ChangeOrderBaseAndAllOptionsValue" %in% colnames(contract) ){
    contract$pChangeOrderUnmodifiedBaseAndAll<-contract$ChangeOrderBaseAndAllOptionsValue/
      contract$UnmodifiedContractBaseAndAllOptionsValue
    contract$pChangeOrderUnmodifiedBaseAndAll[
      is.na(contract$pChangeOrderUnmodifiedBaseAndAll) & contract$SumOfisChangeOrder==0]<-0
    contract$pChangeOrderUnmodifiedBaseAndAll<-as.numeric(as.character(contract$pChangeOrderUnmodifiedBaseAndAll))
    contract$pChange3Sig<-round(
      contract$pChangeOrderUnmodifiedBaseAndAll,3)
    contract$CRai <- cut2(
      contract$pChangeOrderUnmodifiedBaseAndAll,c(
        -0.001,
        0.001,
        0.15)
    )

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



  #Should include this in the original data frame but for now can drive it.
  contract$n_CBre<-contract$ChangeOrderBaseAndAllOptionsValue

  #l_CBre
  contract$l_CBre<-NA
  contract$l_CBre[contract$b_CBre==1 & !is.na(contract$b_CBre)]<-
    log(contract$n_CBre[contract$b_CBre==1 & !is.na(contract$b_CBre)])

  #l_Ceil
  contract$l_Ceil<-log(contract$UnmodifiedContractBaseAndAllOptionsValue)
  contract$l_Ceil[is.infinite(contract$l_Ceil)]<-NA
  if ("UnmodifiedCurrentCompletionDate" %in% colnames(contract) ){
    contract$UnmodifiedCurrentCompletionDate<-as.Date(contract$UnmodifiedCurrentCompletionDate)
  }
  contract<-contract %>% group_by(Ceil) %>%
    mutate(ceil.median.wt = median(UnmodifiedContractBaseAndAllOptionsValue))

  if (identical(levels(contract$Ceil),c("[0,15k)",
                               "[15k,100k)",
                               "[100k,1m)",
                               "[1m,10m)",
                               "[10m,75m)",
                               "[75m+]"
  ))){
    contract$Ceil.Simple<-contract$Ceil
    levels(contract$Ceil.Simple)<- list("0k - <100k"=c("[15k,100k)",
                                                       "[0,15k)"),
                                        "100k - <10m"=c("[1m,10m)",
                                                        "[100k,1m)"),
                                        "10m+"=c("[75m+]",
                                                 "[10m,75m)"))

    contract$Ceil.Big<-contract$Ceil
    levels(contract$Ceil.Big)<- list("0k - <100k"=c("[15k,100k)",
                                                    "[0,15k)"),
                                     "100k - <10m"=c("[1m,10m)",
                                                     "[100k,1m)"),
                                     "10m - <75m"=c("[10m,75m)"),
                                     "75m+"=c("[75m+]"))

    contract$Ceil.1m<-contract$Ceil
    levels(contract$Ceil.1m)<- list("0k - <1m"=c("[0,15k)",
                                                  "[15k,100k)",
                                                  "[100k,1m)"
                                                  ),
                                     "1m - <10m"=c("[1m,10m)"),
                                     "10m - <75m"=c("[10m,75m)"),
                                     "75m+"=c("[75m+]"))
  } else if (identical(levels(contract$Ceil),c("0 - <15k",
                               "15k - <100k",
                               "100k - <1m",
                               "1m - <10m",
                               "10m - <75m",
                               "75m+"
  ))){
    contract$Ceil.Simple<-contract$Ceil
    levels(contract$Ceil.Simple)<- list("0k - <100k"=c("15k - <100k",
                                                       "0 - <15k"),
                                        "100k - <10m"=c("1m - <10m",
                                                        "100k - <1m"),
                                        "10m+"=c("75m+",
                                                 "10m - <75m"))

    contract$Ceil.Big<-contract$Ceil
    levels(contract$Ceil.Big)<- list("0k - <100k"=c("15k - <100k",
                                                    "0 - <15k"),
                                     "100k - <10m"=c("1m - <10m",
                                                     "100k - <1m"),
                                     "10m - <75m"=c("10m - <75m"),
                                     "75m+"=c("75m+"))
    contract$Ceil.1m<-contract$Ceil
    levels(contract$Ceil.1m)<- list("0k - <1m"=c("15k - <100k",
                                                   "0 - <15k",
                                                   "100k - <1m"),
                                    "1m - <10m"=c("1m - <10m"),
                                    "10m - <75m"=c("10m - <75m"),
                                    "75m+"=c("75m+"))
  }



  #l_Days
  contract$l_Days<-NA
  contract$l_Days[!is.na(contract$UnmodifiedDays)&contract$UnmodifiedDays>0]<-
    log(contract$UnmodifiedDays[!is.na(contract$UnmodifiedDays)&contract$UnmodifiedDays>0])
  contract$l_Days[is.infinite(contract$l_Days)]<-NA


  contract$UnmodifiedYearsFloat<-contract$UnmodifiedDays/365.25
  contract$UnmodifiedYearsCat<-floor(contract$UnmodifiedYearsFloat)
  contract$Dur[contract$UnmodifiedYearsCat<0]<-NA

  contract$Dur.Simple<-as.character(contract$Dur)
  contract$Dur.Simple[contract$Dur.Simple %in% c(
    "[0 months,~2 months)",
    "[~2 months,~7 months)",
    "[~7 months-~1 year]")]<-"<~1 year"
  contract$Dur.Simple<-factor(contract$Dur.Simple,
                              levels=c("<~1 year",
                                       "(~1 year,~2 years]",
                                       "(~2 years+]"),
                              ordered=TRUE
  )



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
    contract$n_Comp<-as.numeric(as.character(contract$n_Comp))



    contract$n_Offr<-contract$Offr
    levels(contract$n_Offr) <-
      list("1"=c("1"),
           "2"=c("2"),
           "3"=c("3-4"),
           "4"=c("5+"))
    contract$n_Offr<-as.integer(as.character(contract$n_Offr))
    contract$n_Offr[contract$b_Comp==0 & !is.na(contract$b_Comp)]<-0
    contract$n_Offr[is.na(contract$b_Comp)]<-NA
    contract$CompOffr<-factor(contract$n_Offr)
    levels(contract$CompOffr) <-
      list("No Competition"="0",
           "1 offer"="1",
           "2 offers"="2",
           "3-4 offers"="3",
           "5+ offers"="4")


    #l_Offr
    contract$l_Offr<-log(contract$UnmodifiedNumberOfOffersReceived)
    contract$l_Offr[is.infinite(contract$l_Offr)]<-NA

    contract$cb_Comp<-scale(contract$b_Comp)
    contract$cn_Comp<-scale(contract$n_Comp)
    contract$cn_Offr<-scale(contract$n_Offr)
    contract$cl_Offr<-scale(contract$l_Offr)

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
  }


  #b_Intl
  if("Intl" %in% colnames(contract)){
    contract$b_Intl<-contract$Intl
    contract$b_Intl[contract$b_Intl=="Unlabeled"]<-NA
    levels(contract$b_Intl) <-
      list("0"=c("Just U.S."),
           "1"=c("Any International"))
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



  # contract$DecisionTree<-as.character(contract$MaxOfDecisionTree)
  # contract$DecisionTree[
  #   contract$DecisionTree=="Excluded"|
  #     is.na(contract$DecisionTree)]<-"All Other"
  # contract$DecisionTree<-factor(contract$DecisionTree,levels=c("OCO","Disaster","ARRA","All Other"))
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

    if("MaxOfDecisionTree" %in% colnames(contract)){
      #Crisis Dataset
      # contract$ARRA<-0
      # contract$ARRA[contract$MaxOfDecisionTree=="ARRA"]<-1
      # contract$Dis<-0
      # contract$Dis[contract$MaxOfDecisionTree=="Disaster"]<-1
      # contract$OCO<-0
      # contract$OCO[contract$MaxOfDecisionTree=="OCO"]<-1
      contract$Crisis<-contract$MaxOfDecisionTree
      levels(contract$Crisis)[levels(contract$Crisis)=="Excluded"]<-"Other"
      contract$Crisis[is.na(contract$Crisis)]<-"Other"
      levels(contract$Crisis)[levels(contract$Crisis)=="Disaster"]<-"Dis"
      contract$Crisis<-factor(contract$Crisis,c("Other","ARRA","Dis","OCO"))
    }
  }

  #Calendar Year
  contract$CYear<-year(contract$SignedDate)

  #NAICS
  if("NAICS" %in% colnames(contract)){
    if(file.exists("output//naics_join.Rdata")){
      load("output//naics_join.Rdata")

      # contract<-left_join(contract,NAICS_join, by=c("StartFY"="StartFY",
      #                                               "NAICS"="NAICS_Code"))

      contract$NAICS<-as.integer(as.character(contract$NAICS))
      contract$NAICS5<-as.integer(substr(contract$NAICS,1,5))
      contract$NAICS4<-as.integer(substr(contract$NAICS,1,4))
      contract$NAICS3<-as.integer(substr(contract$NAICS,1,3))
      contract$NAICS2<-create_naics2(contract$NAICS)
      contract$StartCY<-contract$StartFY

      contract<-left_join(contract,NAICS6_join, by=c("StartCY"="CalendarYear",
                                                     "NAICS"="NAICS6"))

      contract<-left_join(contract,NAICS5_join, by=c("StartCY"="CalendarYear",
                                                     "NAICS5"="NAICS5"))

      contract<-left_join(contract,NAICS4_join, by=c("StartCY"="CalendarYear",
                                                "NAICS4"="NAICS4"))
      contract<-left_join(contract,NAICS3_join, by=c("StartCY"="CalendarYear",
                                                     "NAICS3"="NAICS3"))
      contract<-left_join(contract,NAICS2_join, by=c("StartCY"="CalendarYear",
                                                     "NAICS2"="NAICS2"))



      #Remove 0s, they make no sense, source must be one contractors in field have 0 obligations, which is just missing data really
      contract$HHI_lag1[contract$HHI_lag1==0]<-NA
      contract$c_HHI_lag1<-scale(contract$HHI_lag1)

      contract$l_HHI_lag1<-log(contract$HHI_lag1)
      contract$cl_HHI_lag1<-scale(contract$l_HHI_lag1)
    }

  }

  #Office
  if("$Office" %in% colnames(contract)){
    contract$ContractingOfficeCode<-as.character(contract$Office)
    contract<-csis360::read_and_join( contract,
                                      "Office.ContractingOfficeCode.txt",
                                      path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                      directory="office\\",
                                      by="ContractingOfficeCode",
                                      add_var=c("PlaceIntlPercent","CrisisPercent"),
                                      new_var_checked=FALSE)



    contract$OffIntl<-contract$PlaceIntlPercent

    contract$OffPl99<-Hmisc::cut2(contract$OffIntl,c(0.01,0.50))
    levels(contract$OffPl99) <-
      list("US99"=c("[0.00,0.01)"),
           "Mixed"=c("[0.01,0.50)"),
           "Intl"=c("[0.50,1.00]"))


    contract<-contract[ ,!colnames(contract) %in% c("ContractingOfficeCode")]

    contract$Reach6<-factor(paste(contract$OffPl99,contract$Intl,sep="-"))
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

    colnames(contract)[colnames(contract)=="CrisisPercent"]<-"OffCri"
    contract$c_OffCri<-scale(contract$OffCri)

    contract$OffCri<-contract$CrisisPercent
  }
  if("$ProductServiceOrRnDarea" %in% colnames(contract)){
    contract$ProductOrServiceCode<-as.character(contract$ProdServ)
    contract<-csis360::read_and_join( contract,
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
                                      new_var_checked=FALSE)

    contract$ProductServiceOrRnDarea<-factor(contract$ProductServiceOrRnDarea)
    contract$ProductOrServiceArea<-factor(contract$ProductOrServiceArea)
    contract$HostNation3Category<-factor(contract$HostNation3Category)
    contract$CrisisProductOrServiceArea<-factor(contract$CrisisProductOrServiceArea)
    contract$ProductOrServiceCodeText<-factor(contract$ProductOrServiceCodeText)
  }


  contract$cl_Ceil<-scale(contract$l_Ceil)
  contract$cl_Days<-scale(contract$l_Days)





  contract$TermNum<-as.integer(as.character(factor(contract$Term,
                                                   levels=c("Terminated","Unterminated"),
                                                   labels=c(1,0))))

  contract$ObligationWT<-contract$Action.Obligation
  contract$ObligationWT[contract$ObligationWT<0]<-NA

  if(colnames(contract) %in% "UnmodifiedContractBaseAndAllOptionsValue"){
    contract<-contract %>% group_by(Ceil) %>%
      mutate(ceil.median.wt = median(UnmodifiedContractBaseAndAllOptionsValue))
  }


  contract$UnmodifiedYearsFloat<-contract$UnmodifiedDays/365.25
  contract$UnmodifiedYearsCat<-floor(contract$UnmodifiedYearsFloat)
  contract$Dur[contract$UnmodifiedYearsCat<0]<-NA




  contract
}


