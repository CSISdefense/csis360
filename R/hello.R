# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
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

hello <- function() {
  print("Hello, world!")
}

NA.check<-function(VAR.df
  , VAR.input
  , VAR.output
  , VAR.file
){
  #Limit just to relevant columns
  NA.check.df<-subset(VAR.df
    , select=c(VAR.input,VAR.output)
  )
  #Drop all rows
  NA.check.df<-NA.check.df[!complete.cases(NA.check.df),]

  if(nrow(NA.check.df)>0){
    print(unique(NA.check.df))
    stop(paste(nrow(NA.check.df)
      ,"rows of NAs generated in "
      ,paste(VAR.output,collapse=", ")
      ,"from "
      ,VAR.file)
    )
  }
}
