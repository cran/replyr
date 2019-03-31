## ---- echo = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " # ",
  fig.width = 7
)
options(width =100)

## ----setup, warning=FALSE, message=FALSE----------------------------------------------------------
library("dplyr")
library("replyr")

## ----sumstat_intervals----------------------------------------------------------------------------
sumstat_intervals = function(dframe, colname, groupcolname = NULL) {
  mapping = list(COLNAME  =colname,
                 GROUPCOLNAME = groupcolname)
  let(alias = mapping,
      {
        if(!is.null(groupcolname)) {
          dframe <- group_by(dframe, GROUPCOLNAME)
        }
        summarize(dframe, 
                  sdlower = mean(COLNAME)-sd(COLNAME),
                  mean = mean(COLNAME),
                  sdupper = mean(COLNAME) + sd(COLNAME),
                  iqrlower = median(COLNAME)-0.5*IQR(COLNAME),
                  median = median(COLNAME),
                  iqrupper = median(COLNAME)+0.5*IQR(COLNAME))
      })
}

## ----iris1----------------------------------------------------------------------------------------
sumstat_intervals(iris, "Sepal.Length")

## ----iris2----------------------------------------------------------------------------------------
sumstat_intervals(iris, "Sepal.Length", "Species")

## ----iris3----------------------------------------------------------------------------------------
sumstat_intervals(iris, "Petal.Length", "Species")

