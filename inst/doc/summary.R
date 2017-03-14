## ---- echo = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " # "
)
options(width =100)

## ----example--------------------------------------------------------------------------------------

d <- data.frame(x=c(NA,'b'), y=c(1,NA), stringsAsFactors= FALSE)

summary(d)

replyr::replyr_summary(d)

my_db <- dplyr::src_sqlite(":memory:", create = TRUE)
dbData <- dplyr::copy_to(my_db, d)

summary(dbData)

replyr::replyr_summary(dbData)

# glimpse works more like str or head
dplyr::glimpse(dbData)


## ----cleanup--------------------------------------------------------------------------------------
rm(list=ls())
gc()

