## ---- echo = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width =100)

## ----start----------------------------------------------------------------------------------------
library('dplyr')
library('replyr')
d <- data.frame(x=c(1,2,2),
                y=c(3,5,NA),
                z=c(NA,'a','b'),
                stringsAsFactors = FALSE)
print(d)

if (requireNamespace("RSQLite")) {
  my_db <- dplyr::src_sqlite(":memory:", create = TRUE)
  # my_db <- sparklyr::spark_connect(version='2.0.0', master = "local")
  class(my_db)
  dRemote <- replyr::replyr_copy_to(my_db,d,'d',rowNumberColumn='rowNum')
} else {
  dRemote <- d # local stand in when we can't make remote
}
print(dRemote)

## ----completecases, error=TRUE--------------------------------------------------------------------
complete.cases(d)

complete.cases(dRemote)

## ----cc2, error=TRUE------------------------------------------------------------------------------
d %>% filter(complete.cases(.))

dRemote %>% filter(complete.cases(.))

## ----completecasesE1, error=TRUE------------------------------------------------------------------
d$rowNum <- seq_len(nrow(d))
d %>% mutate_all(funs(is.na)) %>%
  mutate(nNAinRow=rowSums(.)-rowNum)

dRemote %>% mutate_all(funs(is.na)) %>%
  mutate(nNAinRow=rowSums(.)-rowNum)

## ----mutateif, error=TRUE-------------------------------------------------------------------------
dRemote %>% mutate_if(TRUE,is.na) # not correct code, just to trigger "local sources" msg

## ----countnacolumns-------------------------------------------------------------------------------
dRemote %>% mutate(nna=0) %>%
  mutate(nna=nna+ifelse(is.na(x),1,0)) %>% 
  mutate(nna=nna+ifelse(is.na(y),1,0)) %>% 
  mutate(nna=nna+ifelse(is.na(z),1,0))  

## ----countforloop---------------------------------------------------------------------------------
cols = setdiff(colnames(dRemote),'rowNum')
dRemote %>% mutate(nna=0) -> dTmp
for(ci in cols) {
  dTmp %>% 
    mutate_(.dots=stats::setNames(paste0('nna+ifelse(is.na(',ci,'),1,0)'),'nna')) -> 
    dTmp
}
print(dTmp)

## ----countforloopr--------------------------------------------------------------------------------
cols = setdiff(colnames(dRemote),'rowNum')
dRemote %>% mutate(nna=0) -> dTmp
for(ci in cols) {
  let(list(TARGETCOL=ci),
      dTmp %>% mutate(nna=nna+ifelse(is.na(TARGETCOL),1,0)) -> dTmp
  )
}
print(dTmp)

## ----naomit, error=TRUE---------------------------------------------------------------------------
na.omit(d)

na.omit(dRemote)

## ----cleanup--------------------------------------------------------------------------------------
rm(list=ls())
gc()

