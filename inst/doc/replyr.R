## ---- echo = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width =100)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  devtools::install_github("WinVector/replyr")

## ----message=FALSE,results='hide',warning=FALSE---------------------------------------------------
library('dplyr')

## ----letexample-----------------------------------------------------------------------------------
# nice parametric function we write
ComputeRatioOfColumns <- function(d,NumeratorColumnName,DenominatorColumnName,ResultColumnName) {
  replyr::let(
    alias=list(NumeratorColumn=NumeratorColumnName,
               DenominatorColumn=DenominatorColumnName,
               ResultColumn=ResultColumnName),
    expr={
      # (pretend) large block of code written with concrete column names.
      # due to the let wrapper in this function it will behave as if it was
      # using the specified paremetric column names.
      d %>% mutate(ResultColumn=NumeratorColumn/DenominatorColumn)
    })
}

# example data
d <- data.frame(a=1:5,b=3:7)

# example application
d %>% ComputeRatioOfColumns('a','b','c')

## ----message=FALSE,results='hide',warning=FALSE---------------------------------------------------
library('dplyr')

## ----substitute-----------------------------------------------------------------------------------
d <- data.frame(Sepal_Length=c(5.8,5.7),
                Sepal_Width=c(4.0,4.4),
                Species='setosa',
                rank=c(1,2))
eval(substitute(d %>% mutate(RankColumn=RankColumn-1),
                list(RankColumn=quote(rank))))

## ----substitute2----------------------------------------------------------------------------------
eval(substitute(d %>% mutate(RankColumn=RankColumn-1),
                list(RankColumn=as.name('rank'))))

## ----substitute3, error=TRUE----------------------------------------------------------------------
eval(substitute(d %>% mutate(RankColumn=RankColumn-1),
                list(RankColumn='rank')))

## ----withex, eval=FALSE---------------------------------------------------------------------------
#  # rank <- NULL # hide binding of rank to function
#  env <- new.env()
#  assign('RankColumn',quote(rank),envir = env)
#  # assign('RankColumn',as.name('rank'),envir = env)
#  # assign('RankColumn',rank,envir = env)
#  # assign('RankColumn','rank',envir = env)
#  with(env,d %>% mutate(RankColumn=RankColumn-1))

## ----subst3---------------------------------------------------------------------------------------
replyr::let(
  alias=list(RankColumn='rank'),
  d %>% mutate(RankColumn=RankColumn-1)
)

## ----message=FALSE,results='hide',warning=FALSE---------------------------------------------------
library('dplyr')

## ----letexampleo----------------------------------------------------------------------------------
# example data
d <- data.frame(a=1:5,b=3:7)

# original function we do not have control of
ComputeRatioOfColumnsHardCoded <- function(d) {
  d %>% mutate(ResultColumn=NumeratorColumn/DenominatorColumn)
}

# wrapper to make function look parametric
ComputeRatioOfColumnsWrapped <- function(d,NumeratorColumnName,DenominatorColumnName,ResultColumnName) {
  d %>% replyr::replyr_mapRestrictCols(list(NumeratorColumn='a',
                                            DenominatorColumn='b')) %>%
    
    ComputeRatioOfColumnsHardCoded() %>%
    replyr::replyr_mapRestrictCols(list(a='NumeratorColumn',
                                        b='DenominatorColumn',
                                        c='ResultColumn'))
}

# example application
d %>% ComputeRatioOfColumnsWrapped('a','b','c')

## ----message=FALSE,results='hide',warning=FALSE---------------------------------------------------
library('dplyr')

## ----gapplyexample--------------------------------------------------------------------------------
d <- data.frame(group=c(1,1,2,2,2),
                order=c(.1,.2,.3,.4,.5))
rank_in_group <- . %>% mutate(constcol=1) %>%
          mutate(rank=cumsum(constcol)) %>% select(-constcol)
d %>% replyr::gapply('group',rank_in_group,ocolumn='order',decreasing=TRUE)

## ----summaryexample-------------------------------------------------------------------------------
d <- data.frame(x=c(1,2,2),y=c(3,5,NA),z=c(NA,'a','b'),
                stringsAsFactors = FALSE)
if (requireNamespace("RSQLite")) {
  my_db <- dplyr::src_sqlite(":memory:", create = TRUE)
  dRemote <- replyr::replyr_copy_to(my_db,d,'d')
} else {
  dRemote <- d # local stand in when we can't make remote
}

summary(dRemote)

replyr::replyr_summary(dRemote)

## ----message=FALSE,results='hide',warning=FALSE---------------------------------------------------
library('dplyr')

## ----filter---------------------------------------------------------------------------------------
values <- c(2)
dRemote %>% replyr::replyr_filter('x',values)

## -------------------------------------------------------------------------------------------------
rm(list=ls())
gc()

