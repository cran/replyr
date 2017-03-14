## ---- echo = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " # "
)
options(width =100)

## -------------------------------------------------------------------------------------------------
d <- data.frame(x = c(1, NA))
print(d)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  d$x_isNA <- is.na(d$x)

## -------------------------------------------------------------------------------------------------
library("dplyr")
packageVersion("dplyr")
d %>% mutate(x_isNA = is.na(x))

## -------------------------------------------------------------------------------------------------
cname <- "x"                            # column we are examining
rname <- paste(cname, "isNA", sep= '_') # where to land results
print(rname)

## ---- eval=FALSE----------------------------------------------------------------------------------
#  d[[rname]] <- is.na(d[[cname]])

## -------------------------------------------------------------------------------------------------
if  (requireNamespace("lazyeval")) {
  print(d %>%
    mutate_(.dots = stats::setNames(list(
      lazyeval::interp(~ is.na(VAR),
                       VAR = as.name(cname))), rname)))
}

## ---- error=TRUE----------------------------------------------------------------------------------
if  (requireNamespace("lazyeval")) {
  print(d %>% mutate_(RCOL = lazyeval::interp(RES ~ is.na(VAR),
                                              VAR= as.name(cname),
                                              RES= as.name(rname))))
}

## -------------------------------------------------------------------------------------------------
d %>% mutate_(.dots =
    stats::setNames(list(substitute(is.na(XVAR),list(XVAR=cname))),
                    rname))

## -------------------------------------------------------------------------------------------------
# dplyr mutate_ paste stats::setNames solution
d %>% mutate_(.dots =
                stats::setNames(paste0('is.na(', cname, ')'),
                rname))

## -------------------------------------------------------------------------------------------------
# dplyr mutate_ lazyeval::interp solution
if  (requireNamespace("lazyeval")) {
  print(d %>% mutate_(.dots =
                        stats::setNames(list(lazyeval::interp("is.na(cname)",
                                         cname = as.name(cname))), rname)))
}

## -------------------------------------------------------------------------------------------------
# wrapr::let solution
wrapr::let(alias = list(cname = cname, rname = rname),
            expr  = {
            d %>% mutate(rname = is.na(cname))
            })

## -------------------------------------------------------------------------------------------------
# replyr::letp solution
d %>% replyr::letp(alias = list(cname = cname, rname = rname),
                   expr  = {
                   . %>% mutate(rname = is.na(cname))
                   })

