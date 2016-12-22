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
d %>% mutate(x_isNA = is.na(x))

## -------------------------------------------------------------------------------------------------
cname <- "x"                            # column we are examining
rname <- paste(cname, "isNA", sep= '_') # where to land results
print(rname)

## ---- eval=FALSE----------------------------------------------------------------------------------
#  d[[rname]] <- is.na(d[[cname]])

## -------------------------------------------------------------------------------------------------
d %>% mutate_(RCOL = lazyeval::interp(~ is.na(cname))) %>%
      rename_(.dots = stats::setNames('RCOL', rname))

## -------------------------------------------------------------------------------------------------
# the following does not correctly name the result column
d %>% mutate_(.dots = stats::setNames(lazyeval::interp( ~ is.na(cname)),
                                      rname))

## -------------------------------------------------------------------------------------------------
# dplyr mutate_ paste stats::setNames solution
d %>% mutate_(.dots =
                stats::setNames(quote(is.na(x)),
                rname))

## -------------------------------------------------------------------------------------------------
# dplyr mutate_ paste stats::setNames solution
d %>% mutate_(.dots =
                stats::setNames(paste0('is.na(', cname, ')'),
                rname))

## -------------------------------------------------------------------------------------------------
# dplyr mutate_ lazyeval::interp solution
d %>% mutate_(RCOL =
                lazyeval::interp("is.na(cname)",
                cname = as.name(cname))) %>%
                rename_(.dots = setNames('RCOL', rname))

## -------------------------------------------------------------------------------------------------
# replyr::let solution
replyr::let(alias = list(cname = cname, rname = rname),
            expr  = {
            d %>% mutate(rname = is.na(cname))
            })

## -------------------------------------------------------------------------------------------------
# replyr::letp solution
d %>% replyr::letp(alias = list(cname = cname, rname = rname),
                   expr  = {
                   . %>% mutate(rname = is.na(cname))
                   })

