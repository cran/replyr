## ----setup, include=FALSE------------------------------------------------
# note: employeeanddate is likely built as a cross-product
#       join of an employee table and set of dates of interest
#       before getting to the join controller step.  We call
#       such a table "row control" or "experimental design."

my_db <- dplyr::src_sqlite(":memory:",
                           create = TRUE)

exDesc <- replyr:::example_employeeAndDate(my_db)

## ----tableNames----------------------------------------------------------
tableNames <- c('employeeanddate',
                'revenue',
                'activity',
                'orgtable')

## ----builddesc-----------------------------------------------------------
suppressPackageStartupMessages(library("dplyr"))
library("replyr")

tDesc <- tableNames %>%
  lapply(
    function(ni) {
      replyr::tableDescription(ni,
                               dplyr::tbl(my_db, ni),
                               keyInspector= key_inspector_sqlite)
    }) %>%
  bind_rows()

print(tDesc[, c('tableName', 'handle', 'keys', 'columns'), ])

## ----plan1---------------------------------------------------------------
columnJoinPlan <- buildJoinPlan(tDesc, check= FALSE)
print(columnJoinPlan[, c('tableName', 'sourceColumn', 'resultColumn', 'isKey')])

## ----rekey---------------------------------------------------------------
columnJoinPlan$resultColumn[columnJoinPlan$sourceColumn=='id'] <- 'eid'
print(columnJoinPlan[, c('tableName', 'sourceColumn', 'resultColumn', 'isKey')])

## ----check1--------------------------------------------------------------
print(paste("issues:", inspectDescrAndJoinPlan(tDesc, columnJoinPlan)))

## ----sort----------------------------------------------------------------
sorted <- NULL
# requireNamespace checks just for strict warning hygiene in vignette
if(requireNamespace('igraph', quietly = TRUE)) {
  sorted <- topoSortTables(columnJoinPlan, 'employeeanddate')
  plot(sorted$dependencyGraph)
  print(sorted$tableOrder)
}

## ----check2--------------------------------------------------------------
if(!is.null(sorted)) {
  print(paste("issues:", inspectDescrAndJoinPlan(tDesc, 
                                                 sorted$columnJoinPlan)))
}

## ----render1-------------------------------------------------------------
# requireNamespace checks just for strict warning hygiene in vignette
if(!is.null(sorted)) {
  have <- c(
    requireNamespace('DiagrammeR', quietly = TRUE),
    requireNamespace('htmlwidgets', quietly = TRUE),
    requireNamespace('webshot', quietly = TRUE),
    requireNamespace('magick', quietly = TRUE),
    requireNamespace('grid', quietly = TRUE)
  )
  if(all(have)) {
    png <- sorted$columnJoinPlan %>%
      makeJoinDiagramSpec() %>%
      renderJoinDiagram()
    if(!is.null(png)) {
      grid::grid.raster(png)
    }
  }
}

## ----steps---------------------------------------------------------------
if(!is.null(sorted)) {
  print("join plan execution log")
  res <- executeLeftJoinPlan(tDesc, 
                             sorted$columnJoinPlan,
                             verbose = TRUE)
  print("join results")
  dplyr::glimpse(res)
}

## ----cleanup-------------------------------------------------------------
for(ni in tableNames) {
  replyr_drop_table_name(my_db, ni)
}
rm(list=ls())
gc(verbose = FALSE)

