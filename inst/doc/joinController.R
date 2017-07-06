## ----init----------------------------------------------------------------
# load packages
suppressPackageStartupMessages(library("dplyr"))
packageVersion("dplyr")
library("replyr")
packageVersion("replyr")

## ----data----------------------------------------------------------------
# load notional example data
my_db <- dplyr::src_sqlite(":memory:", 
                           create = TRUE)
# example data
replyr_copy_to(my_db,
               data.frame(id= c(1,1,2,2),
                          date= c(1,2,1,2),
                          weight= c(200, 180, 98, 120),
                          height= c(60, 54, 12, 14)),
               'meas1_train')
replyr_copy_to(my_db,
               data.frame(id= seq_len(length(letters)),
                          name= letters,
                          stringsAsFactors=FALSE),
               'names_facts')
replyr_copy_to(my_db,
               data.frame(pid= c(2,3),
                          date= c(2,2),
                          weight= c(105, 110),
                          width= 1),
               'meas2_train')

## ----defs----------------------------------------------------------------
# map from abstract names to realized names
trainTables <- data_frame(tableName = c('meas1', 
                                        'names', 
                                        'meas2'),
                          concreteName = c('meas1_train', 
                                           'names_facts', 
                                           'meas2_train'))
# get table references from source by concrete names
trainTables$handle <- lapply(trainTables$concreteName,
                             function(ni) {
                               tbl(my_db, ni)
                             })
# convert to full description table
tDesc <- bind_rows(
  lapply(seq_len(nrow(trainTables)),
         function(ri) {
           ni <- trainTables$tableName[[ri]]
           ti <- trainTables$handle[[ri]]
           tableDescription(ni, ti)
         }
  )
)

## ----lookdesc------------------------------------------------------------
print(tDesc %>% select(tableName, sourceClass, handle, isEmpty))
print(tDesc$columns)
print(tDesc$colClass)

# add names for printing
names(tDesc$keys) <- tDesc$tableName
print(tDesc$keys)

## ----badjoinplan---------------------------------------------------------
tryCatch(
  buildJoinPlan(tDesc),
  error = function(e) {e}
)

## ----keys----------------------------------------------------------------
# declare keys (and give them consistent names)
tDesc$keys[[1]] <- c(PatientID= 'id', MeasurementDate= 'date')
tDesc$keys[[2]] <- c(PatientID= 'id')
tDesc$keys[[3]] <- c(PatientID= 'pid', MeasurementDate= 'date')

print(tDesc$keys)

## ----keycheck------------------------------------------------------------
keysAreUnique(tDesc)

## ----plan----------------------------------------------------------------
# build the column join plan
columnJoinPlan <- buildJoinPlan(tDesc)
print(columnJoinPlan %>% 
        select(tableName, sourceColumn, resultColumn, isKey, want))

## ----plan2---------------------------------------------------------------
# decide we don't want the width column
columnJoinPlan$want[columnJoinPlan$resultColumn=='width'] <- FALSE
# double check our plan
if(!is.null(inspectDescrAndJoinPlan(tDesc, columnJoinPlan))) {
  stop("bad join plan")
}

print(columnJoinPlan %>% 
        select(tableName, sourceColumn, resultColumn, isKey, want))

## ----render1-------------------------------------------------------------
# requireNamespace checks just for strict warning hygiene in vignette
have <- c(
  requireNamespace('DiagrammeR', quietly = TRUE),
  requireNamespace('htmlwidgets', quietly = TRUE),
  requireNamespace('webshot', quietly = TRUE),
  requireNamespace('magick', quietly = TRUE),
  requireNamespace('grid', quietly = TRUE)
)
if(all(have)) {
  png <- columnJoinPlan %>%
    makeJoinDiagramSpec() %>%
    renderJoinDiagram()
  if(!is.null(png)) {
    grid::grid.raster(png)
  }
}

## ----run-----------------------------------------------------------------
# manage the temp names as in:
#  http://www.win-vector.com/blog/2017/06/managing-intermediate-results-when-using-rsparklyr/
tempNameGenerator <- makeTempNameGenerator("extmps")

# execute the left joins
results <- executeLeftJoinPlan(tDesc, columnJoinPlan, 
                               verbose= TRUE,
                               tempNameGenerator= tempNameGenerator)

## ----print---------------------------------------------------------------
dplyr::glimpse(results)

## ----execpartialtab------------------------------------------------------
# hand build table with parallel tableName and handle columns
tTab <- trainTables %>%
  select(tableName, handle)
print(tTab)
r <- executeLeftJoinPlan(tTab, columnJoinPlan, 
                         verbose= FALSE,
                         tempNameGenerator= tempNameGenerator)

## ----listmap-------------------------------------------------------------
# map of abstract table names to handles
tMap = trainTables$handle
names(tMap) <- trainTables$tableName
r <- executeLeftJoinPlan(tMap, columnJoinPlan, 
                         verbose= FALSE,
                         tempNameGenerator= tempNameGenerator)

## ----cleanup-------------------------------------------------------------
# cleanup
temps <- tempNameGenerator(dumpList= TRUE)
for(ti in temps) {
  replyr_drop_table_name(my_db, ti)
}
rm(list=ls())
gc(verbose= FALSE)
