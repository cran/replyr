## ----setup----------------------------------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library("replyr"))
suppressPackageStartupMessages(library("dplyr"))
options(width = 160) 
tng <- replyr::makeTempNameGenerator('fdexample')

## ----notation1------------------------------------------------------------------------------------------------------------------------------------------------
controlTable <- dplyr::tribble(~group, ~col1, ~col2,
                               'aa',  'c1',  'c2',
                               'bb',  'c3',  'c4')
print(controlTable)

## ----notationd1-----------------------------------------------------------------------------------------------------------------------------------------------
dat1 <- dplyr::tribble(
  ~ID,          ~c1,          ~c2,          ~c3,          ~c4,
  'id1', 'val_id1_c1', 'val_id1_c2', 'val_id1_c3', 'val_id1_c4',
  'id2', 'val_id2_c1', 'val_id2_c2', 'val_id2_c3', 'val_id2_c4',
  'id3', 'val_id3_c1', 'val_id3_c2', 'val_id3_c3', 'val_id3_c4' )
print(dat1)

## ----notationd2-----------------------------------------------------------------------------------------------------------------------------------------------
namePairings <- expand.grid(seq_len(nrow(controlTable)), 
                     2:ncol(controlTable))
colnames(namePairings) <- c("controlI", "controlJ")
namePairings$coords_style1 <- 
  vapply(seq_len(nrow(namePairings)),
         function(ii) {
           as.character(paste("column:",
                              controlTable[namePairings$controlI[[ii]], 
                                           namePairings$controlJ[[ii]]]))
         },
         character(1))
namePairings$coords_style2 <- 
  vapply(seq_len(nrow(namePairings)),
         function(ii) {
           paste("group:",
                 controlTable$group[[namePairings$controlI[[ii]]]],
                 ", column:",
                 colnames(controlTable)[[namePairings$controlJ[[ii]]]])
         },
         character(1))
as.matrix(namePairings[ , c("coords_style1", "coords_style2")])

## ----dat2-----------------------------------------------------------------------------------------------------------------------------------------------------
my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
dat1db <- dplyr::copy_to(my_db, dat1, 'dat1db')
dat2 <- replyr::moveValuesToRowsQ(controlTable = controlTable,
                                  wideTableName = 'dat1db',
                                  my_db = my_db,
                                  columnsToCopy = "ID",
                                  tempNameGenerator = tng) %>%
  arrange(ID, group)
print(dat2)

## ----moveValuesToRowsQ----------------------------------------------------------------------------------------------------------------------------------------
wideTableName <- 'dat'
d <- dplyr::copy_to(my_db,
      dplyr::tribble(
        ~ID,          ~c1,          ~c2,          ~c3,          ~c4,
      'id1', 'val_id1_c1', 'val_id1_c2', 'val_id1_c3', 'val_id1_c4',
      'id2', 'val_id2_c1', 'val_id2_c2', 'val_id2_c3', 'val_id2_c4',
      'id3', 'val_id3_c1', 'val_id3_c2', 'val_id3_c3', 'val_id3_c4' ),
             wideTableName, overwrite = TRUE, temporary=TRUE)
controlTable <- dplyr::tribble(~group, ~col1, ~col2,
                                 'aa',  'c1',  'c2',
                                 'bb',  'c3',  'c4')
columnsToCopy <- 'ID'
replyr::moveValuesToRowsQ(controlTable,
                          wideTableName,
                          my_db,
                          columnsToCopy = columnsToCopy,
                          tempNameGenerator = tng) %>%
  arrange(ID, group)

## ----moveValuesToColumnsQ-------------------------------------------------------------------------------------------------------------------------------------
tallTableName <- 'dat'
d <- dplyr::copy_to(my_db,
  dplyr::tribble(
   ~ID,   ~group, ~col1,              ~col2,
   "id1", "aa",   "val_id1_gaa_col1", "val_id1_gaa_col2",
   "id1", "bb",   "val_id1_gbb_col1", "val_id1_gbb_col2",
   "id2", "aa",   "val_id2_gaa_col1", "val_id2_gaa_col2",
   "id2", "bb",   "val_id2_gbb_col1", "val_id2_gbb_col2",
   "id3", "aa",   "val_id3_gaa_col1", "val_id3_gaa_col2",
   "id3", "bb",   "val_id3_gbb_col1", "val_id3_gbb_col2" ),
         tallTableName,
         overwrite = TRUE, temporary=TRUE)
controlTable <- dplyr::tribble(~group, ~col1, ~col2,
                                 'aa',  'c1',  'c2',
                                 'bb',  'c3',  'c4')
keyColumns <- 'ID'
replyr::moveValuesToColumnsQ(keyColumns,
                             controlTable,
                             tallTableName,
                             my_db,
                             tempNameGenerator = tng) %>%
  arrange(ID)

## ----pivot----------------------------------------------------------------------------------------------------------------------------------------------------
d <- data.frame(
  index = c(1, 2, 3, 1, 2, 3),
  meastype = c('meas1','meas1','meas1','meas2','meas2','meas2'),
  meas = c('m1_1', 'm1_2', 'm1_3', 'm2_1', 'm2_2', 'm2_3'),
  stringsAsFactors = FALSE)
print(d)

# the cdata::moveValuesToColumns version
# equivalent to tidyr::spread(d, 'meastype', 'meas')
cdata::moveValuesToColumns(d,
                           columnToTakeKeysFrom = 'meastype',
                           columnToTakeValuesFrom = 'meas',
                           rowKeyColumns= 'index',
                           sep= '_') %>%
  arrange(index)

# the replyr::moveValuesToColumnsQ() version
controlTable <- replyr::buildPivotControlTable(d,
                                               columnToTakeKeysFrom = 'meastype',
                                               columnToTakeValuesFrom = 'meas',
                                               sep = "_")
print(controlTable)

dtall <- dplyr::copy_to(my_db, d, "dtall")
moveValuesToColumnsQ(keyColumns = "index",
                     controlTable = controlTable,
                     tallTableName = "dtall",
                     my_db = my_db,
                     tempNameGenerator = tng) %>% 
  arrange(index)

## ----unpivot--------------------------------------------------------------------------------------------------------------------------------------------------
d <- data.frame(
  index = c(1, 2, 3),
  info = c('a', 'b', 'c'),
  meas1 = c('m1_1', 'm1_2', 'm1_3'),
  meas2 = c('2.1', '2.2', '2.3'),
  stringsAsFactors = FALSE)
print(d)

# the cdata::moveValuesToRows() version
# equivalent to tidyr::gather(d, 'meastype', 'meas', c('meas1','meas2'))
cdata::moveValuesToRows(d,
                        nameForNewKeyColumn= 'meastype',
                        nameForNewValueColumn= 'meas',
                        columnsToTakeFrom= c('meas1','meas2')) %>%
  arrange(index, info)

# the replyr::cdata::moveValuesToRows() version
controlTable <- buildUnPivotControlTable(nameForNewKeyColumn= 'meastype',
                                         nameForNewValueColumn= 'meas',
                                         columnsToTakeFrom= c('meas1','meas2'))
print(controlTable)

keyColumns = c('index', 'info')
dwide <- dplyr::copy_to(my_db, d, "dwide")
moveValuesToRowsQ(controlTable = controlTable,
                  wideTableName = "dwide",
                  my_db = my_db,
                  columnsToCopy = keyColumns,
                  tempNameGenerator = tng) %>%
  arrange(index, info)

## ----pdat-----------------------------------------------------------------------------------------------------------------------------------------------------
purchaseDat <- dplyr::copy_to(my_db, dplyr::tribble(
  ~ID, ~Q1purchases, ~Q2purchases, ~Q1rebates, ~Q2rebates,
    1,           20,           10,          5,          3,
    2,            5,            6,         10,         12),
  'purchaseDat')
print(purchaseDat)

## ----fluidex--------------------------------------------------------------------------------------------------------------------------------------------------
controlTable <- dplyr::tribble(
  ~group, ~purchases,    ~rebates,
  "Q1",   "Q1purchases", "Q1rebates",
  "Q2",   "Q2purchases", "Q2rebates")
print(controlTable)
purchasesTall <- moveValuesToRowsQ(columnsToCopy = "ID", 
                                   controlTable = controlTable, 
                                   wideTableName = "purchaseDat",
                                   my_db = my_db,
                                   tempNameGenerator = tng)
print(purchasesTall)

# perform the calculation in one easy step
calc <- purchasesTall %>% 
  mutate(purchasesPerRebate = purchases/rebates) %>%
  compute(name = "purchasesTallC")
print(calc)

# move what we want back
controlTable <- controlTable %>%
  mutate(purchasesPerRebate = 
           paste0(group, "purchasesPerRebate"))
print(controlTable)

# notice the step back is not a single
# pivot or un-pivot
# due to the larger controlTable
# (especially if there were more quarters)
result <- moveValuesToColumnsQ(keyColumns = "ID",
                               controlTable = controlTable,
                               tallTableName = "purchasesTallC",
                               my_db = my_db,
                               tempNameGenerator = tng)
print(result)

## ----tc-------------------------------------------------------------------------------------------------------------------------------------------------------
controlTable <- dplyr::tribble(~group, ~col1, ~col2,
                                 'aa',  'c1',  'c2',
                                 'bb',  'c3',  'c4')
tallTableName <- 'dc'
d <- dplyr::copy_to(my_db, controlTable, tallTableName)
keyColumns <- NULL
wideTableName <- 'dw'
dw <- moveValuesToColumnsQ(keyColumns,
                           controlTable,
                           tallTableName,
                           my_db) %>%
  compute(name = wideTableName)
print(dw)

## ----tcr------------------------------------------------------------------------------------------------------------------------------------------------------
moveValuesToRowsQ(controlTable,
                  wideTableName,
                  my_db)

## ----cleanup--------------------------------------------------------------------------------------------------------------------------------------------------
for(ti in tng(dumpList = TRUE)) {
  dplyr::db_drop_table(my_db, ti)
}
DBI::dbDisconnect(my_db)

