
# Basic setups ------------------------------------------------------------

library(dplyr)
setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')
load('result/i.Main_result/FAMrisk.behav.MainModel.RData')

# A table for familial risk to reporter difference in bulk measures -----------------
target.table=famRisk.diffsDepre_bulk_measures

target.table[,4:6]=round(target.table[,4:6],3)
target.table[,8]=formatC(target.table[,8], format = "e", digits = 2)

target.table = target.table[,c(1,2,4,5,8)]
target.table$dependent = gsub('KSADS\\.','',target.table$dependent)
target.table$dependent = gsub('\\.diffs','',target.table$dependent)

target.table$factor = gsub('depression_','',target.table$factor)
target.table$factor = gsub('_noManiaVision','',target.table$factor)
target.table$factor = gsub('bio','',target.table$factor)

target.table=target.table[!grepl('current',target.table$dependent),]

table.ready = data.frame(target.table[grep('Mother',target.table$factor),c(1,3:5)],
                         target.table[grep('Father',target.table$factor),c(3:5)],
                         target.table[grep('parents',target.table$factor),c(3:5)])

write.csv(target.table,file='Table/Supple_FamRisk_diffs_bulkMeasure.csv')


