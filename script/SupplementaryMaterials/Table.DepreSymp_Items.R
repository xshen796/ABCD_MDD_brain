# Items used
# By XShen
# 18/05/2020

# Settings ----------------------------------------------------------------
library('dplyr')
library('ggplot2')
library(reshape2)
library(xlsx)
setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')


# Load data dictionary ----------------------------------------------------

dir.var.ls='data/ls.questionnaires.csv'
ls.var=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')

# Depressive symptoms KSADS 
# Adolescents
new.dic = ls.var[grep('^Symptom -',ls.var$Description),]
new.dic = new.dic[grep('t$',new.dic$Field.name),]

new.dic.current.y = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past.y = new.dic.ever[grep('Past',new.dic.ever$Description),]

# Parents on youth
new.dic = ls.var[grep('^Symptom -',ls.var$Description),]
new.dic = new.dic[grep('p$',new.dic$Field.name),]

new.dic.current.p = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past.p = new.dic.ever[grep('Past',new.dic.ever$Description),]



# Make table --------------------------------------------------------------

descrip.table=data.frame(Parent_past=new.dic.past.p$Field.name,
                         Parent_current=new.dic.current.p$Field.name,
                         Youth_past=new.dic.past.y$Field.name,
                         Youth_current=new.dic.current.y$Field.name,
                         Description=new.dic.past.p$Description)
descrip.table$Description=gsub(', Past','',descrip.table$Description)
descrip.table$Description=gsub(' Past','',descrip.table$Description)
descrip.table$Description=gsub('Symptom - ','',descrip.table$Description)

write.xlsx(descrip.table,file = 'Table/SuppleMaterials/DepreSymp_Items.xlsx',
           row.names = F,col.names = T,showNA = F)
