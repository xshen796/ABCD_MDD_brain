
# Basic settings ----------------------------------------------------------

setwd("Z:/Documents/ActiveProject/ImagingProject/Adolescent_MDD_brain")
source('FUNs/wholeB_correction.R')
source('FUNs/reorg_result_web/reorg_result_web.R')
library(dplyr)
library(data.table)
library(rmarkdown)
library(knitr)
library(kableExtra)


# Inputs and outputs ------------------------------------------------------

original.data.obj = 'result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData'
rmd.output.dir = 'script/SupplementaryMaterials/Summary_result_web.Rmd'
final.render.dir = '../../result/summary_result'
intermediate.result.obj = 'result/i.Main_result/YouthDepre_brain_reorganised_forweb.RData'

# The following settings often don't change
ls.result.modality = 'FUNs/reorg_result_web/result.ls.txt'
label_replace = 'FUNs/reorg_result_web/ls.GM_WM_region_replacement_labels.csv'
template.table.code = "FUNs/reorg_result_web/kable_table_code.txt"
rmd.header.file = 'FUNs/reorg_result_web/header.Rmd'

# Tables for Main results (depressive symptoms and MDD) -------------------
rm(list = ls(pattern = '^result.'))
load(original.data.obj)
TargetResult = result.YouthDepre.region.covWholeB

# FWE correction
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.YouthDepre.region.covWholeB=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)

rm(TargetResult)

# Relabel
for (i in ls(pattern = '^result.')){
  result.tmp=get(i)
  result.tmp$factor=gsub('KSADS\\.','',result.tmp$factor)
  result.tmp$factor=gsub('\\.p','.caregiver',result.tmp$factor)
  result.tmp$factor=gsub('\\.y','.child',result.tmp$factor)
  result.tmp$factor=gsub('_ever','',result.tmp$factor)
  result.tmp$factor=gsub('_',' ',result.tmp$factor)
  eval(parse(text=paste0(i,'=result.tmp')))
}

# Prepare inputs for reorg_result_web function
ls.factor.vars=unique(result.tmp$factor)
ls.modality=read.table(ls.result.modality,header = T,
                       sep='\t',stringsAsFactors = F)
ls.factor.modality.combo=expand.grid(ls.factor.vars,ls.modality$region,stringsAsFactors = F)

cols.target=c('beta','std','p.value','p.corrected')

tmp.label=read.table(label_replace,header = T,
                sep='\t',stringsAsFactors = F)

# Save as objects (all modalities) 
tmp.obj.ls = transpose(ls.factor.modality.combo)
obj.disply = lapply(tmp.obj.ls,FUN = reorg_result_web,
                    result.table = result.YouthDepre.region.covWholeB,
                    ls.label=tmp.label,
                    colheaders=c('Beta','std','p value','p corr'),
                    cols.select=cols.target)

# Save info for the objects
colnames(ls.factor.modality.combo)=c('factor','region')
ls.factor.modality.combo$obj.no=1:nrow(ls.factor.modality.combo)
ls.obj.info=merge(ls.factor.modality.combo,ls.modality,by='region',all.x=T)

save(obj.disply,ls.obj.info,file=intermediate.result.obj)

# Organise contents for Rmd file ------------------------------------------

table.code=read.delim(template.table.code,quote="",
                      sep='\t', encoding="ASCII", header=FALSE,blank.lines.skip=F)

ls.obj.info=ls.obj.info[order(ls.obj.info$factor,ls.obj.info$obj.no),]

block_code <- function(tmp.obj.info,tmp.modality,code.template){
  target.row=tmp.obj.info[tmp.obj.info$modality==tmp.modality,]
  target.obj.no=target.row$obj.no
  tmp.label=paste0('#### ',target.row$modality.fullname)
  tmp.code = gsub('targetnumber',target.obj.no,code.template)
  return.code = data.frame(block.code=c(tmp.label,tmp.code,''),stringsAsFactors = F)
  return(return.code)
}

for (f in unique(ls.obj.info$factor)){
  
  block.ls.obj.info = ls.obj.info[grep(f,ls.obj.info$factor),]
  block.code = 
    lapply(as.list(block.ls.obj.info$modality),FUN=block_code,
                      tmp.obj.info=block.ls.obj.info,
                      code.template=table.code$V1) %>%
    bind_rows 
  block.code = data.frame(block.code=c(paste0('### ',f,'{.tabset}'),block.code$block.code),
                          stringsAsFactors=F) 
  
  if(f==unique(ls.obj.info$factor)[1]){
    output.code = block.code
  }else{
    output.code = data.frame(block.code=
                               c(output.code$block.code, block.code$block.code),
                             stringsAsFactors=F)
  }
}


# Combine code with Rmd file header ---------------------------------------

header.code=read.delim(rmd.header.file,sep='\t',quote="",
                       encoding="ASCII", header=FALSE,blank.lines.skip=F)

complete.code = data.frame(c(header.code$V1,
                             output.code$block.code),stringsAsFactors = F)

write.table(complete.code,file = rmd.output.dir,
            quote = F,col.names = F,row.names = F)


# Render r markdown -------------------------------------------------------
rmarkdown::render(rmd.output.dir,
                  output_file = final.render.dir,output_format = 'all')
# 
# 
# # Save bulk measures
# ls.vertical.vars=c('MDD.caregiver','Depressive symptoms.caregiver',
#                    'MDD.child','Depressive symptoms.child')
# cols.select=c('beta','std','p.value')
# ls.modality=read.table('result/i.Main_result/result.ls.txt',header = T,
#                        sep='\t',stringsAsFactors = F)
# ls.bulk_label=read.table('data/result_table_data/ls.BulkMeasures.csv',header = T,
#                          sep='\t',stringsAsFactors = F)
# reorg_result_xlsx(result.table = result.YouthDepre.bulk,
#                   ls.vertical.vars,cols.select,
#                   ls.label=ls.bulk_label,
#                   file_name='Table/SuppleMaterials/MainResult/YouthDepre_BulkMeasure.xlsx',
#                   colheaders=c('Beta','std','p value'),
#                   table_title='Model: Bulk imaging measure ~ MDD/Depressive symptoms')
