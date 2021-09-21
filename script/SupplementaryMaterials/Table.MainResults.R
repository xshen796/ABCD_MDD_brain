
# Basic settings ----------------------------------------------------------

setwd("Z:/Documents/ActiveProject/ImagingProject/Adolescent_MDD_brain")
source('FUNs/reorg_result_xlsx.R')
source('FUNs/wholeB_correction.R')

# Tables for Main results (depressive symptoms and MDD) -------------------
rm(list = ls(pattern = '^result.'))
load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')
TargetResult = result.YouthDepre.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.YouthDepre.region.covWholeB=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)

rm(TargetResult)

for (i in ls(pattern = '^result.')){
  result.tmp=get(i)
  result.tmp$factor=gsub('KSADS\\.','',result.tmp$factor)
  result.tmp$factor=gsub('\\.p','.caregiver',result.tmp$factor)
  result.tmp$factor=gsub('\\.y','.child',result.tmp$factor)
  result.tmp$factor=gsub('_ever','',result.tmp$factor)
  result.tmp$factor=gsub('_',' ',result.tmp$factor)
  eval(parse(text=paste0(i,'=result.tmp')))
}

ls.vertical.vars=c('MDD.caregiver','Depressive symptoms.caregiver',
                   'MDD.child','Depressive symptoms.child')
cols.select=c('beta','std','Lower_95CI','Upper_95CI','p.value','p.corrected')
ls.modality=read.table('result/i.Main_result/result.ls.txt',header = T,
                       sep='\t',stringsAsFactors = F)
ls.FS_label=read.table('data/result_table_data/ls.Freesurfer_region_replacement_labels.csv',header = T,
                       sep='\t',stringsAsFactors = F)
ls.DTI_label=read.table('data/result_table_data/ls.DTI_region_replacement_labels.csv',header = T,
                       sep='\t',stringsAsFactors = F)
ls.label=rbind(ls.FS_label,ls.DTI_label)

# Save vertical results as objects (all modalities) 

for (i in 1:nrow(ls.modality)){

  dep.kw=ls.modality$region[i]
  file_name_short=paste0('YouthDepre_',ls.modality$modality[i])
  file_name_full=paste0('Table/SuppleMaterials/MainResult/',file_name_short,'.xlsx')
  table_title=paste0('Model: ',ls.modality$modality.fullname[i],' ~ MDD/Depressive symptoms')
  tmp.label=ls.label
  tmp.label$Field.name=paste0(dep.kw,'.',tmp.label$Field.name)
  region.vertical.res=
    reorg_result_xlsx(result.table = result.YouthDepre.region.covWholeB,
                      ls.vertical.vars,cols.select,dep.kw,
                      ls.label=tmp.label,
                      file_name=file_name_full,
                      colheaders=c('Beta','std','95% CI (lower)','95% CI (upper)','p value','p corr'),
                      table_title=table_title)
}


# Save bulk measures
ls.vertical.vars=c('MDD.caregiver','Depressive symptoms.caregiver',
                   'MDD.child','Depressive symptoms.child')
cols.select=c('beta','std','Lower_95CI','Upper_95CI','p.value')
ls.modality=read.table('result/i.Main_result/result.ls.txt',header = T,
                       sep='\t',stringsAsFactors = F)
ls.bulk_label=read.table('data/result_table_data/ls.BulkMeasures.csv',header = T,
                       sep='\t',stringsAsFactors = F)
reorg_result_xlsx(result.table = result.YouthDepre.bulk,
                  ls.vertical.vars,cols.select,
                  ls.label=ls.bulk_label,
                  file_name='Table/SuppleMaterials/MainResult/YouthDepre_BulkMeasure.xlsx',
                  colheaders=c('Beta','std','95% CI (lower)','95% CI (upper)','p value'),
                  table_title='Model: Bulk imaging measure ~ MDD/Depressive symptoms')



# Label and move result files to publication format -----------------------
#ls.supple_dat=list.files('Table/SuppleMaterials/MainResult')
# write.table(ls.supple_dat,sep='\n',file = 'data/result_table_data/MainResult_fileList.txt',
#             quote = F,row.names = F,col.names = F)
ls.f.transfer=read.table('data/result_table_data/MainResult_fileList.txt',sep='\t',header=F)
for (i in 1:nrow(ls.f.transfer)){
    old.f=ls.f.transfer[i,1]
    old.f=paste0('Table/SuppleMaterials/MainResult/',old.f)
    new.f=ls.f.transfer[i,2]
    new.f=paste0('PaperWriting/SupplementaryData/',new.f)
    file.copy(from = old.f,to = new.f)
}
