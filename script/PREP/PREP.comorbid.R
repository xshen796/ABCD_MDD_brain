
library(dplyr)

setwd('/gpfs/igmmfs01/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/data/abcd/release2.0.1/iii.data'

# merge and save the master file  ----------------------------------------------------------------------

dir.var.ls='data/ls.comorbid.csv'
ls.var=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.var$Category,'/',ls.var$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      ## follow-up track file have duplicated IDs. Only keep the baseline data. 
      if (length(unique(tmp.file$src_subject_id))<length(tmp.file$src_subject_id)){
            tmp.file = tmp.file[order(tmp.file$interview_date),]
            tmp.file = tmp.file[!duplicated(tmp.file$src_subject_id),]}
      
      if (i==ls.files.tocatch[1]){questn.dat=tmp.file}else{
            cols.keep=!grepl('interview_date|interview_age|sex|gender|eventname',colnames(tmp.file))
            tmp.file=tmp.file[,cols.keep]
            questn.dat=merge(questn.dat,tmp.file,by='src_subject_id',all=T)                  
      }
}


# grep('\\.x$',colnames(IM.dat))  # check if there is any duplicated colnames
rm(i,tmp.file)

questn.dat=questn.dat[,ls.var$Field.name]



# Any comorbid based on KSADS -------------------------------------------------------------------------------------

target.dat = questn.dat

ksads.diagnosis = target.dat %>%
      select(starts_with('ksads')) %>%
      rowSums(.,na.rm=T) 

ksads.comorb.dat = target.dat %>%
      mutate(any_comorb.ksads=if_else(ksads.diagnosis>=1,1,0)) %>%
      select(src_subject_id,any_comorb.ksads)

saveRDS(ksads.comorb.dat,file='data/Behavioural/comorbid.rds')
