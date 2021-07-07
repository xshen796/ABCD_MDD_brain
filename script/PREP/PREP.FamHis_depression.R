######
# by Shen

library(dplyr)

setwd('/exports/igmm/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD/')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/shen/SData/ABCD/release2.0.1/iii.data'

# merge, QC and save the master file (FAM dat) ----------------------------------------------------------------------

dir.var.ls='data/ls.FAM_demographic.csv'
ls.var=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.var$Category,'/',ls.var$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      ## follow-up track file have duplicated IDs. Only keep the baseline data. 
      if (length(unique(tmp.file$src_subject_id))<length(tmp.file$src_subject_id)){
            tmp.file=filter(tmp.file,eventname=='baseline_year_1_arm_1')}
      
      if (i==ls.files.tocatch[1]){FAM.dat=tmp.file}else{
            if (i!='"Mental_Health/pdem02"'){
            cols.keep=!grepl('interview_date|interview_age|gender|sex|eventname',colnames(tmp.file))
            tmp.file=tmp.file[,cols.keep]
            FAM.dat=merge(FAM.dat,tmp.file,by='src_subject_id',all=T)
            }else{
                  
            }
      }
}


# grep('\\.x$',colnames(IM.dat))  # check if there is any duplicated colnames
rm(i,tmp.file)

FAM.dat=FAM.dat[,ls.var$Field.name]

### all subs
saveRDS(FAM.dat,file='data/FamHistory/FAM.dat_AllSubjects.rds')
### unrelated subs
FAM.dat.unrelated=FAM.dat[!duplicated(FAM.dat$rel_family_id),]
saveRDS(FAM.dat.unrelated,file='data/FamHistory/FAM.dat_Unrelated.rds')
### twin pairs
onetwin.ID = !FAM.dat$src_subject_id %in% FAM.dat.unrelated$src_subject_id
onetwin.fam.ID = FAM.dat$rel_family_id[onetwin.ID]
loc.withonetwin = FAM.dat$rel_family_id %in% onetwin.fam.ID
FAM.dat.twins=FAM.dat[loc.withonetwin,]
saveRDS(FAM.dat.twins,file='data/FamHistory/FAM.dat_TwTri.rds')
