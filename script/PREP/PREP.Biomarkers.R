# by XShen
# 16/5/2019
# Aims: remove parents with other major psychiatric conditions,
#       clean up depression-related phenotypes in parents,
#       remove adolescents with severe substance use conditions,
#       add some key covariates: SES, ADHD condition in children and recent social deprivation.
# Before running this one, PREP.FamHis_depression.R needs to be run first.

library(dplyr)

setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0/FamilialRisk_PGRS_MDD')
tab.dat.dir='/sdata/images/projects/UKBIOBANK/ABCD/release2.0/iii.data'

# merge and save the master file  ----------------------------------------------------------------------

dir.var.ls='data/ls.biomarkers.csv'
ls.var=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.var$Category,'/',ls.var$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      ## follow-up track file have duplicated IDs. Only keep the baseline data. 
      if (length(unique(tmp.file$src_subject_id))<length(tmp.file$src_subject_id)){
            tmp.file=filter(tmp.file,eventname=='baseline_year_1_arm_1')}
      
      if (i==ls.files.tocatch[1]){bio.dat=tmp.file}else{
            if (i!='"Mental_Health/pdem02"'){
                  cols.keep=!grepl('interview_date|interview_age|gender|eventname',colnames(tmp.file))
                  tmp.file=tmp.file[,cols.keep]
                  bio.dat=merge(bio.dat,tmp.file,by='src_subject_id',all=T)
            }else{
                  
            }
      }
}


# grep('\\.x$',colnames(IM.dat))  # check if there is any duplicated colnames
rm(i,tmp.file)

bio.dat=bio.dat[,ls.var$Field.name]


# QC data and simplify ----------------------------------------------------------------------

bio_proc <- 
      function(x,biotype) {
            loc = grep(biotype,colnames(x))
            tmp.dat = x[,loc]
            bio.simp.dat=tmp.dat %>% 
                  select(mean=ends_with('mean'),
                         first_rep=ends_with('rep1'),
                         second_rep=ends_with('rep2'))
            bio.simp.dat$src_subject_id=x$src_subject_id
            
            # clean up rep 1
            tmp.loc.NA = rowSums(tmp.dat[,grep('_rep1_',colnames(tmp.dat))]==1,na.rm=T)
            tmp.loc.NA = tmp.loc.NA>=1
            bio.simp.dat$first_rep[tmp.loc.NA]=NA
            # clean up rep 2
            tmp.loc.NA = rowSums(tmp.dat[,grep('_rep2_',colnames(tmp.dat))]==1,na.rm=T)
            tmp.loc.NA = tmp.loc.NA>=1
            bio.simp.dat$second_rep[tmp.loc.NA]=NA
            # clean up means if abs(rep1-rep2) too big
            bio.simp.dat.complete=bio.simp.dat[complete.cases(bio.simp.dat),]
            bio.simp.dat.complete$gap_too_big_rep1n2=
                  abs(bio.simp.dat.complete$first_rep-bio.simp.dat.complete$second_rep)>=
                  0.8*bio.simp.dat.complete$mean
            bio.simp.dat.tomerge=bio.simp.dat.complete[,c('src_subject_id','gap_too_big_rep1n2')]
            bio.simp.dat=merge(bio.simp.dat,bio.simp.dat.tomerge,by='src_subject_id',all.x=T)
            bio.simp.dat$mean[bio.simp.dat$gap_too_big_rep1n2]=NA
            
            bio.simp.dat=bio.simp.dat$mean
            return(bio.simp.dat)
      } 

bio.dat.clean=data.frame(src_subject_id=bio.dat$src_subject_id,
                         DHEA=bio_proc(bio.dat,'dhea'),
                         Estradiol=bio_proc(bio.dat,'hse'),
                         Testosterone=bio_proc(bio.dat,'ert'))

saveRDS(bio.dat.clean,file='data/Biomarkers/biomarkers.rds')
