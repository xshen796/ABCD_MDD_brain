# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
library('stringr')
setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# behavioral data
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income','fam_highest_edu')#,'household_income','ADHD_ever'
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)
# culture, env and physical data
behav.dat=readRDS('data/culture_env_physical.rds')
behav.dat=behav.dat[,!grepl('interview_date|interview_age|gender',colnames(behav.dat))]
tab.data=merge(tab.data,behav.dat,by='src_subject_id',all.x=T)
# Freesufer and subcortical volumes
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion','iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG|ICV',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)
# white matter FA and MD
IM.dat=readRDS('data/IMdat/DTI.dat.QCed_short.rds')
ls.covs=c('iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('dtiFA|dtiMD',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
IM.dat=filter(IM.dat,!is.na(bl.total.dtiFA.FiberAtlas.fibers))
IM.dat[abs(scale(IM.dat$bl.total.dtiFA.FiberAtlas.fibers))>5,grep('dtiFA',colnames(IM.dat))]=NA
IM.dat[abs(scale(IM.dat$bl.total.dtiMD.FiberAtlas.fibers))>5,grep('dtiMD',colnames(IM.dat))]=NA

tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)

targetdata = tab.data
colnames(targetdata)[1]='f.eid'


# Item-wide disprepancy data --------------------------------------------------------------------------------------

discre_dat = targetdata %>% select(contains('KSADS.items'))
discre_dat.p = discre_dat %>%  select(ends_with('.p'))
discre_dat.y = discre_dat %>%  select(ends_with('.y'))

discre_dat.tmp = discre_dat.p-discre_dat.y 
colnames(discre_dat.tmp) = colnames(discre_dat.tmp) %>%
      gsub('\\.p','\\.p_y',.)
discre_dat = data.frame(discre_dat,discre_dat.tmp)

summarise_discre <- function(tmp.dat,n.col){
      
      target.col = tmp.dat[,n.col]
      
      output = data.frame(PhenoName = n.col,
                          proportion.diff = sum(target.col!=0,na.rm=T)/sum(!is.na(target.col)),
                          proportion.P_over_Y = sum(target.col==1,na.rm=T)/sum(!is.na(target.col)),
                          proportion.Y_over_P = sum(target.col==-1,na.rm=T)/sum(!is.na(target.col)),
                          proportion.nodiff = sum(target.col==0,na.rm=T)/sum(!is.na(target.col)),
                          N.total = sum(!is.na(target.col)))
      return(output)
}

summ.ItemDiscre = colnames(discre_dat.tmp) %>%
      as.list %>%
      lapply(.,FUN = summarise_discre,tmp.dat=discre_dat.tmp) %>%
      bind_rows
      
save(summ.ItemDiscre,file='result/summ_ItemDiscre.RData')
