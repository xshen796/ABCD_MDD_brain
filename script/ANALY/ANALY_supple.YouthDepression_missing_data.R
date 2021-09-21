# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/ABCD_MDD_brain/')

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


# Set categorical variables -----------------------------------------------
targetdata$race_ethnicity = as.factor(targetdata$race_ethnicity)
targetdata$sex = as.factor(targetdata$sex)
targetdata$fsqc_qu_motion = as.factor(targetdata$fsqc_qu_motion)
targetdata$site_id_l = as.factor(targetdata$site_id_l)
targetdata$bioFather_remote = as.factor(targetdata$bioFather_remote)
targetdata$bioMother_remote = as.factor(targetdata$bioMother_remote)

ls.dep=read.table('data/ls.culture_env_physical_TestName.txt',header=T,sep=',',stringsAsFactors=F)


# Categorise sample by data missingness -----------------------------------

targetdata = targetdata %>% 
  mutate(missing.MDD=ifelse(!is.na(KSADS.MDD.p)|!is.na(KSADS.MDD.y),1,0),
         missing.DS=ifelse(!is.na(KSADS.Depressive_symptoms_ever.p)|
                             !is.na(KSADS.Depressive_symptoms_ever.y),1,0))


# Define function ---------------------------------------------------------

source('FUNs/reg_phewasStyle_withCI.R')

sum_demographic <- function(x,target.def){
  info.n = x %>% 
    group_by(get(target.def)) %>% 
    count %>% as.data.frame %>% select(n) 
  
  info.sex = x %>% 
    group_by(get(target.def)) %>% 
    count(sex) %>% filter(sex=='M') %>% 
    as.data.frame %>% select(Male.n=n) %>% 
    mutate(Male.percentage = round(Male.n/info.n$n*100,1)) %>% 
    select(Male.percentage)
  
  info.age = x %>% 
    group_by(get(target.def)) %>% 
    summarise(mean(interview_age, trim=.2,na.rm=T)) %>% 
    .[,2]/12
  info.sd = x %>% 
    mutate(age=interview_age/12) %>% 
    group_by(get(target.def)) %>% 
    summarise(sd(age, na.rm=T)) 
  
  # info.socio = x %>% 
  #   group_by(get(target.def)) %>%
  #   summarise_at(vars(meim_p_ss_total,via_p_ss_hc,via_p_ss_amer,
  #                     nsc_p_ss_mean_3_items,fes_p_ss_fc_pr,psb_p_ss_mean,
  #                     pmq_y_ss_mean,fes_y_ss_fc_pr,psb_y_ss_mean,
  #                     crpbi_y_ss_parent,crpbi_y_ss_caregiver,srpf_y_ss_ses,
  #                     srpf_y_ss_iiss,srpf_y_ss_dfs,sds_p_ss_total,pds_p_ss_category), 
  #                list(~ mean(., trim = .2,na.rm=T))) %>% 
  #   select(-`get(target.def)`)
  
  info.return = cbind(info.n,info.sex,info.age,info.sd)
  return(info.return)
}


# Present missing data ----------------------------------------------------

info.missing = c('missing.MDD','missing.DS') %>% 
  as.list %>% 
  lapply(.,sum_demographic,x=targetdata)

