# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
setwd('/gpfs/igmmfs01/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# behavioral data
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income','fam_highest_edu')#,'household_income','ADHD_ever'
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)
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

targetdata = tab.data %>% .[.$src_subject_id %in% FAM.dat$src_subject_id,]
colnames(targetdata)[1]='f.eid'


FS.dat=targetdata[,grep('^f.eid|sa\\.|thk\\.|vol\\.|sulc\\.',colnames(targetdata))]
FS.dat.ID=FS.dat$f.eid[rowSums(!is.na(FS.dat[,2:ncol(FS.dat)]))>0]
sum(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.MDD.p')]),na.rm=T)
sum(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.MDD.y')]),na.rm=T)
sum(rowSums(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.MDD.p','KSADS.MDD.y')]),na.rm=T)>0)
sum(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.Depressive_symptoms_ever.p')]))
sum(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.Depressive_symptoms_ever.y')]))
sum(rowSums(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.Depressive_symptoms_ever.p','KSADS.Depressive_symptoms_ever.y')]),na.rm=T)>0)
sum(rowSums(!is.na(targetdata[targetdata$f.eid %in% FS.dat.ID,c('KSADS.MDD.p','KSADS.MDD.y','KSADS.Depressive_symptoms_ever.p','KSADS.Depressive_symptoms_ever.y')]),na.rm=T)>0)

DTI.dat=targetdata[,grep('^f.eid|dtiFA|dtiMD',colnames(targetdata))]
DTI.dat.ID=DTI.dat$f.eid[rowSums(!is.na(DTI.dat[,2:ncol(DTI.dat)]))>0]
sum(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.MDD.p')]),na.rm=T)
sum(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.MDD.y')]),na.rm=T)
sum(rowSums(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.MDD.p','KSADS.MDD.y')]),na.rm=T)>0)
sum(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.Depressive_symptoms_ever.p')]))
sum(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.Depressive_symptoms_ever.y')]))
sum(rowSums(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.Depressive_symptoms_ever.p','KSADS.Depressive_symptoms_ever.y')]),na.rm=T)>0)
sum(rowSums(!is.na(targetdata[targetdata$f.eid %in% DTI.dat.ID,c('KSADS.MDD.p','KSADS.MDD.y','KSADS.Depressive_symptoms_ever.p','KSADS.Depressive_symptoms_ever.y')]),na.rm=T)>0)


