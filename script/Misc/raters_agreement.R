# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
library('irr')
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

targetdata = tab.data
colnames(targetdata)[1]='f.eid'


# Proportion in agreement for MDD diagnosis ---------------------------------
fit=agree(targetdata[,c('KSADS.MDD.p','KSADS.MDD.y')])
N.mdd=sum(rowSums(!is.na(cbind(targetdata$KSADS.MDD.p,targetdata$KSADS.MDD.y)))==2)
tmp.MDD=cbind(targetdata$KSADS.MDD.p,targetdata$KSADS.MDD.y)
tmp.MDD=tmp.MDD[complete.cases(tmp.MDD),]
count.MDD=rbind(table(tmp.MDD[tmp.MDD[,1]==0,2]),table(tmp.MDD[tmp.MDD[,1]==1,2]))
colnames(count.MDD)=c('child.0','child.1')
rownames(count.MDD)=c('parent.0','parent.1')

#         child.0 child.1
#parent.0    8273     168
#parent.1     182      12


# Correlation for DS --------------------------------------------------------
cor.test(targetdata$KSADS.Depressive_symptoms_ever.p,targetdata$KSADS.Depressive_symptoms_ever.y)
N.ds= sum(rowSums(!is.na(cbind(targetdata$KSADS.Depressive_symptoms_ever.p,targetdata$KSADS.Depressive_symptoms_ever.y)

# Three digrees of severity --------------------------------------------------------

tab.data$KSADS.MDD.3tier = rowSums(tab.data[,c('KSADS.MDD.p','KSADS.MDD.y')])
tab.data$KSADS.MDD.3tier=as.factor(tab.data$KSADS.MDD.3tier)


fit=glm(scale(KSADS.Depressive_symptoms_ever.y) ~ interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+recent_socialdprv+KSADS.MDD.3tier, data = filter(tab.data,KSADS.MDD.3tier!=0))
summary(fit)

fit=glm(scale(KSADS.Depressive_symptoms_ever.p) ~ interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+recent_socialdprv+KSADS.MDD.3tier, data = filter(tab.data,KSADS.MDD.3tier!=0))
summary(fit)

fit=glm(scale(KSADS.Depressive_symptoms_ever.p) ~ interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+recent_socialdprv+KSADS.MDD.3tier, data = filter(tab.data,KSADS.MDD.3tier!=1))
summary(fit)

fit=glm(scale(KSADS.Depressive_symptoms_ever.p) ~ interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+recent_socialdprv+KSADS.MDD.3tier, data = filter(tab.data,KSADS.MDD.3tier!=2,(KSADS.MDD.p-KSADS.MDD.y)!=1))
summary(fit)

mean(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==0],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==0],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==0])

mean(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==1],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==1],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==1])

mean(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==2],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==2],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.p[tab.data$KSADS.MDD.3tier==2])


mean(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==0],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==0],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==0])

mean(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==1],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==1],na.rm=T)

mean(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==2],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==2],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.y[tab.data$KSADS.MDD.3tier==2])

mean(tab.data$KSADS.Depressive_symptoms_ever.p[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==1],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.p[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==1],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.p[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==1])

mean(tab.data$KSADS.Depressive_symptoms_ever.y[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==1],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.y[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==1],na.rm=T)
summary(tab.data$KSADS.Depressive_symptoms_ever.y[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==1])


mean(tab.data$KSADS.Depressive_symptoms_ever.p[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==-1],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.p[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==-1],na.rm=T)

mean(tab.data$KSADS.Depressive_symptoms_ever.y[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==-1],na.rm=T)
sd(tab.data$KSADS.Depressive_symptoms_ever.y[(tab.data$KSADS.MDD.p-tab.data$KSADS.MDD.y)==-1],na.rm=T)






window.slide=data.frame(start.p=0:15,end.p=0:15+6)
targetdata=tab.data

for (i in 1:nrow(window.slide)){
    tmp.dat=filter(targetdata,
              KSADS.Depressive_symptoms_ever.y>=window.slide$start.p[i],
              KSADS.Depressive_symptoms_ever.y<=window.slide$end.p[i])
    #fit=cor.test(tmp.dat$KSADS.Depressive_symptoms_ever.p,tmp.dat$KSADS.Depressive_symptoms_ever.y,na.action=na.omit)
    #tmp.res=c(fit$estimate,fit$parameter,fit$conf.int,fit$p.value)
    tmp.res=c(mean(tmp.dat$KSADS.Depressive_symptoms_ever.p-tmp.dat$KSADS.Depressive_symptoms_ever.y,na.rm=T),
                sd(tmp.dat$KSADS.Depressive_symptoms_ever.p-tmp.dat$KSADS.Depressive_symptoms_ever.y,na.rm=T))
    if(i==1){res.all=tmp.res}else{res.all=rbind(res.all,tmp.res)}
}



### Count present/past  ----------------------------------------------------------------------------------


library(dplyr)

setwd('/gpfs/igmmfs01/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/data/abcd/release2.0.1/iii.data'

# merge and save the master file

dir.var.ls='data/ls.questionnaires.csv'
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

targetdat=questn.dat
targetdat.clean=targetdat

# MDD diagnosis 

# Adolescents
sum_diagnosis <- function(x,diagnosis,dic) {
      tmp.dic = dic[grep(diagnosis,dic$Description),]
      tmp.field = tmp.dic$Field.name
      dat = x[,colnames(x) %in% tmp.field]
      tmp.output = rowSums(dat==1,na.rm=T)
      tmp.output[tmp.output>0]=1
      tmp.output[rowSums(!is.na(dat))==0]=NA

      return(tmp.output)
} 
mdd.diagnosis.dic = ls.var[ls.var$File.name %in% 'abcd_ksad501',] 
diagnosis.dat.tomerge=
      data.frame(src_subject_id=targetdat.clean$src_subject_id,
                 KSADS.MDD.y=sum_diagnosis(targetdat.clean,'Major Depressive Disorder',mdd.diagnosis.dic))
targetdat.clean=merge(targetdat.clean,diagnosis.dat.tomerge)
