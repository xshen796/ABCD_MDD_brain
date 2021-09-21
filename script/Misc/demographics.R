# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
setwd('/gpfs/igmmfs01/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD/')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)

# Freesufer and subcortical volumes
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion','iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
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
DTI.dat=targetdata[,grep('^f.eid|dtiFA|dtiMD',colnames(targetdata))]
DTI.dat.ID=DTI.dat$f.eid[rowSums(!is.na(DTI.dat[,2:ncol(DTI.dat)]))>0]

targetdata = targetdata %>% .[.$f.eid %in% c(FS.dat.ID,DTI.dat.ID),]



# Filter participants -----------------------------------------------------

# ls.IM.modal = c('dtiFA','dtiMD','iqc_dmri_fa_qc','thk\\.','sulc\\.','sa\\.','vol\\.','fsqc_qc','fsqc_qu_motion')
# loc = rowSums(!is.na(tab.data[,grepl(paste0(ls.IM.modal,collapse = '|'),colnames(tab.data))]))
# All participants in this dataset have at least one IM measure. 

# A function for demographic variables for a given sample -----------------

collect_demographic <- function(data,divide_cc=F,divide_var,var_of_interest,var_name){
      if (divide_cc==F){
            
            targetvar=data[,var_of_interest]
            if(is.factor(targetvar)){
                  tmp.t=data.frame(Name=var_name,Values=names(table(targetvar)),Count=table(targetvar))
                  tmp.t=tmp.t[,c(1,2,4)]
            }else{
                  tmp.t=data.frame(Name=var_name,Values=c('Mean','Std'),Count=c(mean(targetvar,na.rm = T),sd(targetvar,na.rm=T)))
            }
            colnames(tmp.t)=c('Name','Values','Count')
            
      }else{
            targetvar1=data[data[,divide_var]==1,var_of_interest]
            targetvar0=data[data[,divide_var]==0,var_of_interest]
            if(is.factor(targetvar1)){
                  tmp.t=data.frame(Name=var_name,Values=names(table(targetvar1)),
                                   Count1=table(targetvar1),Count0=table(targetvar0))
                  tmp.t=tmp.t[,c(1,2,4,6)]
            }else{
                  tmp.t=data.frame(Name=var_name,Values=c('Mean','Std'),
                                   Count1=round(c(mean(targetvar1,na.rm = T),sd(targetvar1,na.rm=T)),2),
                                   Count0=round(c(mean(targetvar0,na.rm = T),sd(targetvar0,na.rm=T)),2))
            }   
            colnames(tmp.t)=c('Name','Values','Count1','Count0')
      }
      return(tmp.t)
}


# Prepare data ------------------------------------------------------------

targetdata=filter(targetdata,sex!='')
targetdata$sex = as.factor(targetdata$sex)
targetdata$interview_age = targetdata$interview_age/12

targetdata$fam_highest_edu_category=targetdata$fam_highest_edu
targetdata$fam_highest_edu_category[targetdata$fam_highest_edu_category<=13]='High school degree or less'
targetdata$fam_highest_edu_category[(targetdata$fam_highest_edu_category>13)&targetdata$fam_highest_edu_category<=16]='College'
targetdata$fam_highest_edu_category[targetdata$fam_highest_edu_category==18]='Bachlor\'s'
targetdata$fam_highest_edu_category[targetdata$fam_highest_edu_category==19]='Master\'s'
targetdata$fam_highest_edu_category[targetdata$fam_highest_edu_category==20]='Professional degree'
targetdata$fam_highest_edu_category[targetdata$fam_highest_edu_category==21]='PhD'
targetdata$fam_highest_edu_category = as.factor(targetdata$fam_highest_edu_category)

targetdata$household_income[targetdata$household_income==1]='Less than $5,000'
targetdata$household_income[targetdata$household_income==2]='$5,000 through $11,999'
targetdata$household_income[targetdata$household_income==3]='$12,000 through $15,999'
targetdata$household_income[targetdata$household_income==4]='$16,000 through $24,999'
targetdata$household_income[targetdata$household_income==5]='$25,000 through $34,999'
targetdata$household_income[targetdata$household_income==6]='$35,000 through $49,999'
targetdata$household_income[targetdata$household_income==7]='$50,000 through $74,999'
targetdata$household_income[targetdata$household_income==8]='$75,000 through $99,999'
targetdata$household_income[targetdata$household_income==9]='$100,000 through $199,999'
targetdata$household_income[targetdata$household_income==10]='$200,000 and greater'
targetdata$household_income = factor(targetdata$household_income,levels=
                                           c('Less than $5,000','$5,000 through $11,999',
                                             '$12,000 through $15,999','$16,000 through $24,999',
                                             '$25,000 through $34,999','$35,000 through $49,999',
                                             '$50,000 through $74,999','$75,000 through $99,999',
                                             '$100,000 through $199,999','$200,000 and greater'))

targetdata$recent_socialdprv_tertile=targetdata$recent_socialdprv
targetdata$recent_socialdprv_tertile[targetdata$recent_socialdprv<=4]=1
targetdata$recent_socialdprv_tertile[targetdata$recent_socialdprv>4&targetdata$recent_socialdprv<=9]=2
targetdata$recent_socialdprv_tertile[targetdata$recent_socialdprv>9]=3
targetdata$recent_socialdprv_tertile=as.factor(targetdata$recent_socialdprv_tertile)

targetdata$race_ethnicity[targetdata$race_ethnicity==1]='White'
targetdata$race_ethnicity[targetdata$race_ethnicity==2]='Black'
targetdata$race_ethnicity[targetdata$race_ethnicity==3]='Hispanic'
targetdata$race_ethnicity[targetdata$race_ethnicity==4]='Asian'
targetdata$race_ethnicity[targetdata$race_ethnicity==5]='Other'
targetdata$race_ethnicity= factor(targetdata$race_ethnicity,levels=
                                           c('White','Black','Hispanic','Asian','Other'))

# Total Sample
ls.info = cbind(c('sex','interview_age','race_ethnicity','fam_highest_edu_category','household_income','recent_socialdprv_tertile'),
                c('Sex','Age','Ethnicity','Family Highest EA','Household Income','Recent Social Deprivation'))

for(i in 1:nrow(ls.info)){
      tmp.table=collect_demographic(data=targetdata,divide_cc = F,divide_var=NA,var_of_interest=ls.info[i,1],var_name=ls.info[i,2])
      if(i==1){final.table=tmp.table}else(final.table=rbind(final.table,tmp.table))
}
total_table = final.table

# Children MDD parents reported
for(i in 1:nrow(ls.info)){
      tmp.table=collect_demographic(data=targetdata,divide_cc = T,divide_var='KSADS.MDD.p',var_of_interest=ls.info[i,1],var_name=ls.info[i,2])
      if(i==1){final.table=tmp.table}else(final.table=rbind(final.table,tmp.table))
}

KSADS_MDD_p.table = final.table


# Children MDD self reported
for(i in 1:nrow(ls.info)){
      tmp.table=collect_demographic(data=targetdata,divide_cc = T,divide_var='KSADS.MDD.y',var_of_interest=ls.info[i,1],var_name=ls.info[i,2])
      if(i==1){final.table=tmp.table}else(final.table=rbind(final.table,tmp.table))
}

KSADS_MDD_y.table = final.table






# For main text -----------------------------------------------------------

# At least one core symptoms
sum(targetdata$KSADS.items.Depressed.Mood.Ever.p==1|
      targetdata$KSADS.items.Anhedonia.Ever.p==1,na.rm=T)/
  sum(!is.na(targetdata$KSADS.Depressive_symptoms_ever.p))

sum(targetdata$KSADS.items.Depressed.Mood.Ever.y==1|
      targetdata$KSADS.items.Anhedonia.Ever.y==1,na.rm=T)/
  sum(!is.na(targetdata$KSADS.Depressive_symptoms_ever.y))

options(pillar.sigfig=4)

# Age and sex by Depressive_symptoms_ever.p
targetdata %>%
  group_by(KSADS.Depressive_symptoms_ever.p) %>%
  summarise(mean = mean(interview_age),sd=sd(interview_age), n = n())

targetdata  %>%
  group_by(KSADS.Depressive_symptoms_ever.p,sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(sex=='M')

# Age and sex by Depressive_symptoms_ever.y  
targetdata %>%
  group_by(KSADS.Depressive_symptoms_ever.y) %>%
  summarise(mean = mean(interview_age),sd=sd(interview_age), n = n())
  
targetdata  %>%
  group_by(KSADS.Depressive_symptoms_ever.y,sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(sex=='M')
  
  
  
  
# Random ----------------------------------------------------------------
# Age and sex by MDD.p
targetdata %>%
  group_by(is.na(KSADS.MDD.p)) %>%
  summarise(mean = mean(interview_age),sd=sd(interview_age), n = n())

targetdata  %>%
  group_by(is.na(KSADS.MDD.p),sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(sex=='M')

# Age and sex by MDD.y
targetdata %>%
  group_by(is.na(KSADS.MDD.y)) %>%
  summarise(mean = mean(interview_age),sd=sd(interview_age), n = n())

targetdata  %>%
  group_by(is.na(KSADS.MDD.y),sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(sex=='M')

# Age and sex by KSADS.Depressive_symptoms_ever.p
targetdata %>%
  group_by(is.na(KSADS.Depressive_symptoms_ever.p)) %>%
  summarise(mean = mean(interview_age),sd=sd(interview_age), n = n())

targetdata  %>%
  group_by(is.na(KSADS.Depressive_symptoms_ever.p),sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(sex=='M')

# Age and sex by KSADS.Depressive_symptoms_ever.y
targetdata %>%
  group_by(is.na(KSADS.Depressive_symptoms_ever.y)) %>%
  summarise(mean = mean(interview_age),sd=sd(interview_age), n = n())

targetdata  %>%
  group_by(is.na(KSADS.Depressive_symptoms_ever.y),sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(sex=='M')
