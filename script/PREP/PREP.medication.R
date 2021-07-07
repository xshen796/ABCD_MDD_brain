######
# by Shen
library(dplyr)
library(base)

setwd('/exports/igmm/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/shen/SData/ABCD/release2.0.1/iii.data'

## Load medication data. Note: multiple instances in this data
medi.dat=readRDS(paste0(tab.dat.dir,'/Physical_Health/medsy01.rds'))
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')

## Check which instance should use to match with imaging assessment
medi.dat$interview_date=as.Date(medi.dat$interview_date, "%m/%d/%Y")
medi.dat=medi.dat[order(medi.dat$interview_date),]

IM.dat$interview_date=as.Date(IM.dat$interview_date, "%m/%d/%Y")
IM.dat.tomerge=IM.dat[,c('src_subject_id','interview_date','interview_age')]
IM.dat.tomerge$IM_yes=1

medi.IM.dat=merge(medi.dat,IM.dat.tomerge,by=c('src_subject_id','interview_age'),all.x=T)
medi.IM.dat=filter(medi.IM.dat,IM_yes==1)

## Extract medication names and number of medications identified
medi.name=medi.IM.dat[,grep('src_subject_id|brought_medications|_rxnorm_p',colnames(medi.dat))]
medi.prescribed=medi.name[,grep('_rxnorm_p',colnames(medi.name))]
medi.prescribed=medi.prescribed[,!grepl('_otc_',colnames(medi.prescribed))]
medi.name$N.prescribed=rowSums(!is.na(medi.prescribed))
medi.name$N.prescribed=medi.name$N.prescribed-rowSums(medi.prescribed=='',na.rm=T)



###### Variable 1: medication vs no medication (medication_status) -----------------------------------------
# columns used: *_rxnorm_p(medication name) and brought_medications (Did your child take any medications in the past two weeks and if so did you bring them with you? 0 = Yes; 1 = Yes; 2 = Refused - Record reason for refusal in Comments Section Se negó - anote el motivo por el cual se negó en la sección de Comentarios.; 3 = Took No Medications)
# For this created variable:
# 0: brought_medications==3 & no medication name reported
# 1: brought_medications==1/0 & no medication name reported
# 2: brought_medications==1/0 & at least one medication name reported & none was included in the antidepressant list
# 3: brought_medications==1/0 & at least one antidepressant reported

medi.name$medication_status_all=99999
medi.name$medication_status_all[(medi.name$brought_medications==3)&(medi.name$N.prescribed)==0]=0
medi.name$medication_status_all[(medi.name$brought_medications<=1)&(medi.name$N.prescribed)==0]=1
medi.name$medication_status_all[(medi.name$brought_medications<=1)&(medi.name$N.prescribed)>0]=2
medi.name$medication_status_all[is.na(medi.name$brought_medications)|is.na(medi.name$N.prescribed)]=NA
medi.name$medication_status_all[medi.name$medication_status_all==99999]=NA

# Find antidepressant
# columns used: *_rxnorm_p(medication name) and brought_medications
# Additional info: a list of antidepressant from the British Medical Booklet: supple info in https://doi.org/10.1016/j.biopsych.2019.06.011
# For this created variable:
# 0: brought_medications!=NA & no antidepressant reported (no reported antidepressant - not due to fail to report)
# 1: brought_medications==1/0 & at least one antidepressant reported


# All reported medications
medi.name.all=as.vector(medi.prescribed)
medi.name.all=medi.name.all[!is.na(medi.name.all)]
medi.name.all=medi.name.all[medi.name.all!='']
medi.name.all=unique(medi.name.all)
medi.name.all=tolower(medi.name.all)

# Load antidepressant list
ls.antidepres=read.table('data/ls.antidepressant',header=F,stringsAsFactors=F)
ls.antidepres=ls.antidepres$V1
ls.anti.ABCD=medi.name.all[grep(paste0(ls.antidepres,collapse='|'),medi.name.all)]

sprintf('Proportion of antidepressants reported: %f',length(ls.anti.ABCD)/length(medi.name.all))

# Create a function to detect antidepressant

detec_antidepre <- function(dat,ls.tofind){
    k=sum(tolower(dat) %in% ls.tofind,na.rm=T)
    if(k==0){output=0}else{output=1}
    return(output)
}

anti.count=apply(X=medi.prescribed,MARGIN=1,FUN=detec_antidepre,ls.tofind=ls.anti.ABCD)
medi.name$medication_status=medi.name$medication_status+anti.count

medi.name.tosave=medi.name[,c('src_subject_id','medication_status')]
medi.name.tosave$medication_status=as.factor(medi.name.tosave$medication_status)
saveRDS(medi.name.tosave,file='data/Behavioural/medication_status.rds')