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

ls.dep=read.table('data/ls.culture_env_physical_TestName.txt',header=T,sep=',',stringsAsFactors=F)

# Set categorical variables -----------------------------------------------
targetdata$race_ethnicity = as.factor(targetdata$race_ethnicity)
targetdata$sex = as.factor(targetdata$sex)
targetdata$fsqc_qu_motion = as.factor(targetdata$fsqc_qu_motion)
targetdata$site_id_l = as.factor(targetdata$site_id_l)
targetdata$bioFather_remote = as.factor(targetdata$bioFather_remote)
targetdata$bioMother_remote = as.factor(targetdata$bioMother_remote)
targetdata$scanner_manufacturer_pd[is.na(targetdata$scanner_manufacturer_pd)]='Other'
targetdata$scanner_manufacturer_pd=as.factor(targetdata$scanner_manufacturer_pd)

# Add parent vs child difference ------------------------------------------
fit=glm(KSADS.Depressive_symptoms_ever.p~KSADS.Depressive_symptoms_ever.y,data=targetdata)
diff.dat=data.frame(resid.KSADS.Depressive_symptoms_ever.p_to_y=resid(fit),
f.eid=targetdata$f.eid[complete.cases(targetdata[,c('KSADS.Depressive_symptoms_ever.y','KSADS.Depressive_symptoms_ever.p')])])
targetdata=merge(targetdata,diff.dat,by='f.eid',all.x=T)
targetdata$KSADS.Depressive_symptoms_ever.p_minus_y=abs(targetdata$KSADS.Depressive_symptoms_ever.p - targetdata$KSADS.Depressive_symptoms_ever.y)
targetdata$KSADS.Depressive_symptoms_ever.p_more=targetdata$KSADS.Depressive_symptoms_ever.p - targetdata$KSADS.Depressive_symptoms_ever.y
targetdata$KSADS.Depressive_symptoms_ever.p_more[targetdata$KSADS.Depressive_symptoms_ever.p_more<0]=NA
targetdata$KSADS.Depressive_symptoms_ever.p_less=targetdata$KSADS.Depressive_symptoms_ever.y - targetdata$KSADS.Depressive_symptoms_ever.p
targetdata$KSADS.Depressive_symptoms_ever.p_less[targetdata$KSADS.Depressive_symptoms_ever.p_less<0]=NA

# Generate long-format data -----------------------------------------------
dat_colnames = colnames(targetdata)[grep('lh\\.|rh\\.',colnames(targetdata))]  # colnames of imaging data
cols_nonimg = colnames(targetdata)[!grepl('lh\\.|rh\\.',colnames(targetdata))]  # colnames of non-imaging data/unilateral img data
cols_nonimg = cols_nonimg[2:length(cols_nonimg)]
source('FUNs/long_format_new.R')
targetdata_longformat <- long_format(targetdata,cols_nonimg,cols_img)


# Define functions --------------------------------------------------------
source('FUNs/reg_phewasStyle_withCI.R')

# Define global vars ------------------------------------------------------

targetdata=targetdata
dat_long=targetdata_longformat

# dependent variables


ls.dep.all=ls.dep$short_name

# factors
ls.factor='KSADS.Depressive_symptoms_ever.p_minus_y'
# ls.factor=c('KSADS.Depressive_symptoms_ever.p_minus_y',
#             'KSADS.Depressive_symptoms_ever.p_more',
#             'KSADS.Depressive_symptoms_ever.p_less')

# combine the two
ls.dep.factor.combo=expand.grid(ls.dep.all,ls.factor,stringsAsFactors = F)

# covs
ls.models=data.frame(dependent=ls.dep.factor.combo$Var1,
                     factor=ls.dep.factor.combo$Var2,
                     covs='',stringsAsFactors = F)

ls.models$covs=paste0(c('interview_age','I(interview_age^2)','sex','fsqc_qu_motion','site_id_l','race_ethnicity','recent_socialdprv'),collapse='+')#,,'household_income','fam_highest_edu'



# ls.models$covs[!grepl('^total.vol.APARC$',ls.models$dependent)]=
#       paste0(ls.models$covs[!grepl('^total.vol.APARC$',ls.models$dependent)],'+total.vol.APARC')

# specify models
ls.models$model.est=''
ls.models$model.est[grep('hemi',ls.models$covs)]='lme'
ls.models$model.est[ls.models$model.est=='']='glm'


# Divide models into bulk measures and individual regions -----------------


ls.models$p_batch=99999
target.model=ls.models
ls.dep.cate=c('thk\\.','sa\\.','vol\\.APARC','sulc\\.','vol\\.ASEG','dtiFA','dtiMD')
ls.factor.cate=unique(target.model$factor)
cate.no = 1
for (fac in ls.factor.cate){
      for (dep in ls.dep.cate){
            loc = grepl(dep,target.model$dependent)&grepl(fac,target.model$factor)
            target.model$p_batch[loc]=cate.no
            cate.no=cate.no+1
      }
}


# Analysis ----------------------------------------------------------------

result=reg_phewasStyle(ls.models,dat_short=targetdata,dat_long=dat_long,correctByFactor = T)
result$p.corrected=p.adjust(result$p.value,method='bonferroni')

rownames(ls.dep)=ls.dep$short_name
result=cbind(result,ls.dep[result$dependent,c('description','category')])

save(result,file='result/i.Main_result/Diffs_YouthDepression_envVariable.RData')

