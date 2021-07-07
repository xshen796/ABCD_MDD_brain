# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0.1//FamilialRisk_PGRS_MDD')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# behavioral data
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income')#,'household_income','ADHD_ever'
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
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiFA.FiberAtlas.fibers))<5)
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiMD.FiberAtlas.fibers))<5)

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


# Generate long-format data -----------------------------------------------
dat_colnames = colnames(targetdata)[grep('lh\\.|rh\\.',colnames(targetdata))]  # colnames of imaging data
cols_nonimg = colnames(targetdata)[!grepl('lh\\.|rh\\.',colnames(targetdata))]  # colnames of non-imaging data/unilateral img data
cols_nonimg = cols_nonimg[2:length(cols_nonimg)]
source('FUNs/long_format_new.R')
targetdata_longformat <- long_format(targetdata,cols_nonimg,cols_img)


# Define functions --------------------------------------------------------
source('FUNs/reg_phewasStyle.R')

# Define global vars ------------------------------------------------------

targetdata=targetdata
dat_long=targetdata_longformat

# dependent variables
ls.dep.fs.long=colnames(dat_long)[grep('sa\\.|thk\\.|vol\\.|sulc\\.|dtiFA\\.|dtiMD\\.|\\.ASEG\\.',colnames(dat_long))]
ls.dep.fs.short=colnames(targetdata)[grep('^bl\\.',colnames(targetdata))]
ls.dep.all=c(ls.dep.fs.long,ls.dep.fs.short)

ls.dep.all=ls.dep.all[c(grep('sa\\.|thk\\.|vol\\.|sulc\\.',ls.dep.all),
                        grep('\\.ASEG\\.',ls.dep.all),
                        grep('dtiFA\\.',ls.dep.all),
                        grep('dtiMD\\.',ls.dep.all))]
ls.dep.all=ls.dep.all[!duplicated(ls.dep.all)]

# factors
ls.factor=colnames(targetdata)[grep('^CBCL\\.|^KSADS\\.',colnames(targetdata))]
ls.factor=ls.factor[grepl('items',ls.factor)]
ls.factor=ls.factor[grepl('Present|current',ls.factor)]
ls.factor=c(ls.factor[grep('.p$',ls.factor)],ls.factor[grep('.y$',ls.factor)])

# combine the two
ls.dep.factor.combo=expand.grid(ls.dep.all,ls.factor,stringsAsFactors = F)

# covs
ls.models=data.frame(dependent=ls.dep.factor.combo$Var1,
                     factor=ls.dep.factor.combo$Var2,
                     covs='',stringsAsFactors = F)

ls.models$covs=paste0(c('interview_age','I(interview_age^2)','sex','fsqc_qu_motion','site_id_l','race_ethnicity'),collapse='+')#'involuntary_unemployment','household_income','ADHD_ever','recent_socialdprv',

ls.models$covs[!grepl('bl\\.',ls.models$dependent)]=paste0(ls.models$covs[!grepl('bl\\.',ls.models$dependent)],'+hemi')

# ls.models$covs[!grepl('^total.vol.APARC$',ls.models$dependent)]=
#       paste0(ls.models$covs[!grepl('^total.vol.APARC$',ls.models$dependent)],'+total.vol.APARC')

# specify models
ls.models$model.est=''
ls.models$model.est[grep('hemi',ls.models$covs)]='lme'
ls.models$model.est[ls.models$model.est=='']='glm'


# Divide models into bulk measures and individual regions -----------------

# bulk measures
ls.model.bulk=ls.models[grep('mean|total|wholeb',ls.models$dependent),]
ls.model.bulk$p_batch=as.numeric(as.factor(ls.model.bulk$factor))

# Analysis ----------------------------------------------------------------

result.YouthDepreItems.bulk=reg_phewasStyle(ls.model.bulk,dat_short=targetdata,dat_long=dat_long,correctByFactor = T)

save(result.YouthDepreItems.bulk,file='result/x.Supplementary_materials/YouthDepreeItems_WM_subcor_FS.RData')
