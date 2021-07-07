# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
setwd('/exports/igmm/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD/')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# Freesufer and subcortical volumes
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion','iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG|ICV',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
tab.data=merge(IM.dat,FAM.dat,by='src_subject_id',all.x=T)
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

# New variable for parents
targetdata$depression_bioParents_noManiaVision=rowSums(targetdata[,c('depression_bioFather_noManiaVision','depression_bioMother_noManiaVision')],na.rm=T)
targetdata$depression_bioParents_noManiaVision[rowSums(is.na(targetdata[,c('depression_bioFather_noManiaVision','depression_bioMother_noManiaVision')]))==2]=NA
targetdata$depression_bioParents_noManiaVision[targetdata$depression_bioParents_noManiaVision>1]=1

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
ls.factor=colnames(targetdata)[grep('^depression_',colnames(targetdata))]
ls.factor=ls.factor[grep('_noManiaVision$',ls.factor)]
ls.factor=ls.factor[!grepl('_family_',ls.factor)]

# combine the two
ls.dep.factor.combo=expand.grid(ls.dep.all,ls.factor,stringsAsFactors = F)

# covs
ls.models=data.frame(dependent=ls.dep.factor.combo$Var1,
                     factor=ls.dep.factor.combo$Var2,
                     covs='',stringsAsFactors = F)

ls.models$covs=paste0(c('interview_age','I(interview_age^2)','sex','fsqc_qu_motion','site_id_l','race_ethnicity','recent_socialdprv'),collapse='+')#'involuntary_unemployment','household_income','ADHD_ever','recent_socialdprv',

ls.models$covs[grep('Parents',ls.models$factor)]=paste0(ls.models$covs[grep('Parents',ls.models$factor)],'+alc_bioFather+alc_bioMother')
ls.models$covs[grep('bioFather',ls.models$factor)]=paste0(ls.models$covs[grep('bioFather',ls.models$factor)],
                                                          '+alc_bioFather+bioFather_remote')#+drug_bioFather
ls.models$covs[grep('bioMother',ls.models$factor)]=paste0(ls.models$covs[grep('bioMother',ls.models$factor)],
                                                          '+alc_bioMother+bioMother_remote')#+drug_bioMother

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
ls.model.bulk$p_batch=1
# individual regions
ls.model.region=ls.models[!grepl('mean|total|wholeb',ls.models$dependent),]
ls.model.region$p_batch=99999
target.model=ls.models
ls.dep.cate=c('thk\\.','sa\\.','vol\\.APARC','sulc\\.','vol\\.ASEG','dtiFA','dtiMD')
ls.factor.cate=unique(target.model$factor)
target.model=ls.model.region
cate.no = 1
for (fac in ls.factor.cate){
      for (dep in ls.dep.cate){
            loc = grepl(dep,target.model$dependent)&grepl(fac,target.model$factor)
            target.model$p_batch[loc]=cate.no
            cate.no=cate.no+1
      }
}
ls.model.region=target.model
# individual region models add brain volume as a covariate
ls.model.region.covWholeB = ls.model.region
ls.model.region.covWholeB$covs[grep('thk\\.|sa\\.|vol\\.APARC|sulc\\.|vol\\.ASEG',ls.model.region.covWholeB$dependent)]=
      paste0(ls.model.region.covWholeB$covs[grep('thk\\.|sa\\.|vol\\.APARC|sulc\\.|vol\\.ASEG',ls.model.region.covWholeB$dependent)],
             '+ICV_ASEG')



# Analysis ----------------------------------------------------------------

ls.site=levels(targetdata$site_id_l)
targetdata=targetdata[!is.na(targetdata$site_id_l),]
targetdata_all=targetdata

for (i in ls.site){
    targetdata=targetdata_all[targetdata_all$site_id_l!=i,]
    result.YouthDepre.bulk=reg_phewasStyle(ls.model.bulk,dat_short=targetdata,dat_long=NA,correctByFactor = F)
    eval(parse(text=paste0('site_res.FamRisk.bulk.',i,'=result.YouthDepre.bulk')))
    rm(result.YouthDepre.bulk)
}

ls.result=ls(pattern = '^site_res')

save(list=ls.result,file='result/x.Supplementary_materials/FamRisk_site_BulkMeasure.RData')