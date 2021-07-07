
# Basic setups ------------------------------------------------------------

library(dplyr)
setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')


# Load data ---------------------------------------------------------------

FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1:4,6:ncol(FAM.dat))]
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income')#,'household_income','ADHD_ever'
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)
locs.covs=grep(paste0(ls.covs,collapse='|'),colnames(tab.data))


# Define functions --------------------------------------------------------

source('FUNs/reg_phewasStyle.R')
source('FUNs/add_p_batch.R')

library('pbapply')
library('nlme')

targetdata=tab.data

# Defs and total scores ---------------------------------------------------

# dependent variables
ls.dep.behav=colnames(targetdata)[grep('^CBCL\\.',colnames(targetdata))]
ls.dep.all=ls.dep.behav
ls.dep.all=c(ls.dep.all[grep('.p$',ls.dep.all)],ls.dep.all[grep('.y$',ls.dep.all)])


# factors
ls.factor=colnames(targetdata)[grep('^depression',colnames(targetdata))]
ls.factor=ls.factor[grep('_noManiaVision$',ls.factor)]
ls.factor=ls.factor[!grepl('_family_',ls.factor)]

# combine the two
ls.dep.factor.combo=expand.grid(ls.dep.all,ls.factor,stringsAsFactors = F)

# covs
ls.models=data.frame(dependent=ls.dep.factor.combo$Var1,
                     factor=ls.dep.factor.combo$Var2,
                     covs='',stringsAsFactors = F)
ls.models$covs=paste0(c('interview_age','I(interview_age^2)','gender','race_ethnicity','site_id_l'),
                        collapse='+') #'involuntary_unemployment','recent_socialdprv','cbcl_scr_dsm5_anxdisord_r','cbcl_scr_dsm5_adhd_r','household_income','ADHD_ever',
                      

ls.models$covs[grep('family',ls.models$factor)]=paste0(ls.models$covs[grep('family',ls.models$factor)],'+alc_family+drug_family')
ls.models$covs[grep('bioFather',ls.models$factor)]=paste0(ls.models$covs[grep('bioFather',ls.models$factor)],
                                                          '+alc_bioFather+drug_bioFather+bioFather_remote')
ls.models$covs[grep('bioMother',ls.models$factor)]=paste0(ls.models$covs[grep('bioMother',ls.models$factor)],
                                                          '+alc_bioMother+drug_bioMother+bioMother_remote')

# model.est
ls.models$model.est=''
ls.models$model.est[grep('hemi',ls.models$covs)]='lme'
ls.models$model.est[ls.models$model.est=='']='glm'


## Divide into bulk measures and items

ls.models.bulk=ls.models[!grepl('items',ls.models$dependent),]
ls.models.item=ls.models[grepl('items',ls.models$dependent),]


ls.dep=c('\\.p','\\.y')

ls.factor=unique(ls.models.bulk$factor)
ls.models.bulk=add_p_batch(ls.models.bulk,ls.factor,ls.dep)

ls.factor=unique(ls.models.item$factor)
ls.models.item=add_p_batch(ls.models.item,ls.factor,ls.dep)


famRisk.Depre_bulk_measures=reg_phewasStyle(ls.models.bulk,dat_short=targetdata,dat_long=dat_long,correctByFactor = T)
famRisk.Depre_items=reg_phewasStyle(ls.models.item,dat_short=targetdata,dat_long=dat_long,correctByFactor = T)



# Report diffs - defs and total scores ------------------------------------

diffs.p = targetdata[,grep('^CBCL',colnames(targetdata))]
diffs.p = diffs.p[,grep('\\.p$',colnames(diffs.p))]
diffs.y = targetdata[,grep('\\.y$',colnames(targetdata))]
diffs.y = diffs.y[,gsub('\\.p','\\.y',colnames(diffs.p))]

diffs.data = diffs.p-diffs.y
colnames(diffs.data)=gsub('\\.p','\\.diffs',colnames(diffs.p))

targetdata=data.frame(targetdata,diffs.data)


# dependent variables
ls.dep.behav=colnames(targetdata)[grep('^CBCL\\.',colnames(targetdata))]
ls.dep.all=ls.dep.behav[grepl('\\.diffs',ls.dep.behav)]

# factors
ls.factor=colnames(targetdata)[grep('^depression',colnames(targetdata))]
ls.factor=ls.factor[grep('_noManiaVision$',ls.factor)]
ls.factor=ls.factor[!grepl('_family_',ls.factor)]

# combine the two
ls.dep.factor.combo=expand.grid(ls.dep.all,ls.factor,stringsAsFactors = F)

# covs
ls.models=data.frame(dependent=ls.dep.factor.combo$Var1,
                     factor=ls.dep.factor.combo$Var2,
                     covs='',stringsAsFactors = F)
ls.models$covs=paste0(c('interview_age','I(interview_age^2)','gender','race_ethnicity','site_id_l'),
                      collapse='+') #'involuntary_unemployment','recent_socialdprv','cbcl_scr_dsm5_anxdisord_r','cbcl_scr_dsm5_adhd_r','household_income','ADHD_ever',


ls.models$covs[grep('family',ls.models$factor)]=paste0(ls.models$covs[grep('family',ls.models$factor)],'+alc_family+drug_family')
ls.models$covs[grep('bioFather',ls.models$factor)]=paste0(ls.models$covs[grep('bioFather',ls.models$factor)],
                                                          '+alc_bioFather+drug_bioFather+bioFather_remote')
ls.models$covs[grep('bioMother',ls.models$factor)]=paste0(ls.models$covs[grep('bioMother',ls.models$factor)],
                                                          '+alc_bioMother+drug_bioMother+bioMother_remote')

# model.est
ls.models$model.est=''
ls.models$model.est[grep('hemi',ls.models$covs)]='lme'
ls.models$model.est[ls.models$model.est=='']='glm'

# Divide models for bulk measures and items
## Divide into bulk measures and items

ls.models.bulk=ls.models[!grepl('items',ls.models$dependent),]
ls.models.item=ls.models[grepl('items',ls.models$dependent),]


ls.dep=c('\\.p','\\.y')

ls.factor=unique(ls.models.bulk$factor)
ls.models.bulk=add_p_batch(ls.models.bulk,ls.factor,ls.dep)

ls.factor=unique(ls.models.item$factor)
ls.models.item=add_p_batch(ls.models.item,ls.factor,ls.dep)


famRisk.diffsDepre_bulk_measures=reg_phewasStyle(ls.models.bulk,dat_short=targetdata,dat_long=dat_long,correctByFactor = T)
famRisk.diffsDepre_items=reg_phewasStyle(ls.models.item,dat_short=targetdata,dat_long=dat_long,correctByFactor = T)


save(famRisk.Depre_bulk_measures,famRisk.Depre_items,famRisk.diffsDepre_bulk_measures,famRisk.diffsDepre_items,
     file='result/x.Supplementary_materials/FAMrisk.behav.CBCL.RData')
