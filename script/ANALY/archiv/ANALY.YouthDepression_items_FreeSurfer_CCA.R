# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
library('CCA')
setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0.1//FamilialRisk_PGRS_MDD')

# Load data ---------------------------------------------------------------
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income','interview_age','sex','fsqc_qu_motion','site_id_l','race_ethnicity')#,'household_income','ADHD_ever'
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
ls.covs=c('fsqc_qu_motion','interview_age','sex','recent_socialdprv')
IM.dat=IM.dat[,c(1,grep('\\.APARC',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)



# Set categorical variables -----------------------------------------------
tab.data$race_ethnicity = as.factor(tab.data$race_ethnicity)
tab.data$sex = as.factor(tab.data$sex)
tab.data$fsqc_qu_motion = as.factor(tab.data$fsqc_qu_motion)
tab.data$site_id_l = as.factor(tab.data$site_id_l)
tab.data$bioFather_remote = as.factor(tab.data$bioFather_remote)
tab.data$bioMother_remote = as.factor(tab.data$bioMother_remote)

targetdata = tab.data
colnames(targetdata)[1]='f.eid'
rm(FAM.dat,IM.dat,behav.dat)

# Data preparation --------------------------------------------------------
targetdata=targetdata

# reorganise imaging data (mean of lh and rh)
ls.IM=colnames(targetdata)[grep('vol\\.',colnames(targetdata))] #|thk\\.|vol\\.|sulc\\.
lh.items=ls.IM[grep('^lh.',ls.IM)]
rh.items=ls.IM[grep('^rh.',ls.IM)]

bl.fs.data=(targetdata[,lh.items]+targetdata[,rh.items])/2
colnames(bl.fs.data)=gsub('lh\\.','',colnames(bl.fs.data))

# add covariates to the data
ls.covs=c('interview_age','sex','fsqc_qu_motion','site_id_l','race_ethnicity')#'involuntary_unemployment','household_income','ADHD_ever','recent_socialdprv',
bl.fs.data=data.frame(f.eid=targetdata$f.eid,bl.fs.data,targetdata[,ls.covs])
bl.fs.data$interview_age2=bl.fs.data$interview_age^2
bl.fs.data=bl.fs.data[complete.cases(bl.fs.data),]

# residulise imaging data
adj_cov <- function(dat.toadjust,dat.cov,cov.ls){
      eq = paste0('dat.toadjust~',paste0(cov.ls,collapse = '+'))
      fit = glm(as.formula(eq),data = dat.cov)
      new.dat = scale(resid(fit))
      return(new.dat)
}

targetdata=bl.fs.data
for (i in colnames(targetdata)[grep('.APARC.',colnames(targetdata))]){
      dat.target = targetdata[,i]
      corrected.single = adj_cov(dat.target,targetdata,ls.covs)
      if (i==colnames(targetdata)[grep('.APARC.',colnames(targetdata))][1]){
            dat.corrected = corrected.single
      }else{
            dat.corrected = cbind(dat.corrected,corrected.single)
      }
}
colnames(dat.corrected)=gsub('lh\\.','',lh.items)
IM.dat=dat.corrected
row.names(IM.dat)=bl.fs.data$f.eid

# factors
targetdata=tab.data
ls.factor=colnames(targetdata)[grep('^KSADS\\.',colnames(targetdata))] #^CBCL\\.|
ls.factor=ls.factor[grepl('items',ls.factor)]
ls.factor=c(ls.factor[grep('.p$',ls.factor)])
ls.factor=c(ls.factor[grep('.Ever',ls.factor)])

depre.dat=targetdata[targetdata$src_subject_id %in% rownames(IM.dat),]
depre.dat=depre.dat[,ls.factor]
depre.dat[is.na(depre.dat)]=0


# Analysis ----------------------------------------------------------------

# check correlations for X, Y and X & Y
corre.depre.IM = matcor(depre.dat,IM.dat)
img.matcor(corre.depre.IM,type=2)

# classical CCA
res.cc = cc(as.matrix(depre.dat),as.matrix(IM.dat))
barplot(res.cc$cor,xlab='Dimension',ylim=c(0,1))
plt.cc(res.cc)


# rCCA
res.regul = estim.regul(as.matrix(depre.dat),as.matrix(IM.dat),plt=T)
res.rcc = rcc(as.matrix(depre.dat),as.matrix(IM.dat),0,0)
barplot(res.rcc$cor,xlab='Dimension', ylab = 'Canonical correlations',ylim=c(0,0.8))
plt.cc(res.rcc)

save(result.fs,file='result/i.Main_result/YouthDepression_to_freesurfer.RData')
