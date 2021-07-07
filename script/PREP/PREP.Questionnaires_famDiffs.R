# Basic setups ------------------------------------------------------------

library(dplyr)
setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')


# Load data ---------------------------------------------------------------

FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_TwTri_clean.rds')
FAM.dat=FAM.dat[,c(1:4,6:ncol(FAM.dat))]
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income','')#,'household_income','ADHD_ever'
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)
locs.covs=grep(paste0(ls.covs,collapse='|'),colnames(tab.data))


# Add sib diffs data ------------------------------------------------------
targetdata=tab.data

# extract fam mean
rep.row<-function(x,n){
      matrix(rep(x,each=n),nrow=n)
}
for (i in unique(targetdata$rel_family_id)){
      fam.dat.total = targetdata[targetdata$rel_family_id==i,]
      fam.dat.items = fam.dat.total[,grep('^CBCL\\.|^KSADS\\.',colnames(fam.dat.total))]
      fam.dat.items = fam.dat.items[,!grepl('items',colnames(fam.dat.items))]
      fam.dat.mean = data.frame(src_subject_id=fam.dat.total$src_subject_id,
                                rep.row(colMeans(fam.dat.items),nrow(fam.dat.items)))
      if (i==unique(targetdata$rel_family_id)[1]){
            fam.mean = fam.dat.mean
      }else{
            fam.mean = rbind(fam.mean,fam.dat.mean)
      }
}
colnames(fam.mean)[2:ncol(fam.mean)]=paste0(colnames(fam.dat.items),'.famMean')

targetdata = merge(targetdata,fam.mean,by='src_subject_id')

# extract individual devs
indiv.dat.items = targetdata[,grep('^CBCL\\.|^KSADS\\.',colnames(targetdata))]
indiv.dat.items = indiv.dat.items[,!grepl('items',colnames(indiv.dat.items))]
indiv.dat.items = indiv.dat.items[,!grepl('.famMean',colnames(indiv.dat.items))]

indiv.dat = indiv.dat.items-fam.mean[,2:ncol(fam.mean)]
colnames(indiv.dat)=paste0(colnames(indiv.dat),'.devFromFam')

targetdata = data.frame(targetdata,indiv.dat)

saveRDS(targetdata,file = 'data/Behavioural/Behavioural_famDiffs.rds')
