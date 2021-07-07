library('dplyr')
library('pbapply')
library('nlme')
library('reshape2')
setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD/')


# Depression --------------------------------------------------------------

load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')
source('FUNs/wholeB_correction.R')
TargetResult = result.YouthDepre.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
result.YouthDepre.region.covWholeB=result.wholeBcorrected
rm(TargetResult)

summ_results<-function(result,ls.keywd){
  keywd.dep=as.character(ls.keywd[1])
  keywd.factor=as.character(ls.keywd[2])
  bulk.loc=grep(keywd.dep,result$dependent)
  bulk.res=result[bulk.loc,]
  bulk.loc=grep(keywd.factor,bulk.res$factor)
  bulk.res=bulk.res[bulk.loc,]
  #bulk.res$p.corrected=p.adjust(bulk.res$p.value,method='fdr')
  
  sig.N=sum(bulk.res$p.corrected<0.05)
  if (sig.N==0){res_summary=c(NA,NA,NA,NA,0)}else{
    bulk.res=filter(bulk.res,p.corrected<0.05)
    beta.range=summary(abs(bulk.res$beta))[c(1,6)]
    p.range=summary(bulk.res$p.corrected)[c(6,1)]
    res_summary=c(beta.range,p.range,sig.N)
  }

  return(res_summary)
}

ls.results = expand.grid(c('vol\\.','thk\\.','sa\\.','sulc\\.','FA\\.','MD\\.'),
                          unique(result.YouthDepre.region.covWholeB$factor),
                         stringsAsFactors = F)
groot = apply(X = ls.results,MARGIN = 1,FUN = summ_results,
              result=result.YouthDepre.region.covWholeB)
groot = t(groot)
groot = data.frame(ls.results,groot)  
colnames(groot)=c('keywd.dep','keywd.factor','beta.min','beta.max','p.max','p.min','Nsig')




# FamRisk -----------------------------------------------------------------

load('result/i.Main_result/famRisk_noSocialCov.RData')
source('FUNs/wholeB_correction.R')
TargetResult = result.famRisk.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
result.YouthDepre.region.covWholeB=result.wholeBcorrected
rm(TargetResult)

summ_results<-function(result,ls.keywd){
  keywd.dep=as.character(ls.keywd[1])
  keywd.factor=as.character(ls.keywd[2])
  bulk.loc=grep(keywd.dep,result$dependent)
  bulk.res=result[bulk.loc,]
  bulk.loc=grep(keywd.factor,bulk.res$factor)
  bulk.res=bulk.res[bulk.loc,]
  #bulk.res$p.corrected=p.adjust(bulk.res$p.value,method='fdr')
  
  sig.N=sum(bulk.res$p.corrected<0.05)
  if (sig.N==0){res_summary=c(NA,NA,NA,NA,0)}else{
    bulk.res=filter(bulk.res,p.corrected<0.05)
    beta.range=summary(abs(bulk.res$beta))[c(1,6)]
    p.range=summary(bulk.res$p.corrected)[c(6,1)]
    res_summary=c(beta.range,p.range,sig.N)
  }
  
  return(res_summary)
}

ls.results = expand.grid(c('vol\\.','thk\\.','sa\\.','sulc\\.','FA\\.','MD\\.'),
                         unique(result.YouthDepre.region.covWholeB$factor),
                         stringsAsFactors = F)
groot = apply(X = ls.results,MARGIN = 1,FUN = summ_results,
              result=result.YouthDepre.region.covWholeB)
groot = t(groot)
groot = data.frame(ls.results,groot)  
colnames(groot)=c('keywd.dep','keywd.factor','beta.min','beta.max','p.max','p.min','Nsig')

