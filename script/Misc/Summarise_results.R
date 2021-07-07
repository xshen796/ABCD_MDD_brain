load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')

summ_results<-function(result,ls.keywd){
  keywd.dep=as.character(ls.keywd[1])
  keywd.factor=as.character(ls.keywd[2])
  bulk.loc=grep(keywd.dep,result$dependent)
  bulk.res=result[bulk.loc,]
  bulk.loc=grep(keywd.factor,bulk.res$factor)
  bulk.res=bulk.res[bulk.loc,]
  bulk.res$p.corrected=p.adjust(bulk.res$p.value,method='fdr')
  
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
# summ_results(result = result.YouthDepre.region,
#                keywd.dep = '',keywd.factor = 'KSADS.MDD.p')



# New p correction and complete labels ------------------------------------

targetresult=result.YouthDepre.region.covWholeB
ls.keywd=ls.results

for (i in 1:nrow(ls.keywd)){
      keywd.dep=as.character(ls.keywd[i,1])
      keywd.factor=as.character(ls.keywd[i,2])
      bulk.loc=grep(keywd.dep,targetresult$dependent)
      bulk.res=targetresult[bulk.loc,]
      bulk.loc=grep(keywd.factor,bulk.res$factor)
      bulk.res=bulk.res[bulk.loc,]
      bulk.res$p.corrected=p.adjust(bulk.res$p.value,method='fdr')
      
      if(i==1){
            new.result=bulk.res
      }else{
            new.result=rbind(new.result,bulk.res)
      }
}
targetresult_p_corrected=new.result
sig.targetresult=targetresult_p_corrected[targetresult_p_corrected$p.corrected<0.05,]

# Organise labels
# Freesurfer
data.label = read.table('data/ls.Freesurfer_vars.csv',sep = '\t',stringsAsFactors = F,header = T)
data.label = filter(data.label, File.name == 'abcd_smrip101')
data.label = data.label[grep('smri_thick_cdk_',data.label$Field.name),]
data.label = data.label[!grepl('smri_thick_cdk_mean',data.label$Field.name),]
data.label = data.label[grep('Cortical thickness in mm of APARC ROI lh',data.label$Description),]
data.label$Description = gsub('Cortical thickness in mm of APARC ROI lh-','',data.label$Description)
data.label$Field.name = gsub('smri_thick_cdk_','',data.label$Field.name)
data.label$Field.name = gsub('lh$','',data.label$Field.name)
data.label$Field.name = paste0('APARC.',data.label$Field.name)
data.label.FS = data.label
# FA
data.label = read.table('data/ls.DTI_vars.csv',sep = '\t',stringsAsFactors = F,header = T)
data.label = filter(data.label, File.name == 'abcd_dti_p101')
data.label = data.label[grep('dmri_dtifa_fiberat_',data.label$Field.name),]
data.label = data.label[grep('lh$',data.label$Field.name),]
data.label$Description = gsub('Average fractional anisotropy within DTI atlas tract left ','FA of ',data.label$Description)
data.label$Field.name = gsub('dmri_dtifa_fiberat_','',data.label$Field.name)
data.label$Field.name = gsub('lh$','',data.label$Field.name)
data.label$Field.name = paste0('dtiFA.FiberAtlas.',data.label$Field.name)
data.label.FA=data.label

data.label=rbind(data.label.FA,data.label.FS)

# Add labels to results
targetresult=sig.targetresult
targetresult$label=targetresult$dependent
targetresult$label=gsub('vol.','Volume of ',targetresult$label)
targetresult$label=gsub('sa.','Surface area of ',targetresult$label)
targetresult$label=gsub('sulc.','Sulcal depth of ',targetresult$label)

for (i in 1:nrow(data.label)){
      loc=grepl(data.label$Field.name[i],targetresult$label)
      targetresult$label[loc]=gsub(data.label$Field.name[i],data.label$Description[i],targetresult$label[loc])
}



# Draft -------------------------------------------------------------------

groot2=targetresult[c(19,21,26,30,32,33),]
groot2=targetresult[c(18:34)[!18:34 %in% c(19,21,26,30,32,33)],]

