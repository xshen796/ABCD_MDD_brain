targetdata=targetdata[!is.na(targetdata$KSADS.MDD.p)&!is.na(targetdata$KSADS.Depressive_symptoms_ever.p)&
                            !is.na(targetdata$depression_family_noManiaVision)&
                            !is.na(targetdata$depression_bioFather_noManiaVision)&
                            !is.na(targetdata$depression_bioMother_noManiaVision),]
ls.IM=unique(ls.model.bulk$dependent)
for (i in ls.IM){
      
      fit.0=glm(as.formula(paste0(i,'~interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity')),
                data=targetdata)
      fit.total=glm(as.formula(paste0(i,'~interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+',
                          'KSADS.Depressive_symptoms_ever.p+KSADS.MDD.p+depression_bioMother_noManiaVision+',
                          'depression_bioFather_noManiaVision+depression_family_noManiaVision')),data=targetdata)
      fit.depre=glm(as.formula(paste0(i,'~interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+',
                                      'depression_bioMother_noManiaVision+',
                                      'depression_bioFather_noManiaVision+depression_family_noManiaVision')),data=targetdata)
      fit.famRisk=glm(as.formula(paste0(i,'~interview_age+I(interview_age^2)+sex+fsqc_qu_motion+site_id_l+race_ethnicity+',
                                        'KSADS.Depressive_symptoms_ever.p+KSADS.MDD.p')),data=targetdata)
      
      r2.all=Dsquared(model = fit.total,adjust = T)-Dsquared(model = fit.0,adjust = T)
      r2.famRisk=Dsquared(model = fit.total,adjust = T)-Dsquared(model = fit.famRisk,adjust = T)
      r2.depre=Dsquared(model = fit.total,adjust = T)-Dsquared(model = fit.depre,adjust = T)
      
      pro.famRisk=r2.famRisk/r2.all*100
      pro.depre=r2.depre/r2.all*100
      pro.shared=(r2.all-r2.famRisk-r2.depre)/r2.all*100
      
      tmp.pro=c(pro.depre,pro.famRisk,pro.shared)
      if (i==ls.IM[1]){pro.summary=tmp.pro}else{pro.summary=rbind(pro.summary,tmp.pro)}
      
}
colnames(pro.summary)=c('pro.depre','pro.famRisk','pro.shared')
pro.summary=data.frame(IM=ls.IM,pro.summary)

pro.summary.sig=pro.summary[c(3:5),]

save(pro.summary.sig,file='result/i.Main_result/R2_famRisk_n_Depre.RData')






