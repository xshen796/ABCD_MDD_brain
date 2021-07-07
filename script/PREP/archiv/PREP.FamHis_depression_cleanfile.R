# by XShen
# 16/5/2019
# Aims: remove parents with other major psychiatric conditions,
#       clean up depression-related phenotypes in parents,
#       remove adolescents with severe substance use conditions,
#       add some key covariates: SES, ADHD condition in children and recent social deprivation.
# Before running this one, PREP.FamHis_depression.R needs to be run first.

library(dplyr)

setwd('/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0.1//FamilialRisk_PGRS_MDD')
tmp.dir.var.ls='data/ls.FAM_demographic.csv'
ls.var=read.csv(tmp.dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
FAM.dat=readRDS('data/FamHistory/FAM.dat_AllSubjects.rds')


# mania/vision in 1st-degree relatives remove -----------------------------------------------------

# extract data first-degree relatives who have mania/visions
tmp.ls.otherP=c('^fam_history_7','_mania$','^fam_history_8','_visions$')
ls.var.torm=ls.var[grep(paste0(tmp.ls.otherP,collapse = '|'),ls.var$Field.name),]
tmp.ls.1strela=c('q7a','q7d','_full_sib_','_half_sib_')
ls.var.torm.1strela=ls.var.torm[grep(paste0(tmp.ls.1strela,collapse = '|'),ls.var.torm$Field.name),]

# extract participants who has no 1st-degree relative who have mania/visions
tmp.datsubsrm.1strela=FAM.dat[,c('src_subject_id',ls.var.torm.1strela$Field.name)]
tmp.datsubsrm.1strela[tmp.datsubsrm.1strela==999]=NA
tmp.datsubsrm.1strela$rm.Y=rowSums(tmp.datsubsrm.1strela[,2:ncol(tmp.datsubsrm.1strela)]==1,na.rm=T)
ls.sub.keep=data.frame(src_subject_id=tmp.datsubsrm.1strela$src_subject_id,keep.NOmania_visions=1)
ls.sub.keep=ls.sub.keep[tmp.datsubsrm.1strela$rm.Y==0,]

rm(list=ls(pattern = '^tmp.'))

# remove columns in FAM.dat in relation to mania/visions
FAM.dat.clean=merge(FAM.dat,ls.sub.keep,by='src_subject_id',all.x=T)
tmp.ls.colrm=paste0(ls.var.torm$Field.name,collapse = '|')
FAM.dat.clean=FAM.dat.clean[,!grepl(tmp.ls.colrm,colnames(FAM.dat.clean))]



# substance use in adolescents remove -------------------------------------

# extract data - adolescents with substance use problems
tmp.ls.sud=c('^ksads_20_')
ls.var.torm=ls.var[grep(tmp.ls.sud,ls.var$Field.name),]

# extract participants who has no 1st-degree relative who have mania/visions
tmp.datsubsrm=FAM.dat.clean[,c('src_subject_id',ls.var.torm$Field.name)]
tmp.datsubsrm[tmp.datsubsrm==999]=NA
tmp.datsubsrm$rm.Y=rowSums(tmp.datsubsrm[,2:ncol(tmp.datsubsrm)]==1,na.rm=T)
ls.sub.keep=data.frame(src_subject_id=tmp.datsubsrm$src_subject_id,keep.NOsud=1)
ls.sub.keep=ls.sub.keep[tmp.datsubsrm$rm.Y==0,]

rm(list=ls(pattern = '^tmp.'))

# remove columns in FAM.dat in relation to mania/visions
FAM.dat.clean=merge(FAM.dat.clean,ls.sub.keep,by='src_subject_id',all.x=T)
tmp.ls.colrm=paste0(ls.var.torm$Field.name,collapse = '|')
FAM.dat.clean=FAM.dat.clean[,!grepl(tmp.ls.colrm,colnames(FAM.dat.clean))]




# mix multiple similar relative measures ----------------------------------

sum_cc <- function(x,relaid) {
      loc = grep(relaid,colnames(x))
      if (length(loc)<=1){
            tmp.output=x[,loc]
            tmp.output[tmp.output==999]=NA
            tmp.output[tmp.output==7]=NA
      }else{
            tmp.dat = x[,loc]
            tmp.dat[tmp.dat==999]=NA
            tmp.dat[tmp.dat==7]=NA
            
            tmp.output = rowSums(tmp.dat==1,na.rm=T)
            tmp.output[tmp.output>0]=1 # any case=1
            tmp.dat$none = rowSums(is.na(tmp.dat))
            tmp.output[(tmp.dat$none>0)&(tmp.output==0)]=NA # any NA will be removed for controls   
      }
      return(tmp.output)
}

ls.q=c(6,11:13)
ls.qname=c('depression','professional_help','hospitalised','suicide')
targetdata=FAM.dat.clean
      for (q in ls.q){
            i=paste0('q',q)
            code.anybloodrela=paste0('fam_history_',q,'_')
            code.father=paste0('_',i,'a_')
            code.mother=paste0('_',i,'d_')
            code.pat.grandfa=paste0('_',i,'b_')
            code.pat.grandmo=paste0('_',i,'c_')
            code.mat.grandfa=paste0('_',i,'e_')
            code.mat.grandmo=paste0('_',i,'f_')
            
            tmp.blockdat=targetdata[,grep(paste0(i,'|',code.anybloodrela),colnames(targetdata))]
            
            # other relatives
            tmp.dat.output=data.frame(all_relatives=sum_cc(tmp.blockdat,code.anybloodrela),
                                      all_1stdegree=sum_cc(tmp.blockdat,paste0(c(code.father,code.mother),collapse = '|')),
                                      all_2nddegree=sum_cc(tmp.blockdat,paste0(c(code.pat.grandfa,code.pat.grandmo,
                                                                               code.mat.grandfa,code.mat.grandmo,
                                                                               '_pat_uncle','_pat_aunt_',
                                                                               '_mat_uncle','_mat_aunt_'),collapse = '|')),
                                      father=sum_cc(tmp.blockdat,code.father),
                                      mother=sum_cc(tmp.blockdat,code.mother),
                                      pat_2nddegree=sum_cc(tmp.blockdat,paste0(c(code.pat.grandfa,code.pat.grandmo,
                                                           '_pat_uncle','_pat_aunt_'),collapse = '|')),
                                      mat_2nddegree=sum_cc(tmp.blockdat,paste0(c(code.mat.grandfa,code.mat.grandmo,
                                                           '_mat_uncle','_mat_aunt_'),collapse = '|')))
            colnames(tmp.dat.output)=paste0(ls.qname[grep(q,ls.q)],'_',colnames(tmp.dat.output))
            
            if (q == ls.q[1]){dat.final.simp=tmp.dat.output}else{
                  dat.final.simp=cbind(dat.final.simp,tmp.dat.output)
            }
      }

# add all relative drugs and alc data
dat.final.simp$alc_all_relatives=targetdata$famhx_4_p
dat.final.simp$drugs_all_relatives=targetdata$fam_history_5_yes_no


## replace the long column data with data generated above
FAM.dat.clean=data.frame(FAM.dat.clean[,
                                       !grepl('^fam_history|^famhx|_depression|_nerves|_professional|_prof$|_hospitalized|_hosp$|_suicide',
                                              colnames(FAM.dat.clean))],
                         dat.final.simp)


# New depression pheno = depre + professional -----------------------------

depre.block = FAM.dat.clean[,grep('depression',colnames(FAM.dat.clean))]
professional.block = FAM.dat.clean[,grep('professional',colnames(FAM.dat.clean))]
for (i in 1:ncol(depre.block)){
      tmp.depre.col = depre.block[,i]
      tmp.professional.col = professional.block[,i]
      
      tmp.add = tmp.depre.col + tmp.professional.col
      tmp.add[tmp.add==1]=NA
      tmp.add[tmp.add==2]=1
      if (i==1){
            new.depre.phenos = tmp.add
      }else{
            new.depre.phenos = cbind(new.depre.phenos,tmp.add)
      }
}
colnames(new.depre.phenos) = gsub('depression_','depression_professional_',colnames(depre.block))
new.depre.phenos = data.frame(new.depre.phenos)

FAM.dat.clean=data.frame(FAM.dat.clean[,
                                       !grepl('^depression_|^professional_',
                                              colnames(FAM.dat.clean))],
                         new.depre.phenos)


# Incremental depression definitions --------------------------------------
incre_depre <- function(x,item1,item2,item3) {
      loc1 = grep(item1,colnames(x))
      loc2 = grep(item2,colnames(x))
      
      tmp.dat1 = x[,loc1]
      tmp.dat2 = x[,loc2]
      
      for (i in 1:ncol(tmp.dat1)){
            i.item=tmp.dat1[,i]
            i.item[(tmp.dat2[,i]==1)&(i.item==1)]=2
            i.item[i.item==1]=NA
            i.item[i.item==2]=1
            tmp.dat1[,i]=i.item
      }

      return(tmp.dat1)
}

depre_hospitalised=incre_depre(FAM.dat.clean,'^depression','^hospitalised')
depre_suicide=incre_depre(FAM.dat.clean,'^depression','^suicide')

colnames(depre_hospitalised)=gsub('depression_professional','depression_profess_hospt',colnames(depre_hospitalised))
colnames(depre_suicide)=gsub('depression_professional','depression_profess_suicd',colnames(depre_suicide))

FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^nerves|^professional_help|^hospitalised|^suicide_',colnames(FAM.dat.clean))],
                         depre_hospitalised,depre_suicide,FAM.dat.clean[,grepl('^suicide_',colnames(FAM.dat.clean))])



# create a column to remove parents who left family -----------------------

prtdemo.dat=FAM.dat.clean %>% 
      select(src_subject_id=src_subject_id,
             Parent_as=demo_prim,
             Has_partner_Y=demo_prnt_prtnr_v2,
             Prtnr_BioParent_Y=demo_prnt_prtnr_bio)

# identify remote biological father
bioFather_remote=rep(0,nrow(prtdemo.dat))
bioFather_remote[(prtdemo.dat$Parent_as==1)&(prtdemo.dat$Has_partner_Y==2)]=1  # mother + no partner
bioFather_remote[(prtdemo.dat$Parent_as==1)&(prtdemo.dat$Prtnr_BioParent_Y==2)]=1  # mother + partner not biological parent
bioFather_remote[(prtdemo.dat$Parent_as>=3)]=1  # non-biological parent
# identify remote biological mother
bioMother_remote=rep(0,nrow(prtdemo.dat))
bioMother_remote[(prtdemo.dat$Parent_as==2)&(prtdemo.dat$Has_partner_Y==2)]=1  # father + no partner
bioMother_remote[(prtdemo.dat$Parent_as==2)&(prtdemo.dat$Prtnr_BioParent_Y==2)]=1  # father + partner not biological parent
bioMother_remote[(prtdemo.dat$Parent_as>=3)]=1  # non-biological parent
# new dat to merge
prtdemo.dat.tomerge=data.frame(src_subject_id=prtdemo.dat$src_subject_id,
                               bioFather_remote=bioFather_remote,
                               bioMother_remote=bioMother_remote)
FAM.dat.clean=merge(FAM.dat.clean,prtdemo.dat.tomerge,by='src_subject_id',all.x=T)
FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^demo_prim|^demo_prnt_prtnr_v2|^demo_prnt_prtnr_bio',colnames(FAM.dat.clean))])



# create a column for household SES ---------------------------------------

prtincome.dat=FAM.dat.clean %>% 
      select(src_subject_id=src_subject_id,
             Prnt_employment=demo_prnt_empl_v2,
             Prnt_income=demo_prnt_income_v2,
             Prtnr_employment=demo_prtnr_empl_v2,
             Prtnr_income=demo_prtnr_income_v2,
             Prnts_income_total=demo_comb_income_v2)

prtincome.dat[prtincome.dat>=777]=NA

ls.involuntary.unemployment=c(2,9,3,11,5)

prtincome.dat$involuntary_unemployment=rowSums(cbind(prtincome.dat$Prnt_employment %in% ls.involuntary.unemployment,
                                                     prtincome.dat$Prtnr_employment %in% ls.involuntary.unemployment),na.rm = T)
prtincome.dat$involuntary_unemployment[prtincome.dat$involuntary_unemployment>1]=1
loc.bothNAs=rowSums(cbind(is.na(prtincome.dat$Prnt_employment),
                          is.na(prtincome.dat$Prtnr_employment)))
loc.bothNAs=loc.bothNAs==2
prtincome.dat$involuntary_unemployment[loc.bothNAs]=NA
prtincome.dat$household_income=prtincome.dat$Prnts_income_total

FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('_income_|_empl_',colnames(FAM.dat.clean))],
                         prtincome.dat[,(ncol(prtincome.dat)-1):ncol(prtincome.dat)],stringsAsFactors = F)

# create a column for recent social deprivation ---------------------------------------

socialdepr.dat=FAM.dat.clean %>% 
      select(src_subject_id=src_subject_id,starts_with('demo_fam_exp'))
socialdepr.dat[socialdepr.dat>=777]=NA

recent_socialdprv=rowSums(socialdepr.dat[,2:ncol(socialdepr.dat)],na.rm=T)
loc.allNAs=rowSums(is.na(socialdepr.dat[,2:ncol(socialdepr.dat)]))
loc.allNAs=loc.allNAs==7
recent_socialdprv[loc.allNAs]=NA

FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^demo_fam_exp',colnames(FAM.dat.clean))],
                         recent_socialdprv=recent_socialdprv,stringsAsFactors = F)

# create a column for ADHD ever ---------------------------------------

ADHD_ever=rowSums(FAM.dat.clean[,grep('ksads_14',colnames(FAM.dat.clean))],na.rm = T)
ADHD_ever[ADHD_ever>1]=1
loc.allNAs=rowSums(is.na(FAM.dat.clean[,grep('ksads_14',colnames(FAM.dat.clean))]))==4
ADHD_ever[loc.allNAs]=NA

FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^ksads_14',colnames(FAM.dat.clean))],
                         ADHD_ever=ADHD_ever,stringsAsFactors = F)



# add a column for scanner type -------------------------------------------

image_info=readRDS('/sdata/images/projects/UKBIOBANK/ABCD/release2.0/iii.data/MRI_file_sharing/image03.rds')
image_info=image_info[,c("src_subject_id","scan_type","scanner_manufacturer_pd")]
image_info=filter(image_info,scan_type=='MR structural (T1)')
image_info=image_info[,c("src_subject_id","scanner_manufacturer_pd")]

FAM.dat.clean=merge(FAM.dat.clean,image_info,by='src_subject_id',all.x=T)

# remove participants -----------------------------------------------------

FAM.dat.clean=filter(FAM.dat.clean,!is.na(keep.NOmania_visions),!is.na(keep.NOsud)) # remove subs who have 1st-degree relatives with mania/visions
FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^keep\\.',colnames(FAM.dat.clean))])
saveRDS(FAM.dat.clean,file='data/FamHistory_clean/FAM.dat_AllSubjects_clean.rds')
### unrelated subs
FAM.dat.unrelated=FAM.dat.clean[!duplicated(FAM.dat.clean$rel_family_id),]
saveRDS(FAM.dat.unrelated,file='data/FamHistory_clean//FAM.dat_Unrelated_clean.rds')
### twin pairs
onetwin.ID = !FAM.dat$src_subject_id %in% FAM.dat.unrelated$src_subject_id
onetwin.fam.ID = FAM.dat$rel_family_id[onetwin.ID]
loc.withonetwin = FAM.dat$rel_family_id %in% onetwin.fam.ID
FAM.dat.twins=FAM.dat[loc.withonetwin,]
saveRDS(FAM.dat.twins,file='data/FamHistory_clean//FAM.dat_TwTri_clean.rds')
