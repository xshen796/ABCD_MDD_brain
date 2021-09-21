# by XShen
# 16/5/2019
# Aims: remove parents with other major psychiatric conditions,
#       clean up depression-related phenotypes in parents,
#       remove adolescents with severe substance use conditions,
#       add some key covariates: SES, ADHD condition in children and recent social deprivation.
# Before running this one, PREP.FamHis_depression.R needs to be run first.

library(dplyr)

setwd('/exports/igmm/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD/')
tmp.dir.var.ls='data/ls.FAM_demographic.csv'
ls.var=read.csv(tmp.dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
FAM.dat=readRDS('data/FamHistory/FAM.dat_AllSubjects.rds')


FAM.dat.clean=FAM.dat


# Functions for tidying parental data -------------------------------------

parents_to_remove <- function(keywd,data,outputkw){
      fam.kw=keywd[1]
      bioMo.kw=keywd[2]
      bioFa.kw=keywd[3]
      
      fam.dat=data[,fam.kw]
      fam.dat[fam.dat==7]=NA
      fam.dat[fam.dat==999]=NA
      
      tmp.blockdat=data[,grep(bioMo.kw,colnames(data))]
      tmp.blockdat[tmp.blockdat==7]=NA
      tmp.blockdat[tmp.blockdat==999]=NA
      if(is.vector(tmp.blockdat)){
            bioMo.dat=tmp.blockdat
      }else{bioMo.dat=rowSums(tmp.blockdat,na.rm=T)}
      
      tmp.blockdat=data[,grep(bioFa.kw,colnames(data))]
      tmp.blockdat[tmp.blockdat==7]=NA
      tmp.blockdat[tmp.blockdat==999]=NA
      if(is.vector(tmp.blockdat)){
            bioFa.dat=tmp.blockdat
      }else{bioFa.dat=rowSums(tmp.blockdat,na.rm=T)}
      
      new.dat=data.frame(cbind(fam.dat,bioMo.dat,bioFa.dat))
      colnames(new.dat)=paste0(outputkw,c('_family','_bioMother','_bioFather'))
      return(new.dat)
}
      
# Substance use in parents ------------------------------------------------
# Alcohol
keywd.ls=c('famhx_4_p','^famhx_4d_','^famhx4a_')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='alc'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]

# Drug
keywd.ls=c('fam_history_5_yes_no','^fam_history_q5d_drugs','^fam_history_q5a_drugs')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='drug'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]

# These two are covariates, so set NAs as 0
block.dat=FAM.dat.clean[,grep('^alc_|^drug_',colnames(FAM.dat.clean))]
block.dat[is.na(block.dat)]=0
FAM.dat.clean[,grep('^alc_|^drug_',colnames(FAM.dat.clean))]=block.dat


# mania/vision in parents remove ------------------------------------------
# mania
keywd.ls=c('fam_history_7_yes_no','^fam_history_q7d_mania','^fam_history_q7a_mania')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='mania'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]

# vision
keywd.ls=c('fam_history_8_yes_no','^fam_history_q8d_visions','^fam_history_q8a_visions')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='visions'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]


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




#########################################################################################################

# Depression phenotypes ---------------------------------------------------

#########################################################################################################


# Functions for this section ----------------------------------------------

create_rm_ls = function(keywd.rm,data){
      ls.rm = paste0(keywd.rm,collapse = '|')
      remove.block.dat=FAM.dat.clean[,grep(ls.rm,colnames(data))]
      remove.ls=rowSums(remove.block.dat,na.rm=T)
      remove.ls[remove.ls>=1]=1
      return(remove.ls)
}

# Create phenotypes -------------------------------------------------------

# Basic depression phenotype
keywd.ls=c('fam_history_6_yes_no','fam_history_q6d_depression','fam_history_q6a_depression')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='depression'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]


# Remove confounding participants for family phenotype (parents who have mania/visian/alc/drug)
keywd.rm=c('mania_family','visions_family')#'alc_family','drug_family',
rm.ls=create_rm_ls(keywd.rm,FAM.dat.clean)
FAM.dat.clean$depression_family_noManiaVision=FAM.dat.clean$depression_family
FAM.dat.clean$depression_family_noManiaVision[rm.ls==1]=NA


# Remove confounding participants for bioFather phenotype (parents who have mania/visian/alc/drug)
keywd.rm=c('mania_bioFather','visions_bioFather')#'alc_family','drug_family',
rm.ls=create_rm_ls(keywd.rm,FAM.dat.clean)
FAM.dat.clean$depression_bioFather_noManiaVision=FAM.dat.clean$depression_bioFather
FAM.dat.clean$depression_bioFather_noManiaVision[rm.ls==1]=NA


# Remove confounding participants for bioMother phenotype (parents who have mania/visian/alc/drug)
keywd.rm=c('mania_bioMother','visions_bioMother')#'alc_family','drug_family',
rm.ls=create_rm_ls(keywd.rm,FAM.dat.clean)
FAM.dat.clean$depression_bioMother_noManiaVision=FAM.dat.clean$depression_bioMother
FAM.dat.clean$depression_bioMother_noManiaVision[rm.ls==1]=NA


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


# Create screening phenos for professional help
keywd.ls=c('fam_history_11_yes_no','^fam_history_q11d_professional','^fam_history_q11a_professional')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='professional'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]

depre_professional.noManiaVision=incre_depre(FAM.dat.clean,'noManiaVision$','^professional')
colnames(depre_professional.noManiaVision)=paste0(colnames(depre_professional.noManiaVision),'_professional')

FAM.dat.clean=data.frame(FAM.dat.clean,depre_professional.noManiaVision)


# Create screening phenos for hospitalisation
keywd.ls=c('fam_history_12_yes_no','^fam_history_q12d_hospitalized','^fam_history_q12a_hospitalized')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='hospitalised'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]

depre_hospitalised.noManiaVision=incre_depre(FAM.dat.clean,'noManiaVision_professional$','^hospitalised')
colnames(depre_hospitalised.noManiaVision)=gsub('_professional','_hospitalised',colnames(depre_hospitalised.noManiaVision))

FAM.dat.clean=data.frame(FAM.dat.clean,depre_hospitalised.noManiaVision)


# Create screening phenos for suicide
keywd.ls=c('fam_history_13_yes_no','^fam_history_q13d_suicide','^fam_history_q13a_suicide')
FAM.dat.clean = data.frame(FAM.dat.clean,parents_to_remove(keywd=keywd.ls, data=FAM.dat.clean, outputkw='suicide'))
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0(keywd.ls,collapse = '|'),colnames(FAM.dat.clean))]

depre_suicide.noManiaVision=incre_depre(FAM.dat.clean,'noManiaVision_hospitalised$','^suicide')
colnames(depre_hospitalised.noManiaVision)=gsub('_hospitalised','_suicide',colnames(depre_hospitalised.noManiaVision))

FAM.dat.clean=data.frame(FAM.dat.clean,depre_hospitalised.noManiaVision)


FAM.dat.clean$depression_bioMother_incremental = rowSums(FAM.dat.clean[,
                                                                       grep('^depression_bioMother_noManiaVision',
                                                                            colnames(FAM.dat.clean))],na.rm=T)
FAM.dat.clean$depression_bioMother_incremental[is.na(FAM.dat.clean$depression_bioMother_noManiaVision)]=NA
FAM.dat.clean$depression_bioFather_incremental = rowSums(FAM.dat.clean[,
                                                                       grep('^depression_bioFather_noManiaVision',
                                                                            colnames(FAM.dat.clean))],na.rm=T)
FAM.dat.clean$depression_bioFather_incremental[is.na(FAM.dat.clean$depression_bioFather_noManiaVision)]=NA

FAM.dat.clean$depression_family_incremental = rowSums(FAM.dat.clean[,
                                                                       grep('^depression_family_noManiaVision',
                                                                            colnames(FAM.dat.clean))],na.rm=T)
FAM.dat.clean$depression_family_incremental[is.na(FAM.dat.clean$depression_family_noManiaVision)]=NA

FAM.dat.clean=FAM.dat.clean[,!grepl(paste0('^hospitalised_|^suicide_|^professional',collapse = '|'),colnames(FAM.dat.clean))]
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0('^visions_|^mania_',collapse = '|'),colnames(FAM.dat.clean))]
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0('professional$|hospitalised$|suicide$',collapse = '|'),colnames(FAM.dat.clean))]
FAM.dat.clean=FAM.dat.clean[,!grepl(paste0('^depression_family$|^depression_bioMother$|^depression_bioFather$',collapse = '|'),colnames(FAM.dat.clean))]


# An additional phenotype for both parents
FAM.dat.clean$depression_parents_noManiaVision=rowSums(FAM.dat.clean[,c('depression_bioMother_noManiaVision',
                                                                      'depression_bioFather_noManiaVision')],na.rm=T)
loc.both.NA=rowSums(is.na(FAM.dat.clean[,c('depression_bioMother_noManiaVision',
                                     'depression_bioFather_noManiaVision')]))
FAM.dat.clean$depression_parents_noManiaVision[loc.both.NA==2]=NA

FAM.dat.clean$depression_parents_incremental=rowSums(FAM.dat.clean[,c('depression_bioMother_incremental',
                                                                        'depression_bioFather_incremental')],na.rm=T)
loc.both.NA=rowSums(is.na(FAM.dat.clean[,c('depression_bioMother_incremental',
                                           'depression_bioFather_incremental')]))
FAM.dat.clean$depression_parents_incremental[loc.both.NA==2]=NA

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
FAM.dat.clean$Parent_as=FAM.dat.clean$demo_prim
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

tidy_column <- function(x){
   x[x>=777]=NA
   return(x)
}

socialdepr.dat=FAM.dat.clean %>% 
      select(src_subject_id=src_subject_id,starts_with('demo_fam_exp')) %>%
      select(-ends_with('.1')) %>% 
      mutate_if(is.numeric, tidy_column)


recent_socialdprv=rowSums(socialdepr.dat[,2:ncol(socialdepr.dat)],na.rm=T)
loc.allNAs=rowSums(is.na(socialdepr.dat[,2:ncol(socialdepr.dat)]))
loc.allNAs=loc.allNAs==7
recent_socialdprv[loc.allNAs]=NA

FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^demo_fam_exp',colnames(FAM.dat.clean))],
                         recent_socialdprv=recent_socialdprv,stringsAsFactors = F)



# Create a column for family's highest educational attainment -------------

edu.dat=FAM.dat.clean %>% 
      select(src_subject_id=src_subject_id,
             parent1_edu=demo_prnt_ed_v2_l,
             parent2_edu=demo_prtnr_ed_v2_l)
edu.dat$parent1_edu[edu.dat$parent1_edu>=777]=NA
edu.dat$parent2_edu[edu.dat$parent2_edu>=777]=NA

fam_highest_edu=apply(edu.dat[,2:ncol(edu.dat)], 1, max ,na.rm=T)
fam_highest_edu[fam_highest_edu<0]=NA
edu.dat$fam_highest_edu=fam_highest_edu


FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('demo_prnt_ed_v2_l|demo_prtnr_ed_v2_l',colnames(FAM.dat.clean))],
                         fam_highest_edu=edu.dat$fam_highest_edu,stringsAsFactors = F)


# Create a column for ADHD ever and ADHD symptoms -------------------------

ADHD_ever=rowSums(FAM.dat.clean[,grep('ksads_14',colnames(FAM.dat.clean))],na.rm = T)
ADHD_ever[ADHD_ever>1]=1
loc.allNAs=rowSums(is.na(FAM.dat.clean[,grep('ksads_14',colnames(FAM.dat.clean))]))==4
ADHD_ever[loc.allNAs]=NA

FAM.dat.clean=data.frame(FAM.dat.clean[,!grepl('^ksads_14',colnames(FAM.dat.clean))],
                         ADHD_ever=ADHD_ever,stringsAsFactors = F)



# add a column for scanner type -------------------------------------------

image_info=readRDS('/exports/igmm/eddie/GenScotDepression/data/abcd/release2.0.1/iii.data/MRI_file_sharing/image03.rds')
image_info=image_info[,c("src_subject_id","scan_type","scanner_manufacturer_pd")]
image_info=filter(image_info,scan_type=='MR structural (T1)')
image_info=image_info[,c("src_subject_id","scanner_manufacturer_pd")]

FAM.dat.clean=merge(FAM.dat.clean,image_info,by='src_subject_id',all.x=T)

# add a column for ASR depression DSM-5 oriented score for parents --------
FAM.dat.clean$ASR_p_depression_DSM5=FAM.dat.clean$asr_scr_depress_r
FAM.dat.clean$ASR_p_depression_DSM5[FAM.dat.clean$asr_scr_depress_nm>(FAM.dat.clean$asr_scr_depress_total/2)]=NA

FAM.dat.clean=FAM.dat.clean[,!grepl('^asr_scr_depress_',colnames(FAM.dat.clean))]

# Reorganise columns and save files ---------------------------------------
FAM.dat.clean=FAM.dat.clean[,c(1:12,113:119,128:137,120:122)]

FAM.dat.clean=filter(FAM.dat.clean,keep.NOsud==1)
saveRDS(FAM.dat.clean,file='data/FamHistory_clean/FAM.dat_AllSubjects_clean.rds')
### unrelated subs
FAM.dat.unrelated=FAM.dat.clean[!duplicated(FAM.dat.clean$rel_family_id),]
saveRDS(FAM.dat.unrelated,file='data/FamHistory_clean//FAM.dat_Unrelated_clean.rds')
### twin pairs
onetwin.ID = !FAM.dat$src_subject_id %in% FAM.dat.unrelated$src_subject_id
onetwin.fam.ID = FAM.dat$rel_family_id[onetwin.ID]
loc.withonetwin = FAM.dat$rel_family_id %in% onetwin.fam.ID
FAM.dat.twins=FAM.dat[loc.withonetwin,]
saveRDS(FAM.dat.twins,file='data/FamHistory_clean/FAM.dat_TwTri_clean.rds')
