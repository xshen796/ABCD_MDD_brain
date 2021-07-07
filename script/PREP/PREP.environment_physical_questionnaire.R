library(dplyr)

setwd('/exports/igmm/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/shen/SData/ABCD/release2.0.1/iii.data'

# merge and save the master file  ----------------------------------------------------------------------

dir.var.ls='data/ls.culture_environment_physical.csv'
ls.var=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.var$Category,'/',ls.var$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      ## follow-up track file have duplicated IDs. Only keep the baseline data. 
      if (length(unique(tmp.file$src_subject_id))<length(tmp.file$src_subject_id)){
            tmp.file = tmp.file[order(tmp.file$interview_date),]
            tmp.file = tmp.file[!duplicated(tmp.file$src_subject_id),]}
      
      if (i==ls.files.tocatch[1]){questn.dat=tmp.file}else{
                  cols.keep=!grepl('interview_date|interview_age|sex|gender|eventname',colnames(tmp.file))
                  tmp.file=tmp.file[,cols.keep]
                  questn.dat=merge(questn.dat,tmp.file,by='src_subject_id',all=T)                  
            }
}


# grep('\\.x$',colnames(IM.dat))  # check if there is any duplicated colnames
rm(i,tmp.file)


targetdata=questn.dat

# Tidy up data with excessive NAs  ---------------------------------------------------------------------
ls.nm=colnames(targetdata)[grep('_nm$',colnames(targetdata))]
ls.nt=colnames(targetdata)[grep('_nt$',colnames(targetdata))]
ls.grep.item=gsub('_nm','',ls.nm)

qc.dat.nm=targetdata[,ls.nm]
qc.dat.nt=targetdata[,ls.nt]
qc.dat=qc.dat.nm/qc.dat.nt
colnames(qc.dat)=ls.grep.item

culture_env_physical_cleandat=targetdata[,!grepl('_nm$|_nt$|_na$',colnames(targetdata))]

for (i in 1:ncol(qc.dat)){
  qc.kw=colnames(qc.dat)[i]
  tmp.qc.vec=qc.dat[,i]
  tmp.qc.vec[is.na(tmp.qc.vec)]=0
  
  culture_env_physical_cleandat[tmp.qc.vec>0.5,grep(qc.kw,colnames(culture_env_physical_cleandat))]=NA
}

# Remove additional repeated columns
culture_env_physical_cleandat=culture_env_physical_cleandat[,!grepl('^fes_p_ss_fc$|^psb_p_ss_answered$|^fes_y_ss_fc$',colnames(culture_env_physical_cleandat))]
#write.table(colnames(culture_env_physical_cleandat),sep='\n',file='data/ls.culture_env_physical_TestName.txt',quote=F,col.name=F,row.name=F)

ls.to.keep=read.table('data/ls.culture_env_physical_TestName.txt',header=T,sep=',',stringsAsFactors=F)
culture_env_physical_cleandat=culture_env_physical_cleandat[,c('src_subject_id','interview_date','interview_age','gender',ls.to.keep$short_name)]

# Combine male and female pubertal development  ---------------------------------------------------------------------
culture_env_physical_cleandat$pds_p_ss_category=rowSums(culture_env_physical_cleandat[,c('pds_p_ss_male_category','pds_p_ss_female_category')],na.rm=T)
culture_env_physical_cleandat$pds_p_ss_category[rowSums(is.na(culture_env_physical_cleandat[,c('pds_p_ss_male_category','pds_p_ss_female_category')]))==2]=NA


saveRDS(culture_env_physical_cleandat,file='data/culture_env_physical.rds')