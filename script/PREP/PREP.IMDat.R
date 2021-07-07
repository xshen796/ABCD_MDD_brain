######
# by Shen
library(dplyr)

setwd('/exports/igmm/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/shen/SData/ABCD/release2.0.1/iii.data'

# merge, QC and save the master file (FreeSurfer)---------------------------------------------------
dir.var.ls='data/ls.Freesurfer_vars.csv'
ls.IMvar=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.IMvar$Category,'/',ls.IMvar$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
      for (i in ls.files.tocatch){
            tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
            if (i==ls.files.tocatch[1]){IM.dat=tmp.file}else{
                  cols.omit=max(grep('interview_date|interview_age|sex|gender|eventname',colnames(tmp.file)))
                  tmp.file=tmp.file[,c(1,(cols.omit+1):ncol(tmp.file))]
                  IM.dat=merge(IM.dat,tmp.file,by='src_subject_id',all=T)
            }
      }
# grep('\\.x$',colnames(IM.dat))  # check if there has been any duplicated colnames
rm(i,tmp.file,cols.omit)

# remove replicated whole brain measures
var.ls=ls.IMvar$Field.name
IM.dat=IM.dat[,var.ls]
colnames(IM.dat)=gsub('smri_','',colnames(IM.dat))
colnames(IM.dat)=gsub('_cdk_','.APARC.',colnames(IM.dat))
colnames(IM.dat)=gsub('_scs_','.ASEG.',colnames(IM.dat))
colnames(IM.dat)=gsub('thick.','thk.',colnames(IM.dat))
colnames(IM.dat)=gsub('area.','sa.',colnames(IM.dat))

# create ICV and remove wholeb, csf
IM.dat$ICV_derived = IM.dat$vol.ASEG.wholeb+IM.dat$vol.ASEG.csf
IM.dat$ICV_ASEG =IM.dat$vol.ASEG.intracranialv
IM.dat=IM.dat[,!grepl('vol.ASEG.wholeb|vol.ASEG.csf|intracranialv',colnames(IM.dat))]

colnames(IM.dat)[grep('lh$',colnames(IM.dat))]=paste0('lh.',gsub('lh$','',colnames(IM.dat)))[grep('lh$',colnames(IM.dat))]
colnames(IM.dat)[grep('rh$',colnames(IM.dat))]=paste0('rh.',gsub('rh$','',colnames(IM.dat)))[grep('rh$',colnames(IM.dat))]
colnames(IM.dat)[grep('.mean$',colnames(IM.dat))]=paste0('bl.mean.',gsub('.mean$','',colnames(IM.dat)))[grep('.mean$',colnames(IM.dat))]
colnames(IM.dat)[grep('.total$',colnames(IM.dat))]=paste0('bl.total.',gsub('.total$','',colnames(IM.dat)))[grep('.total$',colnames(IM.dat))]
colnames(IM.dat)[grep('^vol.',colnames(IM.dat))]=paste0('bl.',colnames(IM.dat))[grep('^vol.',colnames(IM.dat))]

# Some other QC methods: /sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release1.1/FamillialRisk_MDD/script/PREP/PREP.FreesurferDat.R
IM.dat.QCed=filter(IM.dat,fsqc_qc==1,iqc_t1_ok_ser>0)
saveRDS(IM.dat.QCed,file='data/IMdat/FreeSurfer.dat.QCed.rds')





# merge, QC and save the master file (DTI) --------------------------------

dir.var.ls='data/ls.DTI_vars.csv'
ls.IMvar=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.IMvar$Category,'/',ls.IMvar$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      if (i==ls.files.tocatch[1]){IM.dat=tmp.file}else{
            cols.omit=max(grep('interview_date|interview_age|sex|gender|eventname',colnames(tmp.file)))
            tmp.file=tmp.file[,c(1,(cols.omit+1):ncol(tmp.file))]
            IM.dat=merge(IM.dat,tmp.file,by='src_subject_id',all=T)
      }
}
# grep('\\.x$',colnames(IM.dat))  # check if there has been any duplicated colnames
rm(i,tmp.file,cols.omit)

IM.dat=IM.dat[,ls.IMvar$Field.name]
colnames(IM.dat)=gsub('^dmri_','',colnames(IM.dat))
colnames(IM.dat)=gsub('^dtifa_','dtiFA.',colnames(IM.dat))
colnames(IM.dat)=gsub('^dtimd_','dtiMD.',colnames(IM.dat))
colnames(IM.dat)=gsub('^dtild_','dtiLD.',colnames(IM.dat))
colnames(IM.dat)=gsub('^dtitd_','dtiTD.',colnames(IM.dat))
colnames(IM.dat)=gsub('^dtivol_','dtiVol.',colnames(IM.dat))
colnames(IM.dat)=gsub('fiberat_','FiberAtlas.',colnames(IM.dat))


colnames(IM.dat)[grep('\\.all&rh$',colnames(IM.dat))]=
      paste0('rhtotal.',gsub('\\.all|rh$','',colnames(IM.dat)))[grep('\\.all|rh$',colnames(IM.dat))]

colnames(IM.dat)[grep('lh$',colnames(IM.dat))]=paste0('lh.',gsub('lh$','',colnames(IM.dat)))[grep('lh$',colnames(IM.dat))]
colnames(IM.dat)[grep('rh$',colnames(IM.dat))]=paste0('rh.',gsub('rh$','',colnames(IM.dat)))[grep('rh$',colnames(IM.dat))]
colnames(IM.dat)[grep('\\.fmaj$|\\.fmin$|\\.cc$',colnames(IM.dat))]=
      paste0('bl.',colnames(IM.dat)[grep('\\.fmaj$|\\.fmin$|\\.cc$',colnames(IM.dat))])
colnames(IM.dat)[grepl('\\.all',colnames(IM.dat))]=
      paste0('total.',gsub('all','',colnames(IM.dat)))[grep('\\.all',colnames(IM.dat))]
colnames(IM.dat)[grep('^total.lh',colnames(IM.dat))]=paste0('totallh',gsub('^total.lh','',colnames(IM.dat)))[grep('^total.lh',colnames(IM.dat))]
colnames(IM.dat)[grep('^total.rh',colnames(IM.dat))]=paste0('totalrh',gsub('^total.rh','',colnames(IM.dat)))[grep('^total.rh',colnames(IM.dat))]
colnames(IM.dat)[grep('^total\\.',colnames(IM.dat))]=paste0('bl.total',gsub('^total','',colnames(IM.dat)))[grep('^total\\.',colnames(IM.dat))]

# save files 
# Some other QC methods: /sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release1.1/FamillialRisk_MDD/script/PREP/PREP.FreesurferDat.R

### all dat
IM.dat.QCed=filter(IM.dat,iqc_dmri_ok_ser>0,iqc_t1_ok_ser>0,fsqc_qc==1,(dti_postqc_qc==1)|is.na(dti_postqc_qc))
IM.dat.QCed_NOpostproc=filter(IM.dat,iqc_dmri_ok_ser>0,iqc_t1_ok_ser>0,fsqc_qc==1)
saveRDS(IM.dat.QCed,file='data/IMdat/DTI.dat.QCed.rds')
saveRDS(IM.dat.QCed_NOpostproc,file='data/IMdat/DTI.dat.QCed_NoPostProc.rds')

### short data list (less variables)
ls.rm=ls.IMvar[grep('cortex only|excluding fimbria$|without corpus callosum$',ls.IMvar$Description),]
IM.dat.QCed=IM.dat.QCed[,!grepl('cortex only|excluding fimbria$|without corpus callosum$',ls.IMvar$Description)]
saveRDS(IM.dat.QCed,file='data/IMdat/DTI.dat.QCed_short.rds')





# merge, QC and save the master file (rsfMRI) -----------------------------
dir.var.ls='data/ls.rsfMRIvars.csv'
ls.IMvar=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.IMvar$Category,'/',ls.IMvar$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      if (i==ls.files.tocatch[1]){IM.dat=tmp.file}else{
            cols.omit=max(grep('interview_date|interview_age|sex|gender|eventname',colnames(tmp.file)))
            tmp.file=tmp.file[,c(1,(cols.omit+1):ncol(tmp.file))]
            IM.dat=merge(IM.dat,tmp.file,by='src_subject_id',all=T)
      }
}
# grep('\\.x$',colnames(IM.dat))  # check if there has been any duplicated colnames
rm(i,tmp.file,cols.omit)

IM.dat=IM.dat[,ls.IMvar$Field.name]
colnames(IM.dat)=gsub('rsfmri_c_ngd_','rs_conn.',colnames(IM.dat))
colnames(IM.dat)=gsub('rsfmri_var','rs_var',colnames(IM.dat))
colnames(IM.dat)=gsub('_ngd_','_',colnames(IM.dat))
colnames(IM.dat)=gsub('_cdk_','.APARC.',colnames(IM.dat))

colnames(IM.dat)[grep('lh$',colnames(IM.dat))]=paste0('lh.',gsub('lh$','',colnames(IM.dat)))[grep('lh$',colnames(IM.dat))]
colnames(IM.dat)[grep('rh$',colnames(IM.dat))]=paste0('rh.',gsub('rh$','',colnames(IM.dat)))[grep('rh$',colnames(IM.dat))]

loc.qcitems=grep(paste0('rs_conn.visitid|rs_conn.tr|rs_conn.numtrs|rs_conn.nvols|',
                      'rs_conn.stnvols|rs_conn.stcontignvols|rs_conn.ntpoints|rs_conn.meanmotion|',
                      'rs_conn.maxmotion|rs_conn.meantrans|rs_conn.maxtrans|rs_conn.meanrot|rs_conn.maxrot'),colnames(IM.dat))
colnames(IM.dat)[loc.qcitems]=paste0('qc_',colnames(IM.dat)[loc.qcitems])
      

# Some other QC methods: /sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release1.1/FamillialRisk_MDD/script/PREP/PREP.FreesurferDat.R
IM.dat.QCed=filter(IM.dat,fsqc_qc==1,qc_rs_conn.ntpoints>375)
keep= lapply(IM.dat.QCed[,20:ncol(IM.dat.QCed)], function(x) x < quantile(x,0.0025,na.rm=T) | x > quantile(x,0.9975,na.rm=T))
for(ncol in colnames(IM.dat.QCed)[20:ncol(IM.dat.QCed)]){
      expre=paste0('loc.rm=keep$',ncol)
      eval(parse(text=expre))
      loc.rm[is.na(loc.rm)]=F
      IM.dat.QCed[loc.rm,ncol]=NA
}
saveRDS(IM.dat.QCed,file='data/IMdat/rsfmri.dat.QCed.rds')

