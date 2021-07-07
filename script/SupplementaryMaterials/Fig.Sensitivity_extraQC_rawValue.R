# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
library('reshape2')
library('ggplot2')
setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# Freesufer and subcortical volumes
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
QC.file=IM.dat[,c('fsqc_qu_motion','fsqc_qu_pialover','fsqc_qu_wmunder','fsqc_qu_inhomogeneity','fsqc_qu_artifact')]
QC.file$fail_issues_N=rowSums(QC.file>1,na.rm=T)
IM.dat$QC.FS=QC.file$fail_issues_N
IM.dat$QC.FS[IM.dat$QC.FS>0]=1
ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG|ICV|QC.FS',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
tab.data=merge(IM.dat,FAM.dat,by='src_subject_id',all.x=T)
# white matter FA and MD
IM.dat=readRDS('data/IMdat/DTI.dat.QCed_short.rds')
QC.file=IM.dat[,c('dti_postqc_quregt1','dti_postqc_qub0warp','dti_postqc_quimgqual','dti_postqc_qucutoff','dti_postqc_qufseg')]
QC.file$fail_issues_N=rowSums(QC.file>1,na.rm=T)
IM.dat$QC.wm=QC.file$fail_issues_N
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiFA.FiberAtlas.fibers))<5)
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiMD.FiberAtlas.fibers))<5)
IM.dat$QC.wm[IM.dat$QC.wm>0]=1
ls.covs=c('iqc_dmri_fa_qc','iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('dtiFA|dtiMD|QC.wm',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]


tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)


# Data for figure ---------------------------------------------------------
targetdata = tab.data
colnames(targetdata)[1]='f.eid'

ls.bulk.measure=c('bl.mean.sulc.APARC','bl.mean.thk.APARC',
                  'bl.total.sa.APARC','bl.total.vol.APARC',
                  'bl.total.dtiFA.FiberAtlas.fibers',
                  'bl.total.dtiMD.FiberAtlas.fibers')
ls.QC=c('QC.FS','QC.wm')
targetdata = targetdata[,c('f.eid',ls.bulk.measure,ls.QC)]
targetdata[,ls.bulk.measure]=scale(targetdata[,ls.bulk.measure])

fig.dat=melt(data = targetdata, 
             id.vars = "f.eid", 
             measure.vars = ls.bulk.measure)
colnames(fig.dat)=c('f.eid','IM_Measure','Value')

fig.dat$IM_Measure=gsub('bl.mean.sulc.APARC','Cortical sulcal depth',fig.dat$IM_Measure)
fig.dat$IM_Measure=gsub('bl.mean.thk.APARC','Cortical thickness',fig.dat$IM_Measure)
fig.dat$IM_Measure=gsub('bl.total.sa.APARC','Cortical surface area',fig.dat$IM_Measure)
fig.dat$IM_Measure=gsub('bl.total.vol.APARC','Cortical volume',fig.dat$IM_Measure)
fig.dat$IM_Measure=gsub('bl.total.dtiFA.FiberAtlas.fibers','White matter FA',fig.dat$IM_Measure)
fig.dat$IM_Measure=gsub('bl.total.dtiMD.FiberAtlas.fibers','White matter MD',fig.dat$IM_Measure)


fig.dat=merge(fig.dat,targetdata[,c('f.eid',ls.QC)],by='f.eid',all.x=T)

fig.dat$FS.label='Kept'
fig.dat$FS.label[fig.dat$QC.FS==1]='Removed due to\nmore stringent QC'
fig.dat$FS.label=as.factor(fig.dat$FS.label)

fig.dat$WM.label='Kept'
fig.dat$WM.label[fig.dat$QC.wm==1]='Removed due to\nmore stringent QC'
fig.dat$WM.label=as.factor(fig.dat$WM.label)



# Scatter plot ------------------------------------------------------------
# FS Measures
fig.dat.1=fig.dat[grep('sulcal depth|thickness|surface area|volume',fig.dat$IM_Measure),]
fig.FS=ggplot(fig.dat.1, aes(x=FS.label, y=Value)) +
  geom_boxplot(outlier.colour="orangered2", outlier.shape=1,
               outlier.size=0.2)+
  facet_grid(cols = vars(IM_Measure))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=6,face='bold'),
        axis.ticks.x=element_blank(),
        legend.position = 'none',
        axis.line = element_line(colour = "grey"),
        axis.line.x = element_blank(),
        plot.title=element_text(lineheight = 1,face='bold'))+
  ylab('Standardised value')

tiff("Figs/SuppleInfo/ExtraQC_rawFSMeasure.tiff", width = 11, height = 4, units = 'in', res = 300)
fig.FS # Make plot
dev.off()


# WM Measures
fig.dat.2=fig.dat[grep('White matter',fig.dat$IM_Measure),]
fig.wm=ggplot(fig.dat.2, aes(x=WM.label, y=Value)) +
  geom_boxplot(outlier.colour="orangered2", outlier.shape=1,
               outlier.size=0.2)+
  facet_grid(cols = vars(IM_Measure))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=6,face='bold'),
        axis.ticks.x=element_blank(),
        legend.position = 'none',
        axis.line = element_line(colour = "grey"),
        axis.line.x = element_blank(),
        plot.title=element_text(lineheight = 1,face='bold'))+
  ylab('Standardised value')

tiff("Figs/SuppleInfo/ExtraQC_rawWMMeasure.tiff", width = 5, height = 3, units = 'in', res = 300)
fig.wm # Make plot
dev.off()