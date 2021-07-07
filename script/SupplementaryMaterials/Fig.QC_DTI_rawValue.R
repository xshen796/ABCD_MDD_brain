# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
library('reshape2')
library('ggplot2')
setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')

# Load data ---------------------------------------------------------------
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# Freesufer and subcortical volumes
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')

ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG|ICV|QC.FS',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
tab.data=merge(IM.dat,FAM.dat,by='src_subject_id',all.x=T)
# white matter FA and MD
IM.dat=readRDS('data/IMdat/DTI.dat.QCed_NoPostProc.rds')
IM.dat=filter(IM.dat,!is.na(dti_postqc_qc))
IM.dat[(abs(scale(IM.dat$bl.total.dtiFA.FiberAtlas.fibers))>5)&!is.na(IM.dat$bl.total.dtiFA.FiberAtlas.fibers),grep('dtiFA',colnames(IM.dat))]=NA
IM.dat[(abs(scale(IM.dat$bl.total.dtiMD.FiberAtlas.fibers))>5)&!is.na(IM.dat$bl.total.dtiMD.FiberAtlas.fibers),grep('dtiMD',colnames(IM.dat))]=NA

tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)


# Data for figure ---------------------------------------------------------
targetdata = tab.data
colnames(targetdata)[1]='f.eid'

ls.bulk.measure=c('bl.total.dtiFA.FiberAtlas.fibers',
                  'bl.total.dtiMD.FiberAtlas.fibers')
ls.QC=c('dti_postqc_qc')
targetdata = targetdata[,c('f.eid',ls.bulk.measure,ls.QC)]
targetdata[,ls.bulk.measure]=scale(targetdata[,ls.bulk.measure])

fig.dat=melt(data = targetdata, 
             id.vars = "f.eid", 
             measure.vars = ls.bulk.measure)
colnames(fig.dat)=c('f.eid','IM_Measure','Value')

fig.dat$IM_Measure=gsub('bl.total.dtiFA.FiberAtlas.fibers','White matter FA',fig.dat$IM_Measure)
fig.dat$IM_Measure=gsub('bl.total.dtiMD.FiberAtlas.fibers','White matter MD',fig.dat$IM_Measure)


fig.dat=merge(fig.dat,targetdata[,c('f.eid',ls.QC)],by='f.eid',all.x=T)


fig.dat$WM.label='Kept'
fig.dat$WM.label[fig.dat$dti_postqc_qc==1]='Removed due to QC'
fig.dat$WM.label=as.factor(fig.dat$WM.label)



# Scatter plot ------------------------------------------------------------


# WM Measures
fig.dat.2=fig.dat[grep('White matter',fig.dat$IM_Measure),]
fig.wm.box=ggplot(fig.dat.2, aes(x=WM.label, y=Value)) +
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


# Histogram ---------------------------------------------------------------

IM.dat=readRDS('data/IMdat/DTI.dat.QCed_short.rds')
ls.covs=c('iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('dtiFA|dtiMD',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
colnames(IM.dat)[1]='f.eid'
# Raw value

ls.bulk.measure=c('bl.total.dtiFA.FiberAtlas.fibers',
                  'bl.total.dtiMD.FiberAtlas.fibers')
targetdata = IM.dat[,c('f.eid',ls.bulk.measure)]
targetdata[,ls.bulk.measure]=scale(targetdata[,ls.bulk.measure])

fig.dat=melt(data = targetdata, 
             id.vars = "f.eid", 
             measure.vars = ls.bulk.measure)
colnames(fig.dat)=c('f.eid','Measure','Score')


fig.dat$Measure=gsub('bl.total.dtiFA.FiberAtlas.fibers','White matter FA',fig.dat$Measure)
fig.dat$Measure=gsub('bl.total.dtiMD.FiberAtlas.fibers','White matter MD',fig.dat$Measure)


fig.raw=ggplot(fig.dat, aes(x=Score, y=..scaled.., fill = Measure, colour = Measure)) +
  geom_density(alpha = 0.1, adjust = 2) +
  #xlim(0,13)+
  ylab("Density")+
  theme(axis.text=element_text(size=8),
        strip.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text=element_text(size=10),
        axis.line = element_blank())



# Outlier removed
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiFA.FiberAtlas.fibers))<5)
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiMD.FiberAtlas.fibers))<5)

targetdata = IM.dat[,c('f.eid',ls.bulk.measure)]
targetdata[,ls.bulk.measure]=scale(targetdata[,ls.bulk.measure])

fig.dat.2=melt(data = targetdata, 
               id.vars = "f.eid", 
               measure.vars = ls.bulk.measure)
colnames(fig.dat.2)=c('f.eid','Measure','Score')


fig.dat.2$Measure=gsub('bl.total.dtiFA.FiberAtlas.fibers','White matter FA',fig.dat.2$Measure)
fig.dat.2$Measure=gsub('bl.total.dtiMD.FiberAtlas.fibers','White matter MD',fig.dat.2$Measure)


fig.outl.removed=ggplot(fig.dat.2, aes(x=Score, y=..scaled.., fill = Measure, colour = Measure)) +
  geom_density(alpha = 0.1, adjust = 2) +
  #xlim(0,13)+
  ylab("Density")+
  theme(axis.text=element_text(size=8),
        strip.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text=element_text(size=10),
        axis.line = element_blank())

fig.wm.hist=ggarrange(fig.raw,fig.outl.removed,
                      nrow = 1, ncol=2, align='h',common.legend = T)

# Combined the two --------------------------------------------------------

fig.total = ggarrange(fig.wm.box,fig.wm.hist,labels=c('a','b'),nrow=2)

png("Figs/SuppleInfo/QC_WMMeasure_boxNhist.png", width = 5, height = 5.5, units = 'in', res = 300)
fig.total # Make plot
dev.off()
