# Basic setups ------------------------------------------------------------

library('dplyr')
library('pbapply')
library('nlme')
library('ggpubr')
setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')


# Freesurfer data ---------------------------------------------------------

IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion','iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG|ICV',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
colnames(IM.dat)[1]='f.eid'
ls.bulk.measure=c('bl.mean.sulc.APARC','bl.mean.thk.APARC',
                  'bl.total.sa.APARC','bl.total.vol.APARC')
targetdata = IM.dat[,c('f.eid',ls.bulk.measure)]
targetdata[,ls.bulk.measure]=scale(targetdata[,ls.bulk.measure])

fig.dat=melt(data = targetdata, 
             id.vars = "f.eid", 
             measure.vars = ls.bulk.measure)
colnames(fig.dat)=c('f.eid','Measure','Score')

fig.dat$Measure=gsub('bl.mean.sulc.APARC','Cortical sulcal depth',fig.dat$Measure)
fig.dat$Measure=gsub('bl.mean.thk.APARC','Cortical thickness',fig.dat$Measure)
fig.dat$Measure=gsub('bl.total.sa.APARC','Cortical surface area',fig.dat$Measure)
fig.dat$Measure=gsub('bl.total.vol.APARC','Cortical volume',fig.dat$Measure)
fig.dat$Measure=gsub('bl.total.dtiFA.FiberAtlas.fibers','White matter FA',fig.dat$Measure)
fig.dat$Measure=gsub('bl.total.dtiMD.FiberAtlas.fibers','White matter MD',fig.dat$Measure)


fig=ggplot(fig.dat, aes(x=Score, y=..scaled.., fill = Measure, colour = Measure)) +
  geom_density(alpha = 0.1, adjust = 2) +
  #xlim(0,13)+
  ylab("Density")+
  theme(axis.text=element_text(size=8),
        strip.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text=element_text(size=10),
        axis.line = element_blank())

tiff(file="Figs/SuppleInfo/Cortical_Measure_distribution.tiff",
     width=18, height=10, units='cm', res=300)
fig
dev.off()



# white matter FA and MD --------------------------------------------------

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

tiff(file="Figs/SuppleInfo/WM_Measure_distribution.tiff",
     width=22, height=10, units='cm', res=300)
fig.wm
dev.off()
