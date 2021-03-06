
setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/ABCD_MDD_brain/')

# Dot plot ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(cowplot)

# Settings
load('result/x.Supplementary_materials/YouthDepree_WM_subcor_FS_covKSADSotherConditions.RData')

targetdata = result.YouthDepre.bulk[!grepl('^CBCL\\.',result.YouthDepre.bulk$factor),]
Fig.title = ' '

# Tidy up labels in targetdata for illustration purpose
targetdata$dependent = gsub('bl.mean.thk.APARC','Cortical thickness',targetdata$dependent)
targetdata$dependent = gsub('bl.mean.sulc.APARC','Sulcal depth',targetdata$dependent)
targetdata$dependent = gsub('bl.total.sa.APARC','Surface area',targetdata$dependent)
targetdata$dependent = gsub('bl.total.vol.APARC','Cortical volume',targetdata$dependent)
targetdata$dependent = gsub('bl.vol.ASEG.wholeb','Whole-brain volume',targetdata$dependent)
targetdata$dependent = gsub('bl.total.dtiFA.FiberAtlas.fibers','FA',targetdata$dependent)
targetdata$dependent = gsub('bl.total.dtiMD.FiberAtlas.fibers','MD',targetdata$dependent)
targetdata$factor = gsub('KSADS\\.','',targetdata$factor)
targetdata$factor = gsub('_ever\\.p','',targetdata$factor)
targetdata$factor = gsub('_ever\\.y','',targetdata$factor)
targetdata$factor = gsub('\\.p','',targetdata$factor)
targetdata$factor = gsub('\\.y','',targetdata$factor)
targetdata$factor = gsub('_',' ',targetdata$factor)

targetdata$Condition = factor(targetdata$factor)
targetdata$sig = 99999
targetdata$sig[targetdata$p.value<0.05]='*'
targetdata$sig[targetdata$sig==99999]=''


# Parental reports --------------------------------------------------------

targetdata.p = targetdata[grep('\\.p$',targetdata$mod_name),]
targetdata.p.depre = filter(targetdata.p,factor=='Depressive symptoms')

# reorder based on category and beta
targetdata.p.depre = targetdata.p.depre[order(targetdata.p.depre$beta),]
targetdata.p.depre = rbind(targetdata.p.depre[grep('APARC',targetdata.p.depre$mod_name),],
                           targetdata.p.depre[grep('ASEG',targetdata.p.depre$mod_name),],
                           targetdata.p.depre[grep('FiberAtlas',targetdata.p.depre$mod_name),])

# add category information
targetdata.p.depre$category = 99999
targetdata.p.depre$category[grep('APARC',targetdata.p.depre$mod_name)]='Cortical measure'
targetdata.p.depre$category[grep('ASEG',targetdata.p.depre$mod_name)]='Subcortical measure'
targetdata.p.depre$category[grep('FiberAtlas',targetdata.p.depre$mod_name)]='White-matter measure'
targetdata.p.depre$category = factor(targetdata.p.depre$category,
                                     levels=c('Cortical measure','Subcortical measure',
                                              'White-matter measure'))
targetdata.p.depre$ord = 1:nrow(targetdata.p.depre)

targetdata.p.MDD = filter(targetdata.p,factor=='MDD')
targetdata.p.MDD = merge(targetdata.p.MDD,targetdata.p.depre[,c('dependent','category','ord')],
                         by='dependent')

# a figure for depressive symptoms
dodge <- position_dodge(width = 5)
dat.depre=targetdata.p.depre

fig.depre=
  ggplot(dat.depre, aes(x=reorder(dependent,-ord), y=beta)) + 
  geom_bar(fill='darkgoldenrod1',  position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=Lower_95CI, ymax=Upper_95CI), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  ggtitle("Depressive symptoms (reported by caregivers)")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(hjust=0.5),
    axis.line.x = element_line(size=0.3),
    axis.text=element_text(size=8), axis.title=element_text(size=8),
    plot.title = element_text(lineheight=1, face="bold", vjust=1, hjust=0.5,size=9),
    strip.text = element_text(size=8),
    plot.margin=unit(c(1,1,1,3),'mm')) +
  geom_text(aes(label=sig), colour="black", hjust=13, size=3)+
  ylab("Beta") + xlab("\n\n") +
  ylim(c(-0.065,0.05))+
  #scale_y_reverse()+
  scale_x_discrete(position='top')+
  geom_hline(yintercept=0,size=0.5,color='grey')+
  geom_vline(xintercept=2.5,size=0.5,color='grey', linetype = "dashed")+
  coord_flip()

# MDD graph
dat.mdd=targetdata.p.MDD

fig.mdd=
  ggplot(dat.mdd, aes(x=reorder(dependent,-ord), y=beta)) + 
  geom_bar(fill='indianred1',  position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=Lower_95CI, ymax=Upper_95CI), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  ggtitle("MDD (reported by caregivers)")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(size=0.5),
    axis.text=element_text(size=8), axis.title=element_text(size=8),
    plot.title = element_text(lineheight=1, face="bold", vjust=1, hjust=0.5,size=9),
    strip.text = element_text(size=8),
    plot.margin=unit(c(1,1,1,3),'mm')) +
  geom_text(aes(label=sig), colour="black", hjust=13, size=3)+
  ylab("Cohen's d") + xlab("\n\n") +
  scale_x_discrete(position='top')+
  ylim(c(-0.065,0.05))+
  geom_hline(yintercept=0,size=0.5,color='grey')+
  geom_vline(xintercept=2.5,size=0.5,color='grey', linetype = "dashed")+
  #geom_vline(xintercept=3.5,size=0.5,color='grey', linetype = "dashed")+
  #geom_hline(yintercept=-log10(0.05),size=0.5,color='red', linetype = "dashed")+
  coord_flip()



# concatenate all figs
Fig.total.p=ggarrange(fig.depre,fig.mdd,
                      widths = c(1.3,1),
                      nrow = 1, ncol=2, align='h')


# Total figure ------------------------------------------------------------

png("Figs/SuppleInfo/covComorbid_pANDy_report_depre_IM_bulk.png", 
    width = 8, height = 3, units = 'in', res = 300)
Fig.total.p # Make plot
dev.off()
