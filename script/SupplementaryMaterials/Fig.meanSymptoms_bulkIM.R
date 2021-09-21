
setwd("/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain")

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(cowplot)
library(shape2)

# Settings
load('result/x.Supplementary_materials/meanYouthDepree_WM_subcor_FS_noSocialCov.RData')

targetdata = result.YouthDepre.bulk
Fig.title = ' '

# Tidy up labels in targetdata for illustration purpose
targetdata$dependent = gsub('bl.mean.thk.APARC','Cortical thickness',targetdata$dependent)
targetdata$dependent = gsub('bl.mean.sulc.APARC','Sulcal depth',targetdata$dependent)
targetdata$dependent = gsub('bl.total.sa.APARC','Surface area',targetdata$dependent)
targetdata$dependent = gsub('bl.total.vol.APARC','Cortical volume',targetdata$dependent)
targetdata$dependent = gsub('bl.vol.ASEG.wholeb','Whole-brain volume',targetdata$dependent)
targetdata$dependent = gsub('bl.total.dtiFA.FiberAtlas.fibers','FA',targetdata$dependent)
targetdata$dependent = gsub('bl.total.dtiMD.FiberAtlas.fibers','MD',targetdata$dependent)

targetdata$sig = 99999
targetdata$sig[targetdata$p.value<0.05]='*'
targetdata$sig[targetdata$sig==99999]=''


targetdata.cbcl = targetdata

# reorder based on category and beta
targetdata.cbcl = targetdata.cbcl[order(targetdata.cbcl$beta),]
targetdata.cbcl = rbind(targetdata.cbcl[grep('APARC',targetdata.cbcl$mod_name),],
                        targetdata.cbcl[grep('ASEG',targetdata.cbcl$mod_name),],
                        targetdata.cbcl[grep('FiberAtlas',targetdata.cbcl$mod_name),])

# add category information
targetdata.cbcl$category = 99999
targetdata.cbcl$category[grep('APARC',targetdata.cbcl$mod_name)]='Cortical measure'
targetdata.cbcl$category[grep('ASEG',targetdata.cbcl$mod_name)]='Subcortical measure'
targetdata.cbcl$category[grep('FiberAtlas',targetdata.cbcl$mod_name)]='White-matter measure'
targetdata.cbcl$category = factor(targetdata.cbcl$category,
                                  levels=c('Cortical measure','Subcortical measure',
                                           'White-matter measure'))
targetdata.cbcl$ord = 1:nrow(targetdata.cbcl)

# a figure for depressive symptoms
dodge <- position_dodge(width = 5)
dat.depre=targetdata.cbcl

fig.depre=
  ggplot(dat.depre, aes(x=reorder(dependent,-ord), y=beta)) + 
  geom_bar(fill='peru',  position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=Lower_95CI, ymax=Upper_95CI), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  ggtitle("Average of caregiver and child reports\nfor severity of depression")+
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
  ylim(c(-0.07,0.05))+
  #scale_y_reverse()+
  geom_hline(yintercept=0,size=0.5,color='grey')+
  geom_vline(xintercept=2.5,size=0.5,color='grey', linetype = "dashed")+
  coord_flip()

png("Figs/SuppleInfo/meanDepsymptoms_depre_IM_bulk.png", width = 4, height = 3, units = 'in', res = 300)
fig.depre # Make plot
dev.off()