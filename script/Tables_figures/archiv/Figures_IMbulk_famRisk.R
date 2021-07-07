
setwd("Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD")

# Dot plot ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(cowplot)

# Settings
load('result/i.Main_result/famRisk_noSocialCov.RData')

targetdata = result.famRisk.bulk[!grepl('^CBCL\\.',result.famRisk.bulk$factor),]
Fig.title = ' '

# Tidy up labels in targetdata for illustration purpose
targetdata$dependent = gsub('bl.mean.thk.APARC','Cortical thickness',targetdata$dependent)
targetdata$dependent = gsub('bl.mean.sulc.APARC','Sulcal depth',targetdata$dependent)
targetdata$dependent = gsub('bl.total.sa.APARC','Surface area',targetdata$dependent)
targetdata$dependent = gsub('bl.total.vol.APARC','Cortical volume',targetdata$dependent)
targetdata$dependent = gsub('bl.vol.ASEG.wholeb','Whole-brain volume',targetdata$dependent)
targetdata$dependent = gsub('bl.total.dtiFA.FiberAtlas.fibers','FA',targetdata$dependent)
targetdata$dependent = gsub('bl.total.dtiMD.FiberAtlas.fibers','MD',targetdata$dependent)
targetdata$factor = gsub('depression_bio','',targetdata$factor)
targetdata$factor = gsub('_noManiaVision','',targetdata$factor)
targetdata$factor = gsub('depression_parents','Parents',targetdata$factor)

targetdata$Condition = factor(targetdata$factor)
targetdata$sig = 99999
targetdata$sig[targetdata$p.value<0.05]='*'
targetdata$sig[targetdata$sig==99999]=''


# Mother
targetdata.m = targetdata[grep('bioMother',targetdata$mod_name),]

# reorder based on category and beta
targetdata.m = targetdata.m[order(targetdata.m$beta),]
targetdata.m = rbind(targetdata.m[grep('APARC',targetdata.m$mod_name),],
                           targetdata.m[grep('ASEG',targetdata.m$mod_name),],
                           targetdata.m[grep('FiberAtlas',targetdata.m$mod_name),])

# add category information
targetdata.m$category = 99999
targetdata.m$category[grep('APARC',targetdata.m$mod_name)]='Cortical measure'
targetdata.m$category[grep('ASEG',targetdata.m$mod_name)]='Subcortical measure'
targetdata.m$category[grep('FiberAtlas',targetdata.m$mod_name)]='White-matter measure'
targetdata.m$category = factor(targetdata.m$category,
                                     levels=c('Cortical measure','Subcortical measure',
                                              'White-matter measure'))
targetdata.m$ord = 1:nrow(targetdata.m)

targetdata.f = filter(targetdata,factor=='Father')
targetdata.f = merge(targetdata.f,targetdata.m[,c('dependent','category','ord')],
                         by='dependent')

targetdata.p = filter(targetdata,factor=='Parents')
targetdata.p = merge(targetdata.p,targetdata.m[,c('dependent','category','ord')],
                     by='dependent')


dodge <- position_dodge(width = 5)
dat.m=targetdata.m

fig.m=
  ggplot(dat.m, aes(x=reorder(dependent,-ord), y=beta)) + 
  geom_bar(fill="indianred1",  position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=beta-std, ymax=beta+std), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  ggtitle("Mother")+
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
  geom_text(aes(label=sig), colour="black", hjust=9, size=3)+
  ylab("Beta") + xlab("\n\n") +
  ylim(c(-0.06,0.05))+
  #scale_y_reverse()+
  #scale_x_discrete(position='top')+
  geom_hline(yintercept=0,size=0.5,color='grey')+
  geom_vline(xintercept=2.5,size=0.5,color='grey', linetype = "dashed")+
  coord_flip()

# Father
dat.f=targetdata.f

fig.f=
  ggplot(dat.f, aes(x=reorder(dependent,-ord), y=beta)) + 
  geom_bar(fill="#56B4E9",  position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=beta-std, ymax=beta+std), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  ggtitle("Father")+
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
  geom_text(aes(label=sig), colour="black", hjust=7, size=3)+
  ylab("Beta") + xlab("\n\n") +
  ylim(c(-0.06,0.05))+
  geom_hline(yintercept=0,size=0.5,color='grey')+
  geom_vline(xintercept=2.5,size=0.5,color='grey', linetype = "dashed")+
  #geom_vline(xintercept=3.5,size=0.5,color='grey', linetype = "dashed")+
  #geom_hline(yintercept=-log10(0.05),size=0.5,color='red', linetype = "dashed")+
  coord_flip()


# Father
dat.p=targetdata.p

fig.p=
  ggplot(dat.p, aes(x=reorder(dependent,-ord), y=beta)) + 
  geom_bar(fill="#999999",  position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=beta-std, ymax=beta+std), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  ggtitle("Parents")+
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
  geom_text(aes(label=sig), colour="black", hjust=7, size=3)+
  ylab("Beta") + xlab("\n\n") +
  scale_x_discrete(position='top')+
  ylim(c(-0.06,0.05))+
  geom_hline(yintercept=0,size=0.5,color='grey')+
  geom_vline(xintercept=2.5,size=0.5,color='grey', linetype = "dashed")+
  #geom_vline(xintercept=3.5,size=0.5,color='grey', linetype = "dashed")+
  #geom_hline(yintercept=-log10(0.05),size=0.5,color='red', linetype = "dashed")+
  coord_flip()



# concatenate all figs
Fig.total.p=ggarrange(fig.m,fig.f,fig.p,
                      widths = c(1.3,1,1),
                      nrow = 1, ncol=3, align='h')

tiff("Figs/famRisk_IM/famRisk_IM_bulk.tiff", width = 10, height = 3, units = 'in', res = 300)
Fig.total.p # Make plot
dev.off()
