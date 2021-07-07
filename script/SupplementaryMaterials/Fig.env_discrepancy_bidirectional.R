library('ggplot2')
library('Hmisc')
library('ggpubr')
library('dplyr')

setwd("Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD")
load('result/x.Supplementary_materials/bidirectDiffs_YouthDepression_envVariable.RData')
fig.result=filter(result,p.corrected<0.05)

fig.result=fig.result[order(fig.result$beta,decreasing = F),]
fig.result$ord=1:nrow(fig.result)

fig.result$description[grep('^Parent:',fig.result$description)]=
  paste0(fig.result$description[grep('^Parent:',fig.result$description)],'*')
fig.result$description=gsub('Parent:','',fig.result$description)
fig.result$description=gsub('Child:','',fig.result$description)

dodge <- position_dodge(width = 5)
fig.result.PoverY=filter(fig.result,factor=='KSADS.DS.p_over_y')
fig.result.YoverP=filter(fig.result,factor=='KSADS.DS.y_over_p')


# colors of each subset
dat.IM=fig.result.PoverY
fig.IM.PoverY=
  ggplot(dat.IM, aes(x=reorder(description,ord), y=beta)) + 
  geom_point(aes(colour = beta),  position=position_dodge(width = 0.1), stat="identity", size=2) +
  geom_errorbar(aes(ymin=beta-std, ymax=beta+std), width=.1,
                position=position_dodge(.9),colour="grey")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(size=0.5),
    axis.text=element_text(size=10), axis.title=element_text(size=11),
    plot.title = element_text(lineheight=1, face="bold", vjust=1, hjust=0.5,size=9),
    strip.text = element_text(size=8),
    plot.margin=unit(c(1,1,1,3),'mm')) +
  ylab("Standardised regression coefficientz\n\n\n") + xlab("\n\n") +
  #scale_y_continuous(limits=c(0,3))+
  #scale_y_reverse()+
  scale_colour_hp(option = "NewtScamander",name=' ')+
  scale_x_discrete(position='top')+
  geom_hline(yintercept=0,color = "black", size=0.3)+
  geom_vline(xintercept=6.5,color = "grey",linetype='dashed', size=1)+
  coord_flip()+
  ggtitle('Caregiver > Child')


dat.IM=fig.result.YoverP
fig.IM.YoverP=
  ggplot(dat.IM, aes(x=reorder(description,ord), y=beta)) + 
  geom_point(aes(colour = beta),  position=position_dodge(width = 0.1), stat="identity", size=2) +
  geom_errorbar(aes(ymin=beta-std, ymax=beta+std), width=.1,
                position=position_dodge(.9),colour="grey")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(size=0.5),
    axis.text=element_text(size=10), axis.title=element_text(size=11),
    plot.title = element_text(lineheight=1, face="bold", vjust=1, hjust=0.5,size=9),
    strip.text = element_text(size=8),
    plot.margin=unit(c(1,1,1,3),'mm')) +
  ylab("Standardised regression coefficient") + xlab("\n\n") +
  #scale_y_continuous(limits=c(0,3))+
  #scale_y_reverse()+
  scale_colour_hp(option = "NewtScamander",name='')+
  scale_x_discrete(position='top')+
  geom_hline(yintercept=0,color = "black", size=0.3)+
  geom_vline(xintercept=5.5,color = "grey",linetype='dashed', size=1)+
  coord_flip()+
  ggtitle('Child > Caregiver')

# make a plot file
sumPlot=ggarrange(fig.IM.PoverY,fig.IM.YoverP,
                  nrow = 2, ncol=1, align='v',
                  heights = c(1.25,1),
                  common.legend = T,
                  labels = c('a','b'),
                  legend = 'bottom')

tiff("Figs/SuppleInfo/EnvVar_bidirDiffYouthDepre.tiff", width = 9, height = 7, units = 'in', res = 300)
sumPlot # Make plot
dev.off()
