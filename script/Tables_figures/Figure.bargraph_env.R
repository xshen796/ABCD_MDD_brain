library('ggplot2')
library('Hmisc')
library('ggpubr')
library('dplyr')

setwd("Z:/Documents/ActiveProject/ABCD_XShen/Adolescent_MDD_brain/")
load('result/i.Main_result/Diffs_YouthDepression_envVariable.RData')
fig.result=filter(result,p.corrected<0.05)


fig.result=fig.result[order(fig.result$beta,decreasing = F),]
fig.result$ord=1:nrow(fig.result)

fig.result$description[grep('^Parent:',fig.result$description)]=
  paste0(fig.result$description[grep('^Parent:',fig.result$description)],'*')
fig.result$description=gsub('Parent:','',fig.result$description)
fig.result$description=gsub('Child:','',fig.result$description)

dodge <- position_dodge(width = 5)

# colors of each subset
dat.IM=fig.result
fig.IM=
  ggplot(dat.IM, aes(x=reorder(description,ord), y=beta)) + 
  geom_bar(fill='dodgerblue2',  position=position_dodge(), stat="identity", width=0.8) +
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
  scale_x_discrete(position='top')+
  geom_hline(yintercept=0,color = "black", size=0.3)+
  geom_vline(xintercept=7.5,color = "grey",linetype='dashed', size=1)+
  coord_flip()


fig.IM=
  ggplot(dat.IM, aes(x=reorder(description,ord), y=beta)) + 
  geom_point(colour='tomato2',  position=position_dodge(width = 0.1), stat="identity", size=2) +
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
  scale_x_discrete(position='top')+
  geom_hline(yintercept=0,color = "black", size=0.3)+
  geom_vline(xintercept=7.5,color = "grey",linetype='dashed', size=0.3)+
  coord_flip()


# make a plot file
tiff("Figs/Behav/EnvVar_DiffYouthDepre.tiff", width = 9, height = 3, units = 'in', res = 300)
fig.IM # Make plot
dev.off()
