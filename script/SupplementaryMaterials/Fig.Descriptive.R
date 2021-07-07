# Descriptive stats
# By XShen
# 18/05/2020

# Settings ----------------------------------------------------------------
library('dplyr')
library('ggplot2')
library(reshape2)
library('plyr')
library(ggridges)
library(viridis)
library(hrbrthemes)
setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')

# Load data 
# fam data
FAM.dat=readRDS('data/FamHistory_clean/FAM.dat_Unrelated_clean.rds')
FAM.dat=FAM.dat[,c(1,6:ncol(FAM.dat))]
# behavioral data
behav.dat=readRDS('data/Behavioural/Behavioural.rds')
ls.covs=c('interview_age','recent_socialdprv','household_income','fam_highest_edu')#,'household_income','ADHD_ever'
tab.data=merge(FAM.dat,behav.dat,by='src_subject_id',all.x=T)
# Freesufer and subcortical volumes
IM.dat=readRDS('data/IMdat/FreeSurfer.dat.QCed.rds')
ls.covs=c('interview_age','sex','recent_socialdprv','household_income','fsqc_qc','fsqc_qu_motion','iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('\\.APARC|\\.ASEG|ICV',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)
# white matter FA and MD
IM.dat=readRDS('data/IMdat/DTI.dat.QCed_short.rds')
ls.covs=c('iqc_dmri_fa_qc')#,'household_income','ADHD_ever'
IM.dat=IM.dat[,c(1,grep('dtiFA|dtiMD',colnames(IM.dat)),grep(paste0(ls.covs,collapse='|'),colnames(IM.dat)))]
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiFA.FiberAtlas.fibers))<5)
IM.dat=filter(IM.dat,abs(scale(bl.total.dtiMD.FiberAtlas.fibers))<5)

tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)

targetdata = tab.data
colnames(targetdata)[1]='f.eid'


# Set categorical variables 
targetdata$race_ethnicity = as.factor(targetdata$race_ethnicity)
targetdata$sex = as.factor(targetdata$sex)
targetdata$fsqc_qu_motion = as.factor(targetdata$fsqc_qu_motion)
targetdata$site_id_l = as.factor(targetdata$site_id_l)
targetdata$bioFather_remote = as.factor(targetdata$bioFather_remote)
targetdata$bioMother_remote = as.factor(targetdata$bioMother_remote)

targetdata$Discrepancy_parent_child_abs=abs(targetdata$KSADS.Depressive_symptoms_ever.p-
                                          targetdata$KSADS.Depressive_symptoms_ever.y)
targetdata$Discrepancy_parent_child=targetdata$KSADS.Depressive_symptoms_ever.p-
                                   targetdata$KSADS.Depressive_symptoms_ever.y


# QQridgess - correlation: KSADS DS and CBCL ---------------------------
fig.dat=filter(targetdata,!is.na(KSADS.Depressive_symptoms_ever.p))
fig.dat$KSADS.Depressive_symptoms_ever.p=
  mapvalues(fig.dat$KSADS.Depressive_symptoms_ever.p,from=3:0,to= 
                              c("Severe", 
                                "Moderate",
                                'Mild',
                               'None of the above')) 
fig.dat$KSADS.Depressive_symptoms_ever.p=factor(fig.dat$KSADS.Depressive_symptoms_ever.p,
                         levels=c('None of the above',
                                  'Mild',
                                  "Moderate",
                                  "Severe"))
fig.p=ggplot(fig.dat, aes(y=as.factor(KSADS.Depressive_symptoms_ever.p), 
                          x=CBCL.dsm5.depre.p,
                          fill=as.factor(KSADS.Depressive_symptoms_ever.p))) + 
  geom_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  theme_ridges() +
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5),
        axis.text.y=element_blank(),
        legend.position = 'bottom')+
  guides(colour=guide_legend(nrow=2))+
  xlab('CBCL-measured DS')+
  ylab('Density')+
  labs(fill = "Group (KSADS-measured DS)")
  #ggtitle('Parent report')+
  #xlim(-1,4)

tiff(file="Figs/SuppleInfo/DepreSymp_CBCLandKSADS_correlation.tiff",
     width=22, height=13, units='cm', res=200)
fig.p
dev.off()


# Histogram: KSADS by parents and by youths ---------------------------
fig.dat=targetdata[,c('f.eid','KSADS.Depressive_symptoms_ever.p',
                      'KSADS.Depressive_symptoms_ever.y')]
colnames(fig.dat)[2:3]=c('reported by caregivers','reported by children')
fig.dat=melt(data=fig.dat,
             id.vars = 'f.eid')
colnames(fig.dat)=c('f.eid','Measure','Score')

fig.1=ggplot(fig.dat, aes(x=Score, fill = Measure)) +
  geom_histogram(alpha = 0.6,binwidth=1,position = "stack") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  facet_wrap(~Measure)+
  ylab("Count")+
  xlab("Depressive symptoms") +
  theme(axis.text=element_text(size=6),
        strip.text = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text=element_text(size=6),
        legend.position = 'right',
        axis.line = element_blank())

fig.2=ggplot(targetdata, aes(x=Discrepancy_parent_child_abs)) +
  geom_histogram(alpha = 0.1, binwidth=1 ,color="#E69F00", fill = "#E69F00") +
  #xlim(0,13)+
  ylab("Count") +
  labs(x='Absolute discrepancy of depressive symptoms\nreported by caregiver and child') +
  theme(axis.text=element_text(size=6),
        strip.text = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text=element_text(size=6),
        axis.line = element_blank())
  

fig.3=ggplot(targetdata, aes(x=Discrepancy_parent_child)) +
  geom_histogram(alpha = 0.1,binwidth=1,color="#999999", fill = "#999999") +
  #xlim(0,13)+
  ylab("Density")+
  theme(axis.text=element_text(size=6),
        strip.text = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text=element_text(size=6),
        axis.line = element_blank())+
  labs(x='Discrepancy of depressive symptoms\nreported by parent and child')

fig=ggarrange(fig.1,
              ggarrange(fig.2,fig.3,ncol=2,labels=c('b','c')),
                      heights = c(1.2,1),labels = 'a',
                      nrow = 2)



png(file="Figs/SuppleInfo/DepreSymp_YandP_distribution.png",
     width=14, height=14, units='cm', res=200)
fig
dev.off()
