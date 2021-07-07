setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD/')
library(dplyr)
library(plyr)
library(ggplot2)
library(ggridges)
library(ggpubr)

# Load data ---------------------------------------------------------------

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
IM.dat=filter(IM.dat,!is.na(bl.total.dtiFA.FiberAtlas.fibers))
IM.dat[abs(scale(IM.dat$bl.total.dtiFA.FiberAtlas.fibers))>5,grep('dtiFA',colnames(IM.dat))]=NA
IM.dat[abs(scale(IM.dat$bl.total.dtiMD.FiberAtlas.fibers))>5,grep('dtiMD',colnames(IM.dat))]=NA

tab.data=merge(IM.dat,tab.data,by='src_subject_id',all.x=T)
targetdata = tab.data



# Extract DS for MDD groups defined by parent/child -----------------------

# A column for different MDD groups:
# 4 = MDD p&y, 3 = MDD p but not y, 2 = MDD y but not p, 1 = controls p&y 
# NA = none of the above
targetdata = mutate(targetdata,
                    MDDgroups=ifelse(KSADS.MDD.p==1&KSADS.MDD.y==1,4,
                                ifelse(KSADS.MDD.p==1&KSADS.MDD.y==0,3,
                                  ifelse(KSADS.MDD.p==0&KSADS.MDD.y==1,2,
                                     ifelse(KSADS.MDD.p==0&KSADS.MDD.y==0,1,NA
                                     )))))

# Whether any core symptom is present
targetdata = mutate(targetdata,
                    core.symptom.p=ifelse(KSADS.items.Anhedonia.Ever.p==1|
                                          KSADS.items.Depressed.Mood.Ever.p==1,1,
                                        ifelse(is.na(KSADS.items.Anhedonia.Ever.p)&
                                                 is.na(KSADS.items.Depressed.Mood.Ever.p),NA,0)))
targetdata = mutate(targetdata,
                    core.symptom.y=ifelse(KSADS.items.Anhedonia.Ever.y==1|
                                            KSADS.items.Depressed.Mood.Ever.y==1,1,
                                          ifelse(is.na(KSADS.items.Anhedonia.Ever.y)&
                                                   is.na(KSADS.items.Depressed.Mood.Ever.y),NA,0)))

# # Porprotion of core symptoms present for MDD groups
# groot1 = targetdata %>% group_by(MDDgroups) %>% tally(core.symptom.p==1)
# groot2 = targetdata %>% group_by(MDDgroups) %>% tally()
# groot1/groot2
# 
# # DS for MDD groups
# groot1 = targetdata %>% group_by(MDDgroups) %>% summarise(mean=mean(KSADS.Depressive_symptoms_ever.p,na.rm=T))

# Figures ------------------------------------------------------------------
fig.dat=filter(targetdata,!is.na(MDDgroups))
fig.dat$MDDgroups=mapvalues(fig.dat$MDDgroups,from=4:1,to= 
                          c("MDD case: parent and child report", 
                            "MDD case: parent report",
                            'MDD case: child report',
                            'Control: parent and child report')) 
fig.dat$MDDgroups=factor(fig.dat$MDDgroups,
                         levels=c('Control: parent and child report',
                                  'MDD case: child report',
                                  "MDD case: parent report",
                                  "MDD case: parent and child report"))
fig.p=ggplot(fig.dat, aes(y=as.factor(MDDgroups), 
                       x=KSADS.Depressive_symptoms_ever.p,
                       fill=as.factor(MDDgroups))) + 
  geom_density_ridges(aes(fill=as.factor(MDDgroups)),stat='binline',binwidth=1,scale=0.95) +
  theme_ridges() +
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5),
        axis.text.y=element_blank(),
        legend.position = 'bottom')+
  guides(colour=guide_legend(nrow=2))+
  xlab('Depressive symptoms')+
  ylab('Density')+
  labs(fill = "Group")+
  ggtitle('Parent report')+
  xlim(-1,4)

fig.y=ggplot(fig.dat, aes(y=as.factor(MDDgroups), 
                          x=KSADS.Depressive_symptoms_ever.y,
                          fill=as.factor(MDDgroups))) + 
  geom_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  theme_ridges() +
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5),
        axis.text.y=element_blank(),
        legend.position = 'bottom')+
  guides(colour=guide_legend(nrow=2))+
  xlab('Depressive symptoms')+
  ylab('Density')+
  labs(fill = "Group")+
  ggtitle('Child report')+
  xlim(0,15)

fig.total=ggarrange(fig.p,fig.y,ncol=2, widths = c(1,1), 
          common.legend = TRUE, legend="right")

ggsave('Figs/Behav/DS_for_MDDgroups.tiff',fig.total,width=12,height=5)





# qqridges ----------------------------------------------------------------

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
                          x=KSADS.Depressive_symptoms_ever.y,
                          fill=as.factor(KSADS.Depressive_symptoms_ever.p))) + 
  geom_density_ridges(aes(fill=as.factor(KSADS.Depressive_symptoms_ever.p)),
                      stat='binline',
                      binwidth=1,scale=0.95) +
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
