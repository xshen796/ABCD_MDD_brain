library(dplyr)

ksad=readRDS('/sdata/images/projects/UKBIOBANK/ABCD/release2.0/iii.data/Mental_Health/abcd_ksad01.rds')

ksad.instance0=ksad[ksad$eventname=='baseline_year_1_arm_1',]
ksad.instance1=ksad[ksad$eventname=='1_year_follow_up_y_arm_1',]

MDD_lifetime.dat.ins0=data.frame(id=ksad.instance0$src_subject_id,
                                 ksad.instance0[,c('ksads_1_840_p','ksads_1_841_p','ksads_1_842_p')])
Depression_lifetime.dat.ins0=data.frame(id=ksad.instance0$src_subject_id,
                                        ksad.instance0[,c('ksads_1_840_p','ksads_1_841_p','ksads_1_842_p',
                                                          'ksads_1_846_p','ksads_1_847_p')])


##### MDD
MDD_lifetime.ins0=data.frame(ID=MDD_lifetime.dat.ins0$id,ksads.MDD.lifetime.case=
                                   rowSums(MDD_lifetime.dat.ins0[,2:ncol(MDD_lifetime.dat.ins0)]==1,na.rm=T))
MDD_lifetime.ins0$ksads.MDD.lifetime.control=rowSums(MDD_lifetime.dat.ins0[,2:ncol(MDD_lifetime.dat.ins0)]==0,na.rm=T)

MDD_lifetime.ins0$ksads.MDD.lifetime=rep(99999,nrow(MDD_lifetime.ins0))
MDD_lifetime.ins0$ksads.MDD.lifetime[MDD_lifetime.ins0$ksads.MDD.lifetime.case>0]=1
MDD_lifetime.ins0$ksads.MDD.lifetime[MDD_lifetime.ins0$ksads.MDD.lifetime.control==3]=0
MDD_lifetime.ins0$ksads.MDD.lifetime[MDD_lifetime.ins0$ksads.MDD.lifetime==99999]=NA

##### Depression

Depression_lifetime.ins0=data.frame(ID=Depression_lifetime.dat.ins0$id,ksads.MDD.lifetime.case=
                                   rowSums(Depression_lifetime.dat.ins0[,2:ncol(Depression_lifetime.dat.ins0)]==1,na.rm=T))
Depression_lifetime.ins0$ksads.MDD.lifetime.control=rowSums(Depression_lifetime.dat.ins0[,2:ncol(Depression_lifetime.dat.ins0)]==0,na.rm=T)

Depression_lifetime.ins0$ksads.MDD.lifetime=rep(99999,nrow(Depression_lifetime.ins0))
Depression_lifetime.ins0$ksads.MDD.lifetime[Depression_lifetime.ins0$ksads.MDD.lifetime.case>0]=1
Depression_lifetime.ins0$ksads.MDD.lifetime[Depression_lifetime.ins0$ksads.MDD.lifetime.control==5]=0
Depression_lifetime.ins0$ksads.MDD.lifetime[Depression_lifetime.ins0$ksads.MDD.lifetime==99999]=NA

