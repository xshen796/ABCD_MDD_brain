######
# by Shen
# Purposes:
# 1. Create and tidy up depression categorical phenotypes: MDD and broad depression (other unspecified types of depression included)
# 2. Summarise depressive symptoms based on KSADS and CBCL
# 3. Extract individual items in KSADS and CBCL


library(dplyr)

setwd('/gpfs/igmmfs01/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/data/abcd/release2.0.1/iii.data'

# merge and save the master file  ----------------------------------------------------------------------

dir.var.ls='data/ls.questionnaires.csv'
ls.var=read.csv(dir.var.ls,stringsAsFactors=FALSE,sep = '\t',quote = '')
ls.files.tocatch=paste0(ls.var$Category,'/',ls.var$File.name)
ls.files.tocatch=unique(ls.files.tocatch)
for (i in ls.files.tocatch){
      tmp.file=readRDS(paste0(tab.dat.dir,'/',i,'.rds'))
      ## follow-up track file have duplicated IDs. Only keep the baseline data. 
      if (length(unique(tmp.file$src_subject_id))<length(tmp.file$src_subject_id)){
            tmp.file = tmp.file[order(tmp.file$interview_date),]
            tmp.file = tmp.file[!duplicated(tmp.file$src_subject_id),]}
      
      if (i==ls.files.tocatch[1]){questn.dat=tmp.file}else{
                  cols.keep=!grepl('interview_date|interview_age|sex|gender|eventname',colnames(tmp.file))
                  tmp.file=tmp.file[,cols.keep]
                  questn.dat=merge(questn.dat,tmp.file,by='src_subject_id',all=T)                  
            }
}


# grep('\\.x$',colnames(IM.dat))  # check if there is any duplicated colnames
rm(i,tmp.file)

questn.dat=questn.dat[,ls.var$Field.name]

######################################################  preproc data ######################################################################

targetdat=questn.dat
targetdat.clean=targetdat

# MDD diagnosis -----------------------------------------------------------

# Adolescents
sum_diagnosis <- function(x,diagnosis,dic) {
      tmp.dic = dic[grep(diagnosis,dic$Description),]
      tmp.field = tmp.dic$Field.name
      dat = x[,colnames(x) %in% tmp.field]
      tmp.output = rowSums(dat==1,na.rm=T)
      tmp.output[tmp.output>0]=1
      tmp.output[rowSums(!is.na(dat))==0]=NA

      return(tmp.output)
} 
mdd.diagnosis.dic = ls.var[ls.var$File.name %in% 'abcd_ksad501',]
diagnosis.dat.tomerge=
      data.frame(src_subject_id=targetdat.clean$src_subject_id,
                 KSADS.MDD.y=sum_diagnosis(targetdat.clean,'Major Depressive Disorder',mdd.diagnosis.dic))
targetdat.clean=merge(targetdat.clean,diagnosis.dat.tomerge)

# Parents' reports on youth
mdd.diagnosis.dic = ls.var[ls.var$File.name %in% 'abcd_ksad01',]
diagnosis.dat.tomerge=
      data.frame(src_subject_id=targetdat.clean$src_subject_id,
                 KSADS.MDD.p=sum_diagnosis(targetdat.clean,'Major Depressive Disorder',mdd.diagnosis.dic))
targetdat.clean=merge(targetdat.clean,diagnosis.dat.tomerge)
targetdat.clean=data.frame(targetdat.clean[,!grepl('^ksads_1_8',colnames(targetdat.clean))])


# Total number of depressive symptoms KSADS ------------------------------------------------
# Adolescents
new.dic = ls.var[grep('^Symptom -|Suicid|suicid|Attempt|SelfInjur',ls.var$Description),] %>%  # Items under MDD category in ABCD (symptom) + suicidality & self-injurial behaviour (diagnosis)
          .[grep('_ksad',.$File.name),] %>%
          .[grep('t$',.$Field.name),]
new.dic$Description=gsub(',','',new.dic$Description)


new.dic.ever = new.dic[!grepl('^Symptom - No two month symptom',new.dic$Description),]
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

# make a data for depressive symptoms ever
# example: weight gain for both current and past is only counted once
new.dic.current = new.dic.current[order(new.dic.current$Description),]
new.dic.past = new.dic.past[order(new.dic.past$Description),]

for (i in 1:nrow(new.dic.current)){
    tmp.field.current=new.dic.current$Field.name[i]
    tmp.field.past=new.dic.past$Field.name[i]
    tmp.dat.col=rowSums(targetdat.clean[,c(tmp.field.current,tmp.field.past)],na.rm=T)
    tmp.dat.col[rowSums(is.na(targetdat.clean[,c(tmp.field.current,tmp.field.past)]))]=NA
    if(i==1){tmp.dat.ever=tmp.dat.col}else{tmp.dat.ever=cbind(tmp.dat.ever,tmp.dat.col)}
}
colnames(tmp.dat.ever)=new.dic.current$Field.name

tmp.dat.ever[tmp.dat.ever>1]=1
tmp.dat.ever.y=tmp.dat.ever
dic.current.y=new.dic.current

ls.items.Symp = list(
                    'Depressed Mood',
                    'Anhedonia',
                    'Fatigue',
                    'Concentration Disturbance',
                    'Decreased Self-Esteem',
                    'Guilt',
                    'Hopeless',
                    'Impairment in functioning due to depression',
                    'Indecision',
                    'Irritability',  
                     c('Insomnia','Hypersomnia'),
                     c('Weight Loss','Weight Gain'),
                     c('Decreased Appetite','Increased Appetite'),
                     c('Psychomotor Agitation','Psychomotor Retardation'),
                     c('Suicid|suicid|Attempt|SelfInjur'))

sum_diagnosis <- function(x,tmp.dic,ls.items.groupSymp) {
      tmp.field = tmp.dic$Field.name
      dat = x[,colnames(x) %in% tmp.field]
      
      tmp.group.field = lapply(ls.items.groupSymp,FUN=function(x) tmp.dic$Field.name[grep(paste0(x,collapse='|'),tmp.dic$Description)])
      tmp.group.output = lapply(tmp.group.field,FUN=function(x) rowSums(data.frame(dat[,x])==1,na.rm=T)) %>%
                         bind_cols 
      tmp.group.output[tmp.group.output>1]=1
      tmp.output=tmp.group.output %>%
                         rowSums(.,na.rm=T)
      return(tmp.output)
} 

      
Depre_symptoms.DSM.tomerge=
      data.frame(src_subject_id=targetdat.clean$src_subject_id,
                 KSADS.Dep_symptoms_sum_ever.y=sum_diagnosis(tmp.dat.ever,new.dic.current,ls.items.Symp),
                 KSADS.Dep_symptoms_sum_current.y=sum_diagnosis(targetdat.clean,new.dic.current,ls.items.Symp))

# remove people with all NAs for either MDD symptomology or self-harm&suicidality diagnosis
tmp.loc.symp = rowSums(!is.na(tmp.dat.ever[,new.dic.current$Field.name[grep('^Symptom',new.dic.current$Description)]]))==0
tmp.loc.diagn = rowSums(!is.na(tmp.dat.ever[,new.dic.current$Field.name[grep('^Diagnosis',new.dic.current$Description)]]))==0
tmp.loc = tmp.loc.symp==T&tmp.loc.diagn==T 
Depre_symptoms.DSM.tomerge$KSADS.Dep_symptoms_sum_ever.y[tmp.loc]=NA
tmp.loc = rowSums(is.na(targetdat.clean[,new.dic.current$Field.name]))
tmp.loc.symp = rowSums(!is.na(targetdat.clean[,new.dic.current$Field.name[grep('^Symptom',new.dic.current$Description)]]))==0
tmp.loc.diagn = rowSums(!is.na(targetdat.clean[,new.dic.current$Field.name[grep('^Diagnosis',new.dic.current$Description)]]))==0
tmp.loc = tmp.loc.symp==T&tmp.loc.diagn==T 
Depre_symptoms.DSM.tomerge$KSADS.Dep_symptoms_sum_current.y[tmp.loc]=NA

targetdat.clean=merge(targetdat.clean,Depre_symptoms.DSM.tomerge,by='src_subject_id',all.x=T)

# Parents on youth
new.dic = ls.var[grep('^Symptom -|Suicid|suicid|Attempt|SelfInjur',ls.var$Description),] %>%  # Items under MDD category in ABCD (symptom) + suicidality & self-injurial behaviour (diagnosis)
          .[grep('_ksad',.$File.name),] %>%
          .[grep('p$',.$Field.name),]

new.dic.ever = new.dic[!grepl('^Symptom - No two month symptom',new.dic$Description),]
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

# make data for depressive symptoms ever
# example: weight gain for both current and past is only counted once
new.dic.current = new.dic.current[order(new.dic.current$Description),]
new.dic.past = new.dic.past[order(new.dic.past$Description),]

for (i in 1:nrow(new.dic.current)){
    tmp.field.current=new.dic.current$Field.name[i]
    tmp.field.past=new.dic.past$Field.name[i]
    tmp.dat.col=rowSums(targetdat.clean[,c(tmp.field.current,tmp.field.past)],na.rm=T)
    tmp.dat.col[rowSums(is.na(targetdat.clean[,c(tmp.field.current,tmp.field.past)]))]=NA
    if(i==1){tmp.dat.ever=tmp.dat.col}else{tmp.dat.ever=cbind(tmp.dat.ever,tmp.dat.col)}
}
colnames(tmp.dat.ever)=new.dic.current$Field.name

tmp.dat.ever[tmp.dat.ever>1]=1
tmp.dat.ever.p=tmp.dat.ever
dic.current.p=new.dic.current

Depre_symptoms.DSM.tomerge=
      data.frame(src_subject_id=targetdat.clean$src_subject_id,
                 KSADS.Dep_symptoms_sum_ever.p=sum_diagnosis(tmp.dat.ever,new.dic.current,ls.items.Symp),
                 KSADS.Dep_symptoms_sum_current.p=sum_diagnosis(targetdat.clean,new.dic.current,ls.items.Symp))

# remove people with all NAs
tmp.loc.symp = rowSums(!is.na(tmp.dat.ever[,new.dic.current$Field.name[grep('^Symptom',new.dic.current$Description)]]))==0
tmp.loc.diagn = rowSums(!is.na(tmp.dat.ever[,new.dic.current$Field.name[grep('^Diagnosis',new.dic.current$Description)]]))==0
tmp.loc = tmp.loc.symp==T&tmp.loc.diagn==T 
Depre_symptoms.DSM.tomerge$KSADS.Dep_symptoms_sum_ever.p[tmp.loc]=NA
tmp.loc = rowSums(is.na(targetdat.clean[,new.dic.current$Field.name]))
tmp.loc.symp = rowSums(!is.na(targetdat.clean[,new.dic.current$Field.name[grep('^Symptom',new.dic.current$Description)]]))==0
tmp.loc.diagn = rowSums(!is.na(targetdat.clean[,new.dic.current$Field.name[grep('^Diagnosis',new.dic.current$Description)]]))==0
tmp.loc = tmp.loc.symp==T&tmp.loc.diagn==T 

Depre_symptoms.DSM.tomerge$KSADS.Dep_symptoms_sum_current.p[tmp.loc]=NA

targetdat.clean=merge(targetdat.clean,Depre_symptoms.DSM.tomerge,by='src_subject_id',all.x=T)


# Depressive symptoms (DSM-V criteria) based on DSM core and secondary symptoms ever ------------

# Select depressive items (ever)    ---------------------------------------
Acol.item = 'Depressed.Mood|Anhedonia|Fatigue'
Suicidality.item = 'Suicid|Attempt'

ls.cols.p.core = dic.current.p$Field.name[grep(Acol.item,dic.current.p$Description)]
ls.cols.y.core = dic.current.y$Field.name[grep(Acol.item,dic.current.y$Description)]

ls.cols.p.suicidality = dic.current.p[grep(Suicidality.item,dic.current.p$Description),] %>%
                         .[!grepl('Suicidalideation',.$Description),] %>%
                         .[,'Field.name']
ls.cols.y.suicidality = dic.current.y[grep(Suicidality.item,dic.current.y$Description),] %>%
                         .[!grepl('Suicidalideation',.$Description),] %>%
                         .[,'Field.name']

targetdat.clean$DS.core.total.p=rowSums(tmp.dat.ever.p[,ls.cols.p.core],na.rm=T)
targetdat.clean$DS.secondary.total.p=targetdat.clean$KSADS.Dep_symptoms_sum_ever.p-targetdat.clean$DS.core.total.p
targetdat.clean$DS.suicidality.total.p=rowSums(tmp.dat.ever.p[,ls.cols.p.suicidality],na.rm=T)
targetdat.clean$DS.core.total.y=rowSums(tmp.dat.ever.y[,ls.cols.y.core],na.rm=T)
targetdat.clean$DS.secondary.total.y=targetdat.clean$KSADS.Dep_symptoms_sum_ever.y-targetdat.clean$DS.core.total.y
targetdat.clean$DS.suicidality.total.y=rowSums(tmp.dat.ever.y[,ls.cols.y.suicidality],na.rm=T)

# parental report
targetdat.clean = mutate(targetdat.clean,KSADS.Depressive_symptoms_ever.p=ifelse(((DS.core.total.p==3 & DS.secondary.total.p>3)|(DS.suicidality.total.p>=1))&KSADS.MDD.p==1, 3, 
                                           ifelse((DS.core.total.p>1 & DS.secondary.total.p>2)|(KSADS.Dep_symptoms_sum_ever.p>=7 & KSADS.Dep_symptoms_sum_ever.p<=8),2,
                                           ifelse((DS.core.total.p>1 & DS.secondary.total.p>=1)|(KSADS.Dep_symptoms_sum_ever.p>=5 & KSADS.Dep_symptoms_sum_ever.p<=6),1,0))))
# child report
targetdat.clean = mutate(targetdat.clean,KSADS.Depressive_symptoms_ever.y=ifelse(((DS.core.total.y==3 & DS.secondary.total.y>3)|(DS.suicidality.total.y>=1))&KSADS.MDD.y==1, 3, 
                                           ifelse((DS.core.total.y>1 & DS.secondary.total.y>2)|(KSADS.Dep_symptoms_sum_ever.y>=7 & KSADS.Dep_symptoms_sum_ever.y<=8),2,
                                           ifelse((DS.core.total.y>1 & DS.secondary.total.y>=1)|((KSADS.Dep_symptoms_sum_ever.y>=5 & KSADS.Dep_symptoms_sum_ever.y<=6)),1,0))))
targetdat.clean$KSADS.Depressive_symptoms_ever.p[is.na(targetdat.clean$KSADS.Dep_symptoms_sum_ever.p)]=NA
targetdat.clean$KSADS.Depressive_symptoms_ever.y[is.na(targetdat.clean$KSADS.Dep_symptoms_sum_ever.y)]=NA

                                           
targetdat.clean = targetdat.clean[,!grepl('^DS.core|^DS.secondary|^DS.suici',colnames(targetdat.clean))]


# Depressive symptoms CBCL (total score) ----------------------------------
item_qc <- function(x,kw,dic,nitem) {
      tmp.dic = dic[grep(kw,dic$Description),]
      tmp.field = tmp.dic$Field.name
      dat = x[,colnames(x) %in% tmp.field]
      
      tmp.output = dat[,grep('_r$',colnames(dat))]
      tmp.output[dat[,grep('_nm$',colnames(dat))]>(nitem/2)]=NA
      return(tmp.output)
}

tmp.dic = ls.var[grep('CBCL',ls.var$Description),]
ls.subscale = tmp.dic$Description
ls.subscale = gsub(' Syndrome Scale ','',ls.subscale)
ls.subscale = gsub(' Scale ','',ls.subscale)
ls.subscale = gsub(' Scale2007','',ls.subscale)
ls.subscale = gsub(' CBCL DSM5','',ls.subscale)
ls.subscale = gsub(' CBCL',' ',ls.subscale)
ls.subscale = gsub(' \\(OCD\\)','',ls.subscale)
ls.subscale = gsub(' \\(SCT\\)','',ls.subscale)

ls.subscale = gsub('\\(raw score\\)|\\(t-score\\)|\\(missing values\\)|\\(number of missing values\\)','',ls.subscale) %>%
      trimws(.,which='both')
ls.subscale = unique(ls.subscale)

for (i in ls.subscale){
      tmp.output = item_qc(targetdat.clean,i,tmp.dic,5)
      if (i==ls.subscale[1]){
            total.output=tmp.output
      }else{
            total.output=cbind(total.output,tmp.output)
      }
}
total.output = data.frame(total.output)
colnames(total.output)=ls.subscale %>% gsub(' $','',.) %>% gsub(' ','_',.) %>% gsub('-','_',.) %>%
      paste0('CBCL.',.)
targetdat.clean=data.frame(targetdat.clean,total.output) %>%
      rename(CBCL.dsm5.depre.p=CBCL.Depress)

targetdat.clean=data.frame(targetdat.clean[,!grepl('^cbcl_scr_',colnames(targetdat.clean))])


# Depressive symptoms KSADS (individual items) ----------------------------

# Adolescents
new.dic = ls.var %>% .[grep('^Symptom -|Diagnosis',.$Description),] %>% 
  .[!grepl('Depressive Disorder',.$Description),]
new.dic = new.dic[grep('t$',new.dic$Field.name),]

new.dic.ever = new.dic[!grepl('^Symptom - No two month symptom',new.dic$Description),]
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

new.dic.ever$shortname=gsub('Symptom - ','',new.dic.ever$Description) %>% 
      gsub('Diagnosis - ','',.) %>%
      gsub(',','',.)
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

ksads.current=targetdat.clean[,new.dic.current$Field.name]
ksads.past=targetdat.clean[,new.dic.past$Field.name]
colnames(ksads.current)=paste0('KSADS.items.',gsub(' ','_',new.dic.current$shortname),'.y')
ksads.ever = ksads.current+ksads.past
ksads.ever[ksads.ever>1]=1
colnames(ksads.ever)=gsub('Present','Ever',colnames(ksads.ever))

targetdat.clean=data.frame(targetdat.clean,ksads.current,ksads.ever)


# Parents on youth
new.dic = ls.var %>% .[grep('^Symptom -|Diagnosis',.$Description),] %>% 
      .[!grepl('Depressive Disorder',.$Description),]
new.dic = new.dic[grep('p$',new.dic$Field.name),]

new.dic.ever = new.dic[!grepl('^Symptom - No two month symptom',new.dic$Description),]
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

new.dic.ever$shortname=gsub('Symptom - ','',new.dic.ever$Description) %>% 
      gsub('Diagnosis - ','',.) %>%
      gsub(',','',.)
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

ksads.current=targetdat.clean[,new.dic.current$Field.name]
ksads.past=targetdat.clean[,new.dic.past$Field.name]
colnames(ksads.current)=paste0('KSADS.items.',gsub(' ','_',new.dic.current$shortname),'.p')
ksads.ever = ksads.current+ksads.past
ksads.ever[ksads.ever>1]=1
colnames(ksads.ever)=gsub('Present','Ever',colnames(ksads.ever))

targetdat.clean=data.frame(targetdat.clean,ksads.current,ksads.ever)


targetdat.clean=data.frame(targetdat.clean[,!grepl('^ksads_1|^ksads_22',colnames(targetdat.clean))])


# Depressive symptoms CBCL (individual items) -----------------------------
cbcl.items=read.csv('data/CBCL.items',header = F,sep = '\t')
cbcl.items$V2=as.character(cbcl.items$V2)

new.dic.cbcl=ls.var[grep('cbcl_q',ls.var$Field.name),]
new.dic.cbcl$q.number=gsub('cbcl_q','',new.dic.cbcl$Field.name)
new.dic.cbcl$q.number=gsub('_p','',new.dic.cbcl$q.number)
new.dic.cbcl$q.number=as.numeric(new.dic.cbcl$q.number)

new.dic.cbcl=merge(new.dic.cbcl,cbcl.items,by.x='q.number',by.y='V1')

cbcl.item.data=targetdat.clean[,new.dic.cbcl$Field.name]
colnames(cbcl.item.data)=paste0('CBCL.items.',new.dic.cbcl$V2,'.p')

targetdat.clean=data.frame(targetdat.clean,cbcl.item.data)
targetdat.clean=data.frame(targetdat.clean[,!grepl('^cbcl_',colnames(targetdat.clean))])


# Final tidying and save the file -----------------------------------------

targetdat.clean=data.frame(targetdat.clean[,!grepl('^eventname$',colnames(targetdat.clean))])
saveRDS(targetdat.clean,file='data/Behavioural/Behavioural.rds')
