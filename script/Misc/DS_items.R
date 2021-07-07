######
# by Shen
# Purposes:
# 1. Create and tidy up depression categorical phenotypes: MDD and broad depression (other unspecified types of depression included)
# 2. Summarise depressive symptoms based on KSADS and CBCL
# 3. Extract individual items in KSADS and CBCL


library(dplyr)

setwd('/gpfs/igmmfs01/eddie/GenScotDepression/shen/SData/UKB/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')
tab.dat.dir='/exports/igmm/eddie/GenScotDepression/shen/bakup.dat/ABCDphenotype/release2.0.1/iii.data'

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
tmp.dat.ever = targetdat.clean[,new.dic.current$Field.name]+
      targetdat.clean[,new.dic.past$Field.name]
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

tmp.dic=new.dic.current
      tmp.field = tmp.dic$Field.name
      tmp.group.field = lapply(ls.items.Symp,FUN=function(x) tmp.dic$Field.name[grep(paste0(x,collapse='|'),tmp.dic$Description)]) %>%
                        unlist
      rownames(tmp.dic)=tmp.dic$Field.name
      tmp.dic.output = tmp.dic[tmp.group.field,c('Field.name','Description')]
      rownames(tmp.dic.output)=NULL
      dic.current.y=tmp.dic.output

tmp.dic=new.dic.past
      tmp.field = tmp.dic$Field.name
      tmp.group.field = lapply(ls.items.Symp,FUN=function(x) tmp.dic$Field.name[grep(paste0(x,collapse='|'),tmp.dic$Description)]) %>%
                        unlist
      rownames(tmp.dic)=tmp.dic$Field.name
      tmp.dic.output = tmp.dic[tmp.group.field,c('Field.name','Description')]
      rownames(tmp.dic.output)=NULL
      dic.past.y=tmp.dic.output

tmp.dic=cbind(dic.current.y$Field.name,dic.past.y)
colnames(tmp.dic)[1:2]=c('Field.name.current','Field.name.past')
tmp.dic$Description=gsub(' Past','',tmp.dic$Description)
tmp.dic$Description=gsub('Past','',tmp.dic$Description)
tmp.dic$Description=gsub('Symptom - ','',tmp.dic$Description)
tmp.dic$Description=gsub('Diagnosis - ','',tmp.dic$Description)

dic.y=tmp.dic


# Parents on adolescents
new.dic = ls.var[grep('^Symptom -|Suicid|suicid|Attempt|SelfInjur',ls.var$Description),] %>%  # Items under MDD category in ABCD (symptom) + suicidality & self-injurial behaviour (diagnosis)
          .[grep('_ksad',.$File.name),] %>%
          .[grep('p$',.$Field.name),]
new.dic$Description=gsub(',','',new.dic$Description)


new.dic.ever = new.dic[!grepl('^Symptom - No two month symptom',new.dic$Description),]
new.dic.current = new.dic.ever[grep('Present',new.dic.ever$Description),]
new.dic.past = new.dic.ever[grep('Past',new.dic.ever$Description),]

# make a data for depressive symptoms ever
# example: weight gain for both current sand past is only counted once
new.dic.current = new.dic.current[order(new.dic.current$Description),]
new.dic.past = new.dic.past[order(new.dic.past$Description),]
tmp.dat.ever = targetdat.clean[,new.dic.current$Field.name]+
      targetdat.clean[,new.dic.past$Field.name]
tmp.dat.ever[tmp.dat.ever>1]=1
tmp.dat.ever.y=tmp.dat.ever
dic.current.y=new.dic.current


tmp.dic=new.dic.current
      tmp.field = tmp.dic$Field.name
      tmp.group.field = lapply(ls.items.Symp,FUN=function(x) tmp.dic$Field.name[grep(paste0(x,collapse='|'),tmp.dic$Description)]) %>%
                        unlist
      rownames(tmp.dic)=tmp.dic$Field.name
      tmp.dic.output = tmp.dic[tmp.group.field,c('Field.name','Description')]
      rownames(tmp.dic.output)=NULL
      dic.current.y=tmp.dic.output

tmp.dic=new.dic.past
      tmp.field = tmp.dic$Field.name
      tmp.group.field = lapply(ls.items.Symp,FUN=function(x) tmp.dic$Field.name[grep(paste0(x,collapse='|'),tmp.dic$Description)]) %>%
                        unlist
      rownames(tmp.dic)=tmp.dic$Field.name
      tmp.dic.output = tmp.dic[tmp.group.field,c('Field.name','Description')]
      rownames(tmp.dic.output)=NULL
      dic.past.y=tmp.dic.output

tmp.dic=cbind(dic.current.y$Field.name,dic.past.y)
colnames(tmp.dic)[1:2]=c('Field.name.current','Field.name.past')
tmp.dic$Description=gsub(' Past','',tmp.dic$Description)
tmp.dic$Description=gsub('Past','',tmp.dic$Description)
tmp.dic$Description=gsub('Symptom - ','',tmp.dic$Description)
tmp.dic$Description=gsub('Diagnosis - ','',tmp.dic$Description)

dic.p=tmp.dic

dic.all=cbind(dic.p[,1:2],dic.y)

write.table(dic.all,file='Table/SuppleMaterials/TableS1_DepreSympItems_draft.csv',quote=T,sep='\t',row.names=F,col.names=T)