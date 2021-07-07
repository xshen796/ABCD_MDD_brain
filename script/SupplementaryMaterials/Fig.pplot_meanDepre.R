setwd("/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain")

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')

# Settings
load('result/x.Supplementary_materials/meanYouthDepree_WM_subcor_FS_noSocialCov.RData')


# Preprocess results object -----------------------------------------------
# Re-correct p values whole-brain level
TargetResult = result.YouthDepre.region.covWholeB %>% 
  mutate(dependent=gsub('.x$','',dependent)) %>% 
  mutate(dependent=gsub('.y$','',dependent)) %>%
  .[!duplicated(.$dependent),]
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)

# Remove vol.ASEG (subcortical volumes)
targetdata = result.wholeBcorrected[!grepl('vol.ASEG',result.wholeBcorrected$dependent),]

# Load main model results -------------------------------------------------

load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')
result.mainmodel=filter(result.YouthDepre.region.covWholeB,
                        factor=='KSADS.Depressive_symptoms_ever.p')%>% 
  mutate(dependent=gsub('.x$','',dependent)) %>% 
  mutate(dependent=gsub('.y$','',dependent)) %>%
  .[!duplicated(.$dependent),]

TargetResult = result.mainmodel
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected.mainmodel=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)
ls.dep.sig=result.wholeBcorrected.mainmodel$dependent[result.wholeBcorrected.mainmodel$p.corrected<0.05]


# Extract regions to put label on -----------------------------------------

ls.target.sig = filter(targetdata,p.corrected<0.05) %>% 
  .$dependent

ls.mark.region = ls.target.sig %>%  
  c(.,ls.dep.sig) %>% 
  .[!duplicated(.)]

ls.changed = c(setdiff(ls.target.sig,ls.dep.sig), # new sig
               setdiff(ls.dep.sig,ls.target.sig)) # new null

# Add category and labels -------------------------------------------------
ls.category=read.table('data/p_plot_dat/ls.category.txt',header=F,stringsAsFactors = F,sep = '\t')
ls.category$V2=gsub('\\\\n','\\\n',ls.category$V2)

add_label <- function(tmp.lab,tmp.replace.dic){
  tmp.region = strsplit(tmp.lab,split = '\\.') %>% 
    .[[1]] %>% .[length(.)]
  tmp.label.return = tmp.replace.dic$Description[tmp.replace.dic$Field.name==tmp.region]
  return(tmp.label.return)
}

region.replace=rbind(read.csv('data/result_table_data/ls.Freesurfer_region_replacement_labels.csv',sep='\t',stringsAsFactors = F),
                     read.csv('data/result_table_data/ls.DTI_region_replacement_labels.csv',sep='\t',stringsAsFactors = F))

ls.label = data.frame(V1=ls.mark.region,
                      V2=lapply(as.list(ls.mark.region),
                                FUN = add_label,
                                tmp.replace.dic=region.replace) %>% unlist,
                      stringsAsFactors = F) %>% 
  mutate(is.shape=ifelse(V1 %in% ls.target.sig==1,T,F)) %>% 
  mutate(V2=ifelse(V1 %in% setdiff(ls.target.sig,ls.dep.sig),paste0(V2,'(+)'),V2)) %>% 
  mutate(V2=ifelse(V1 %in% setdiff(ls.dep.sig,ls.target.sig),paste0(V2,'(-)'),V2))

# Make the figure: a file printed at 'outputpath' and object returned as fig
fig=p_plot(TargetResult=targetdata,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,fig_size=c(29,18),
           labels_annot=ls.label,add_category_name=T,y_lim=4.3,
           outputpath='Figs/SuppleInfo/meanDepre_pplot.png')