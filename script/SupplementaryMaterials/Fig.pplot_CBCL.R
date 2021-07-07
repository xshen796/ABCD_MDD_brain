setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD/')

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')

# Settings
load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')


# Preprocess results object -----------------------------------------------
# Re-correct p values whole-brain level
TargetResult = result.YouthDepre.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)

# Select CBCL results. Remove vol.ASEG (subcortical volumes)
targetdata = filter(result.wholeBcorrected,factor=='CBCL.dsm5.depre.p')
targetdata = targetdata[!grepl('vol.ASEG',targetdata$dependent),]

# Add category and labels -------------------------------------------------
ls.category=read.table('data/p_plot_dat/ls.category.txt',header=F,stringsAsFactors = F,sep = '\t')
ls.category$V2=gsub('\\\\n','\\\n',ls.category$V2)

add_label <- function(tmp.lab,tmp.replace.dic){
  tmp.region = strsplit(tmp.lab,split = '\\.') %>% 
    .[[1]] %>% .[length(.)]
  tmp.label.return = tmp.replace.dic$Description[tmp.replace.dic$Field.name==tmp.region]
  return(tmp.label.return)
}

region.replace=rbind(read.csv('data/result_table_data/ls.Freesurfer_region_replacement_labels.csv',sep='\t'),
                     read.csv('data/result_table_data/ls.DTI_region_replacement_labels.csv',sep='\t'))
ls.sig.region=filter(targetdata,
                     p.corrected<0.05) %>% 
  .[grep('CBCL.dsm5',.$factor),] %>%  
  .$dependent
ls.label = data.frame(V1=ls.sig.region,
                      V2=lapply(as.list(ls.sig.region),
                                FUN = add_label,
                                tmp.replace.dic=region.replace) %>% unlist)

# Make the figure: a file printed at 'outputpath' and object returned as fig
fig=p_plot(TargetResult=targetdata,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,
           labels_annot=ls.label,add_category_name=T,
           outputpath='Figs/SuppleInfo/CBCL_pplot.tiff')