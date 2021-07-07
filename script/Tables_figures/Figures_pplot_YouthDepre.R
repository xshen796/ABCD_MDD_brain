setwd('Z:/Documents/ActiveProject/ImagingProject/Adolescent_MDD_brain/')

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')


# Settings
load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')

# Preprocess result objects -----------------------------------------------

# Re-correct p values whole-brain level
TargetResult = result.YouthDepre.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

# Select CBCL results. Remove vol.ASEG (subcortical volumes)
targetdata = filter(TargetResult,factor!='CBCL.dsm5.depre.p')
targetdata = targetdata[!grepl('vol.ASEG',targetdata$dependent),]

result.wholeBcorrected=wholeB_correction(TargetResult = targetdata,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)

targetdata=result.wholeBcorrected

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
                     .[grep('KSADS.Depressive_symptoms_ever',.$factor),] %>%  
                     .$dependent
ls.label = data.frame(V1=ls.sig.region,
                      V2=lapply(as.list(ls.sig.region),
                                FUN = add_label,
                                tmp.replace.dic=region.replace) %>% unlist)
ls.sig.region=filter(targetdata,
                     p.corrected<0.05) %>%
                     .[grep('KSADS.MDD',.$factor),] %>%  
                    .$dependent
ls.label.MDD = data.frame(V1=ls.sig.region,
                      V2=lapply(as.list(ls.sig.region),
                                FUN = add_label,
                                tmp.replace.dic=region.replace) %>% unlist)


# DS ----------------------------------------------------------------------

# Parental reports
targetdata.p = targetdata[grep('\\.p$',targetdata$mod_name),]
targetdata.p.depre = filter(targetdata.p,factor=='KSADS.Depressive_symptoms_ever.p')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.p.DS=p_plot(TargetResult=targetdata.p.depre,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,sig_nominal=T,
           plot_title='Depressive symptoms (reported by caregivers)',
           labels_annot=ls.label,add_category_name=F)

# Reports by children 
targetdata.y = targetdata[grep('\\.y$',targetdata$mod_name),]
targetdata.y.depre = filter(targetdata.y,factor=='KSADS.Depressive_symptoms_ever.y')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.y.DS=p_plot(TargetResult=targetdata.y.depre,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,sig_nominal=T,
           plot_title='Depressive symptoms (reported by children)',
           labels_annot=ls.label,add_category_name=T)

# Combine plots
fig.DS=ggarrange(fig.p.DS,fig.y.DS,
                 heights = c(1,1.3),
                 nrow=2,ncol=1,align = 'v',
                 labels = c('a.','b.'))
  
  
tiff("Figs/Depre_IM/Depre_IM_bypANDy.tiff", width = 9, height = 13, units = 'in', res = 300)
fig.DS # Make plot
dev.off()



# MDD ---------------------------------------------------------------------

# Parental reports
targetdata.p = targetdata[grep('\\.p$',targetdata$mod_name),]
targetdata.p.depre = filter(targetdata.p,factor=='KSADS.MDD.p')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.p.MDD=p_plot(TargetResult=targetdata.p.depre,color_theme_usr='Shen',shape_sig=T,
                category.input=ls.category,sig_nominal=T,
                plot_title='MDD (reported by caregivers)',
                labels_annot=ls.label.MDD,add_category_name=F)

# Reports by children 
targetdata.y = targetdata[grep('\\.y$',targetdata$mod_name),]
targetdata.y.depre = filter(targetdata.y,factor=='KSADS.MDD.y')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.y.MDD=p_plot(TargetResult=targetdata.y.depre,color_theme_usr='Shen',shape_sig=T,
                category.input=ls.category,sig_nominal=T,
                plot_title='MDD (reported by children)',
                labels_annot=ls.label.MDD,add_category_name=T)

# Combine plots
fig.MDD=ggarrange(fig.p.MDD,fig.y.MDD,
                 heights = c(1,1.3),
                 nrow=2,ncol=1,align = 'v',
                 labels = c('a.','b.'))


tiff("Figs/Depre_IM/MDD_IM_bypANDy.tiff", width = 9, height = 13, units = 'in', res = 300)
fig.MDD # Make plot
dev.off()
