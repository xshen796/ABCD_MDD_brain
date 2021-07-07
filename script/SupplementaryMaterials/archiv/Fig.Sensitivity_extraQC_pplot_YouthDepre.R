setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')

# Settings
load('result/x.Supplementary_materials/YouthDepree_WM_subcor_FS_harshQC.RData')
ls.category=read.table('data/p_plot_dat/ls.category.txt',header=F,stringsAsFactors = F,sep = '\t')
ls.category$V2=gsub('\\\\n','\\\n',ls.category$V2)
ls.label=read.table('data/p_plot_dat/ls.label.txt',header=F,stringsAsFactors = F,sep='\t')
ls.label$V2=gsub('\\\\n','\\\n',ls.label$V2)

# Preprocess result objects -----------------------------------------------

# Re-correct p values whole-brain level
TargetResult = result.YouthDepre.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)

# Select CBCL results. Remove vol.ASEG (subcortical volumes)
targetdata = filter(result.wholeBcorrected,factor!='CBCL.dsm5.depre.p')
targetdata = targetdata[!grepl('vol.ASEG',targetdata$dependent),]


# DS ----------------------------------------------------------------------

# Parental reports
targetdata.p = targetdata[grep('\\.p$',targetdata$mod_name),]
targetdata.p.depre = filter(targetdata.p,factor=='KSADS.Depressive_symptoms_ever.p')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.p.DS=p_plot(TargetResult=targetdata.p.depre,color_theme_usr='Shen',shape_sig=T,
                category.input=ls.category,
                plot_title='Depressive symptoms (reported by parents)',
                labels_annot=ls.label,add_category_name=F)

# Reports by children 
targetdata.y = targetdata[grep('\\.y$',targetdata$mod_name),]
targetdata.y.depre = filter(targetdata.y,factor=='KSADS.Depressive_symptoms_ever.y')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.y.DS=p_plot(TargetResult=targetdata.y.depre,color_theme_usr='Shen',shape_sig=T,
                category.input=ls.category,
                plot_title='Depressive symptoms (reported by children)',
                labels_annot=ls.label,add_category_name=T)

# Combine plots
fig.DS=ggarrange(fig.p.DS,fig.y.DS,
                 heights = c(1,1.3),
                 nrow=2,ncol=1,align = 'v',
                 labels = c('a.','b.'))


tiff("Figs/SuppleInfo/HarshQC_Depre_IM_bypANDy.tiff", width = 9, height = 13, units = 'in', res = 300)
fig.DS # Make plot
dev.off()



# MDD ---------------------------------------------------------------------

# Parental reports
targetdata.p = targetdata[grep('\\.p$',targetdata$mod_name),]
targetdata.p.depre = filter(targetdata.p,factor=='KSADS.MDD.p')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.p.MDD=p_plot(TargetResult=targetdata.p.depre,color_theme_usr='Shen',shape_sig=T,
                 category.input=ls.category,
                 plot_title='MDD (reported by parents)',
                 labels_annot=ls.label,add_category_name=F)

# Reports by children 
targetdata.y = targetdata[grep('\\.y$',targetdata$mod_name),]
targetdata.y.depre = filter(targetdata.y,factor=='KSADS.MDD.y')
# Make the figure: a file printed at 'outputpath' and object returned as fig
fig.y.MDD=p_plot(TargetResult=targetdata.y.depre,color_theme_usr='Shen',shape_sig=T,
                 category.input=ls.category,
                 plot_title='MDD (reported by children)',
                 labels_annot=ls.label,add_category_name=T)

# Combine plots
fig.MDD=ggarrange(fig.p.MDD,fig.y.MDD,
                  heights = c(1,1.3),
                  nrow=2,ncol=1,align = 'v',
                  labels = c('a.','b.'))


tiff("Figs/SuppleInfo/HarshQC_MDD_IM_bypANDy.tiff", width = 9, height = 13, units = 'in', res = 300)
fig.MDD # Make plot
dev.off()
