setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/FamilialRisk_PGRS_MDD')

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')

# Settings
load('result/x.Supplementary_materials/FAMrisk_asr_WM_FS.RData')
ls.category=read.table('data/p_plot_dat/ls.category.txt',header=F,stringsAsFactors = F,sep = '\t')
ls.category$V2=gsub('\\\\n','\\\n',ls.category$V2)
ls.label=read.table('data/p_plot_dat/ls.label_famRisk_mainresult.txt',header=F,stringsAsFactors = F,sep='\t')

# Re-correct p values whole-brain level
TargetResult = result.YouthDepre.region.covWholeB
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)

# Mother
targetdata = filter(result.wholeBcorrected,factor=='ASRDepre_bioMother')
targetdata = targetdata[!grepl('vol.ASEG',targetdata$dependent),]
fig.m=p_plot(TargetResult=targetdata,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,
           plot_title='Mother',y_lim=4,
           labels_annot=ls.label,add_category_name=F)

# Father
targetdata = filter(result.wholeBcorrected,factor=='ASRDepre_bioFather')
targetdata = targetdata[!grepl('vol.ASEG',targetdata$dependent),]
fig.f=p_plot(TargetResult=targetdata,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,
           plot_title='Father',y_lim=4,
           labels_annot=NA,add_category_name=F)

# Parents
targetdata = filter(result.wholeBcorrected,factor=='ASRDepre_bioParents')
targetdata = targetdata[!grepl('vol.ASEG',targetdata$dependent),]
fig.p=p_plot(TargetResult=targetdata,color_theme_usr='Shen',shape_sig=T,
           category.input=ls.category,
           plot_title='Parents',y_lim=4,
           labels_annot=NA,add_category_name=T)



# Combine plots -----------------------------------------------------------

fig.total=ggarrange(fig.m,fig.f,fig.p,
                  heights = c(1,1,1.3),
                  nrow=3,ncol=1,align = 'v',
                  labels = c('a.','b.','c.'))


tiff("Figs/SuppleInfo/famRisk_asr_pplot.tiff", width = 9, height = 16, units = 'in', res = 300)
fig.total # Make plot
dev.off()

