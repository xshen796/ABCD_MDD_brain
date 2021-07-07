# Site Comparison
# By XShen
# 18/05/2020

# Settings ----------------------------------------------------------------
library('dplyr')
library('ggplot2')
library('reshape2')
library('ggpubr')
setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD/')


# Load data ---------------------------------------------------------------

load("result/x.Supplementary_materials/YouthDepree_site_BulkMeasure.RData")
load("result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData")


# Make new result tables --------------------------------------------------

# Change factors and dependent variables
for (i in ls(pattern = '^site_res.||result.')){
  result.tmp=get(i)
  result.tmp$factor=gsub('KSADS\\.','',result.tmp$factor)
  result.tmp$factor=gsub('_ever','',result.tmp$factor)
  result.tmp$dependent=gsub('bl.mean.thk.APARC','Cortical_thickness',result.tmp$dependent)
  result.tmp$dependent=gsub('bl.mean.sulc.APARC','Cortical_sulcal_depth',result.tmp$dependent)
  result.tmp$dependent=gsub('bl.total.sa.APARC','Cortical_surface_area',result.tmp$dependent)
  result.tmp$dependent=gsub('bl.total.vol.APARC','Cortical_volume',result.tmp$dependent)
  result.tmp$dependent=gsub('bl.total.dtiFA.FiberAtlas.fibers','WM_FA',result.tmp$dependent)
  result.tmp$dependent=gsub('bl.total.dtiMD.FiberAtlas.fibers','WM_MD',result.tmp$dependent)
  
  eval(parse(text=paste0(i,'=result.tmp')))
}



# Create a list of results
tmp.result=site_res.YouthDepre.bulk.site01
tmp.result=filter(tmp.result,factor!='CBCL.dsm5.depre.p')
ls.test=tmp.result[,1:2]
# Reformat results
ls.res=ls(pattern = '^site_res')

for (k in 1:nrow(ls.test)){
  obj.name=paste0(ls.test[k,],collapse ='_')
  for (i in 1:length(ls.res)){
    tmp.site.res=get(ls.res[i])
    tmp.site.name=gsub('site_res.YouthDepre.bulk.','',ls.res[i])
    tmp.site.res$site=tmp.site.name
    tmp.site.res.test=tmp.site.res[(tmp.site.res$dependent==ls.test$dependent[k])&
                                     (tmp.site.res$factor==ls.test$factor[k]),]
    if (i==1){
      site.res.total=tmp.site.res.test
    }else{
      site.res.total=rbind(site.res.total,tmp.site.res.test)
    }
  }
  site.res.total$type='Site'
  wholeSample.res=result.YouthDepre.bulk
  wholeSample.res$site='Total'
  wholeSample.res$type='Total'
  wholeSample.res=wholeSample.res[(wholeSample.res$dependent==ls.test$dependent[k])&
                                   (wholeSample.res$factor==ls.test$factor[k]),]
  site.res.total=rbind(site.res.total[order(abs(site.res.total$beta),decreasing = T),],
                      wholeSample.res)
  site.res.total$ord=1:nrow(site.res.total)
  eval(parse(text=paste0(obj.name,'=site.res.total')))
}


# Define plotting function ------------------------------------------------
loo_plot<-function(result.table){
  fig.title=paste(gsub('_',' ',result.table$dependent[1]))
  
  fig = ggplot(result.table, aes(x=reorder(site,-ord), y=beta, color=type)) + 
    geom_line() +
    geom_point()+
    geom_errorbar(aes(ymin=beta-std, ymax=beta+std), width=.2,
                  position=position_dodge(0.05))+
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ggtitle(fig.title)+
    theme(legend.position = 'none',
          axis.text=element_text(size=8), axis.title=element_text(size=8),
          plot.title = element_text(lineheight=1, face="bold", vjust=1, hjust=0.5,size=9))+
    xlab('Site')+
    ylab('Beta')+
    coord_flip()
    return(fig)
  
}

# Make single plots -------------------------------------------------------
for (i in 1:nrow(ls.test)){
  obj.name=paste0(ls.test[i,],collapse ='_')
  tmp.fig=loo_plot(get(obj.name))
  eval(parse(text=paste0('fig.',obj.name,'=tmp.fig')))
}

ls.plots=ls.test
ls.plots$fig.name=paste0('fig.',paste0(ls.test[,1],'_',ls.test[,2]))


# Make concatenated plots -------------------------------------------------
ls.dep=unique(ls.plots$dependent)
ls.sumplots=unique(ls.plots$factor)


DSy.sumPlot=ggarrange(fig.Cortical_thickness_Depressive_symptoms.y,
                      fig.Cortical_sulcal_depth_Depressive_symptoms.y,
                      fig.Cortical_surface_area_Depressive_symptoms.y,
                      fig.Cortical_volume_Depressive_symptoms.y,
                      fig.WM_FA_Depressive_symptoms.y,
                      fig.WM_MD_Depressive_symptoms.y,
                      nrow = 2, ncol=3, align='h')

DSp.sumPlot=ggarrange(fig.Cortical_thickness_Depressive_symptoms.p,
                      fig.Cortical_sulcal_depth_Depressive_symptoms.p,
                      fig.Cortical_surface_area_Depressive_symptoms.p,
                      fig.Cortical_volume_Depressive_symptoms.p,
                      fig.WM_FA_Depressive_symptoms.p,
                      fig.WM_MD_Depressive_symptoms.p,
                      nrow = 2, ncol=3, align='h')

MDDy.sumPlot=ggarrange(fig.Cortical_thickness_MDD.y,
                      fig.Cortical_sulcal_depth_MDD.y,
                      fig.Cortical_surface_area_MDD.y,
                      fig.Cortical_volume_MDD.y,
                      fig.WM_FA_MDD.y,
                      fig.WM_MD_MDD.y,
                      nrow = 2, ncol=3, align='h')
MDDp.sumPlot=ggarrange(fig.Cortical_thickness_MDD.p,
                       fig.Cortical_sulcal_depth_MDD.p,
                       fig.Cortical_surface_area_MDD.p,
                       fig.Cortical_volume_MDD.p,
                       fig.WM_FA_MDD.p,
                       fig.WM_MD_MDD.p,
                      nrow = 2, ncol=3, align='h')




# Make plot files ---------------------------------------------------------

tiff("Figs/SuppleInfo/loo_DSp.tiff", width = 8, height = 6, units = 'in', res = 400)
DSp.sumPlot # Make plot
dev.off()

tiff("Figs/SuppleInfo/loo_DSy.tiff", width = 8, height = 6, units = 'in', res = 400)
DSy.sumPlot # Make plot
dev.off()

tiff("Figs/SuppleInfo/loo_MDDp.tiff", width = 8, height = 6, units = 'in', res = 400)
MDDp.sumPlot # Make plot
dev.off()

tiff("Figs/SuppleInfo/loo_MDDy.tiff", width = 8, height = 6, units = 'in', res = 400)
MDDy.sumPlot # Make plot
dev.off()
