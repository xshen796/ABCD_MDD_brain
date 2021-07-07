setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD/')

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')

# Settings
load('result/x.Supplementary_materials/YouthDepree_WM_subcor_FS_CovMRImanufacturer.RData')
res.1=result.YouthDepre.region.covWholeB
load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')
res.2=result.YouthDepre.region.covWholeB

# Re-correct p values whole-brain level
TargetResult = res.1
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

res.1=wholeB_correction(TargetResult = res.1,ls.factor = ls.factor,ls.dep = ls.dep)
res.2=wholeB_correction(TargetResult = res.2,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)

# Select CBCL results. Remove vol.ASEG (subcortical volumes)
res.1 = filter(res.1,factor!='CBCL.dsm5.depre.p')
res.1 = res.1[!grepl('vol.ASEG',res.1$dependent),]
res.2 = filter(res.2,factor!='CBCL.dsm5.depre.p')
res.2 = res.2[!grepl('vol.ASEG',res.2$dependent),]



# Figs --------------------------------------------------------------------

# Correlation: Beta 

fig.dat.beta=data.frame(res2.beta=res.2$beta,res1.beta=res.1$beta,
                        res2.std=res.2$std,res1.std=res.1$std)
cor.dat.beta=cor(fig.dat.beta[,c('res1.beta','res2.beta')])
r.beta=round(cor.dat.beta[1,2],digits = 3)
fig.beta=ggplot(fig.dat.beta, aes(x=res2.beta, 
                                  y=res1.beta)) +
  geom_point(alpha=0.7,size=1)+
  annotate(geom="text",label=paste0('r = ',r.beta), x=-0.02, y=0.01)+
  xlab('Beta/Cohen\'s d: Not controlling for\nMRI manufacturer (main model)')+
  ylab('Beta/Cohen\'s d: Controlling for\nMRI manufacturer')+
  geom_smooth(method=lm,size=0.3)

# Correlation: p uncorrected 
fig.dat.p=data.frame(res2.p.value=res.2$p.value,
                     res2.p.corrected=res.2$p.corrected,
                     res1.p.value=res.1$p.value)
fig.dat.p$cl='p.corr>0.05 (main model)'
fig.dat.p$cl[fig.dat.p$res2.p.corrected<0.05]='p.corr<0.05 (main model)'
cor.dat.p=cor(fig.dat.p[,c('res1.p.value','res2.p.value')])
r.p=round(cor.dat.p[1,2],digits = 3)

fig.p=ggplot(fig.dat.p, aes(x=-log10(res2.p.value), 
                            y=-log10(res1.p.value))) +
  geom_point(alpha=0.7,size=1,aes(colour=cl))+
  scale_color_manual(values=c("orangered2","Black"))+ #, "#56B4E9"
  geom_hline(yintercept=-log10(0.05),linetype="dashed",color = "grey",size=1)+
  theme(legend.title = element_blank())+
  annotate(geom="text",label=paste0('r = ',r.p), x=1, y=3)+
  xlab('-log10(P-value): Not controlling for\nMRI manufacturer (main model)')+
  ylab('-log10(P-value): Controlling for\nMRI manufacturer')+
  geom_smooth(method=lm,size=0.3)



# Final plot --------------------------------------------------------------

sumPlot=ggarrange(fig.beta,fig.p,
                  nrow = 1, ncol=2, align='h',widths = c(1,1.3))
png("Figs/SuppleInfo/CovMRI_MainModel_correlation.png", width = 12, height = 5, units = 'in', res = 300)
sumPlot # Make plot
dev.off()
