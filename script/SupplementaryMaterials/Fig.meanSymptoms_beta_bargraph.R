setwd('Z:/Documents/sdata_backup/Shen/iv.ABCD/release2.0.1/AdolescentMDD/')

source('FUNs/PheWAS_style_p_plot.R')
source('FUNs/wholeB_correction.R')

# Settings
load('result/i.Main_result/YouthDepree_WM_subcor_FS_noSocialCov.RData')
result.mainmodel=filter(result.YouthDepre.region.covWholeB,
                        factor=='KSADS.Depressive_symptoms_ever.p')
load('result/x.Supplementary_materials/meanYouthDepree_WM_subcor_FS_noSocialCov.RData')
ls.category=read.table('data/p_plot_dat/ls.category.txt',header=F,stringsAsFactors = F,sep = '\t')
ls.category$V2=gsub('\\\\n','\\\n',ls.category$V2)
ls.FS_label=read.table('data/result_table_data/ls.Freesurfer_region_replacement_labels.csv',header = T,
                       sep='\t',stringsAsFactors = F)
ls.DTI_label=read.table('data/result_table_data/ls.DTI_region_replacement_labels.csv',header = T,
                        sep='\t',stringsAsFactors = F)
ls.label=rbind(ls.FS_label,ls.DTI_label)


# Preprocess results ------------------------------------------------------

# Re-correct p values whole-brain level - main model
TargetResult = result.mainmodel
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected.mainmodel=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)
ls.dep.sig=result.wholeBcorrected.mainmodel$dependent[result.wholeBcorrected.mainmodel$p.corrected<0.05]

# Re-correct p values whole-brain level - diffs model
TargetResult = result.YouthDepre.region.covWholeB[result.YouthDepre.region.covWholeB$dependent %in% ls.dep.sig,]
ls.dep = c('vol.APARC','sa.APARC','sulc.APARC','thk.APARC','vol.ASEG',
           'dtiFA.FiberAtlas','dtiMD.FiberAtlas')
ls.factor = unique(TargetResult$factor)

result.wholeBcorrected.diffs=wholeB_correction(TargetResult = TargetResult,ls.factor = ls.factor,ls.dep = ls.dep)
rm(TargetResult)


targetdata = result.wholeBcorrected.diffs


# Add category --------------------------------------------------------
tmp.result.table=targetdata
tmp.result.table$category=''
category.input=ls.category
add_category <- function(ls.capture,category.name,targetMat,col.tocap){
  loc.tocap=grep(ls.capture,targetMat[,col.tocap])
  targetMat[loc.tocap,'category']=category.name
  return(targetMat)
}
for (i in 1:nrow(category.input)){
  if (i==1){
    tmp.result.table=add_category(category.input[i,1],category.input[i,2],
                                  targetMat = tmp.result.table, col.tocap = 'dependent')
  }else{
    tmp.result.table=add_category(category.input[i,1],category.input[i,2],
                                  targetMat = tmp.result.table, col.tocap = 'dependent')
  }
}


# Add label ---------------------------------------------------------------

if (sum(!is.na(ls.label))>0){
  for(k in 1:nrow(ls.label)){
    tmp.result.table$dependent[grep(ls.label[k,1],tmp.result.table$dependent)]=ls.label[k,2] 
  }
}
tmp.result.table$dependent=gsub('\\\\n',' ',tmp.result.table$dependent)
tmp.result.table$category=gsub('\\n',' ',tmp.result.table$category)

tmp.result.table$dependent=paste0(tmp.result.table$category,' in ',tmp.result.table$dependent)


# Reorder based on betas --------------------------------------------------
reord_graphdat=function(ls.cate,targetdata){
  for (c in ls.cate){
    temp.chunk=filter(targetdata,category==c)
    temp.chunk=temp.chunk[order(-temp.chunk$beta),]
    temp.length=nrow(temp.chunk)
    temp.chunk$l.category=rep(99999,nrow(temp.chunk))
    temp.chunk$l.category[round(temp.length/2)]=c
    temp.chunk$l.category[temp.chunk$l.category==99999]=' '
    if (c==ls.cate[1]){new.seq.targetdata=temp.chunk}else{
      new.seq.targetdata=rbind(new.seq.targetdata,temp.chunk)
    }
  }
  new.seq.targetdata$ord=1:nrow(new.seq.targetdata)
  return(new.seq.targetdata)
}
tmp.result.table=reord_graphdat(ls.cate = unique(tmp.result.table$category),tmp.result.table)
tmp.result.table$category=
  factor(tmp.result.table$category, 
         levels = unique(tmp.result.table$category))



# Make the figure ---------------------------------------------------------
fig.dat=tmp.result.table
fig.dat$sig = 99999
fig.dat$sig[fig.dat$p.value<0.05]='*'
fig.dat$sig[fig.dat$sig==99999]=''
cl.theme=c('orangered1','slategray3','orange1',
           'mediumpurple1','royalblue3','lightseagreen','maroon',
           'salmon2','palevioletred2','olivedrab','darkslategray3')
fig=
  ggplot(fig.dat, 
         aes(x=reorder(dependent,-ord), y=beta,fill=category)) +
  geom_bar(stat="identity", width = 0.5, position=position_dodge())+
  geom_errorbar(aes(x=reorder(dependent,-ord), 
                    ymin=beta-std, ymax=beta+std), width=0.2, colour="grey", alpha=0.9, size=0.4)+
  scale_fill_manual(values = cl.theme)+
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank(),
        #legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey"),
        axis.line.x = element_line(colour = 'grey',size=1),
        axis.line.y = element_blank(),
        plot.title=element_text(lineheight = 1,face='bold',hjust=0.5))+
  geom_hline(yintercept=0 , color = "grey", size=0.5)+
  geom_text(aes(label=sig), colour="black", hjust=10, size=3)+
  xlab('Phenotype')+
  ylab('Standardised effect size')+
  guides(fill=guide_legend(title="Category"))+
  coord_flip()


# Save file ---------------------------------------------------------------

png("Figs/SuppleInfo/meanSymptoms_beta_bargraph.png", width = 10, height = 3, units = 'in', res = 300)
fig # Make plot
dev.off()
