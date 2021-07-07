library(dplyr)
library(ggplot2)
library(data.table)

setwd('/exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/Adolescent_MDD_brain')
load('result/x.Supplementary_materials/summ_ItemDiscre.RData')


# Make proportion bar plot ----------------------------------------------------------------------------------------

targetdata = summ.ItemDiscre %>%
      select(-N.total)%>%
      .[!grepl('_Present.p_y',.$PhenoName),] %>%
      mutate_if(is.numeric,function(x) x*100) %>%
      mutate(Item = gsub('_Present.p_y','',PhenoName)) %>%
      mutate(Item = gsub('_Ever.p_y','',Item)) %>%
      mutate(Item = gsub('KSADS.items.','',Item)) %>%
      mutate(Item = gsub('_',' ',Item)) %>%
      mutate(Item = gsub('\\.',' ',Item)) 


# bar plot for proportions of p>y, y>p and no diffs
      
fig.dat = targetdata  %>% 
      .[order(.$proportion.diff,decreasing = T),] %>%
      select(-proportion.diff,-PhenoName) %>%
      mutate(Item = factor(Item,levels=Item)) %>%
      melt(.,id.vars='Item',measure.vars=c('proportion.P_over_Y','proportion.Y_over_P','proportion.nodiff')) %>%
      mutate(variable = gsub('proportion.P_over_Y','Caregiver > Child',variable)) %>%
      mutate(variable = gsub('proportion.Y_over_P','Child > Caregiver',variable)) %>%
      mutate(variable = gsub('proportion.nodiff','Caregiver = Child',variable)) %>%
      mutate(Type=variable,`Proportion (%)`=value) 

fig.bar = ggplot(fig.dat, aes(fill=Type, y=`Proportion (%)`, x=Item)) + 
      geom_bar(position="stack", stat="identity")+
      coord_flip()

png(file="Figs/SuppleInfo/Items_Discre.png",
     width=24, height=15, units='cm', res=200)
fig.bar
dev.off()