groot=readRDS('/sdata/images/projects/UKBIOBANK/ABCD/release2.0/iii.data/ABCDgenomic/genomics_sample03.rds')
pheno=data.frame(groot$subjectkey,round(runif(nrow(groot),min = 0,max = 1),digits = 0))
write.table(pheno,file = '/sdata/images/projects/UKBIOBANK/users/Shen/iv.ABCD/release2.0/FamilialRisk_PGRS_MDD/data/pheno.file',
            quote = F,sep = '\t',row.names = F,col.names = F)
