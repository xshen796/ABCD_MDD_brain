table.report=result.defs_totalscore[result.defs_totalscore$factor=='depression_bioMother_noManiaVision',]
table.report=table.report[,c(1,2,4,7)]
rownames(table.report)=table.report$dependent
table.report=table.report[c('KSADS.MDD.y','KSADS.Broad_depression.y',
                            'KSADS.Depressive_symptoms_ever.y',
                            'KSADS.MDD.p','KSADS.Broad_depression.p',
                            'KSADS.Depressive_symptoms_ever.p'),]


table.report=result.defs_totalscore[result.defs_totalscore$factor=='depression_bioFather_noManiaVision',]
table.report=table.report[,c(1,2,4,7)]
rownames(table.report)=table.report$dependent
table.report=table.report[c('KSADS.MDD.y','KSADS.Broad_depression.y',
                            'KSADS.Depressive_symptoms_ever.y',
                            'KSADS.MDD.p','KSADS.Broad_depression.p',
                            'KSADS.Depressive_symptoms_ever.p'),]

table.report=result.defs_totalscore[result.defs_totalscore$factor=='depression_parents_noManiaVision',]
table.report=table.report[,c(1,2,4,7)]
rownames(table.report)=table.report$dependent
table.report=table.report[c('KSADS.MDD.y','KSADS.Broad_depression.y',
                            'KSADS.Depressive_symptoms_ever.y',
                            'KSADS.MDD.p','KSADS.Broad_depression.p',
                            'KSADS.Depressive_symptoms_ever.p'),]

table.report=result.diffs[result.diffs$factor=='depression_bioMother_noManiaVision',]
table.report=table.report[,c(1,2,4,7)]
table.report$beta=round(table.report$beta,digits = 2)
rownames(table.report)=table.report$dependent
table.report=table.report[c('KSADS.MDD.diffs','KSADS.Broad_depression.diffs',
                            'KSADS.Depressive_symptoms_ever.diffs'),]

table.report=result.diffs[result.diffs$factor=='depression_bioFather_noManiaVision',]
table.report=table.report[,c(1,2,4,7)]
table.report$beta=round(table.report$beta,digits = 2)
rownames(table.report)=table.report$dependent
table.report=table.report[c('KSADS.MDD.diffs','KSADS.Broad_depression.diffs',
                            'KSADS.Depressive_symptoms_ever.diffs'),]

table.report=result.diffs[result.diffs$factor=='depression_parents_noManiaVision',]
table.report=table.report[,c(1,2,4,7)]
table.report$beta=round(table.report$beta,digits = 2)
rownames(table.report)=table.report$dependent
table.report=table.report[c('KSADS.MDD.diffs','KSADS.Broad_depression.diffs',
                            'KSADS.Depressive_symptoms_ever.diffs'),]



# Freesurfer summary measures ---------------------------------------------

result.report=result.fs[grep('mean.thk.APARC|total.sa.APARC|total.vol.APARC|mean.sulc.APARC',
                             result.fs$dependent),]
result.report=result.report[!duplicated(result.report$mod_name),]
result.report$beta=round(result.report$beta,digits = 3)
result.report.concat=cbind(result.report[9:12,c(4,7)],
                           result.report[5:8,c(4,7)],
                           result.report[13:16,c(4,7)])


result.report=result.fs[grep('mean.thk.APARC|total.sa.APARC|total.vol.APARC|mean.sulc.APARC',
                             result.fs$dependent),]
result.report=result.report[!duplicated(result.report$mod_name),]
result.report=result.report[!grepl('^CBCL|current',result.report$factor),]
result.report$beta=round(result.report$beta,digits = 3)
result.report.concat=cbind(result.report[9:12,c(4,7)],
                           result.report[5:8,c(4,7)],
                           result.report[13:16,c(4,7)])
