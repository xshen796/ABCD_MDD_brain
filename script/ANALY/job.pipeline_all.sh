#!/bin/sh
#$ -N ABCD
#$ -cwd
#$ -m beas
#$ -M xueyi.shen@ed.ac.uk
#$ -l h_vmem=32G
#$ -l h_rt=2:00:00
. /etc/profile.d/modules.sh
source ~/.bash_profile

module unload R
module add igmm/apps/R/3.6.1

cd /exports/igmm/eddie/GenScotDepression/shen/ActiveProject/ImagingProject/ABCD_MDD_brain/


#R CMD BATCH script/ANALY/ANALY.YouthDepression_to_WM_subcor_corticalFS.R
#R CMD BATCH script/ANALY/ANALY.YouthDepression_brain_covParentDepre.R
#R CMD BATCH script/ANALY/ANALY.DiffsYouthDepression_to_env.R
#R CMD BATCH script/ANALY/ANALY_supple.YouthDepression_brain_site.R
#R CMD BATCH script/ANALY/ANALY_supple.YouthDepression_brain_covMRImanufacturer.R
#R CMD BATCH script/ANALY/ANALY_supple.YouthDepression_brain_covMedication.R
#R CMD BATCH script/ANALY/ANALY_supple.DiffsYouthDepression_to_brain.R
#R CMD BATCH script/ANALY/ANALY_supple.YouthDepression_brain_PostProcessingQC.R
R CMD BATCH script/ANALY/ANALY_supple.meanYouthDepression_to_WM_corticalFS.R
R CMD BATCH script/ANALY/ANALY_supple.DiffsYouthDepression_to_brain.R

