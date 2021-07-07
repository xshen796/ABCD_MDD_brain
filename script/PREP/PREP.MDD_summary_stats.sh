cd /exports/igmm/eddie/GenScotDepression/shen/PGRS/MDD_PRS_ABCD/

awk '{print $1,$2,$3,$8,$9,$10}' /exports/igmm/eddie/GenScotDepression/shen/MDD_meta/mdd_meta_Shen/23andmePGCUKBnoMRI_20181028_shen_3cohorts.meta > pre.prsice.meta

sed -i -e '1s/MarkerName/SNP/' -e '1s/Allele1/A1/' -e '1s/Allele2/A2/' -e '1s/Effect/BETA/' -e '1s/StdErr/SE/' -e '1s/P-value/P/' pre.prsice.meta
awk '$2 = toupper($2)' pre.prsice.meta > basedat.meta.temp
awk '$3 = toupper($3)' basedat.meta.temp > 23andmePGCUKBnoMRI_20181028_shen_3cohorts.meta.forPRS

rm pre.prsice.meta basedat.meta.temp
