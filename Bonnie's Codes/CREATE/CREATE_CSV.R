## create csv files
# save file 
setwd("~/Desktop/Database/csv files/u01_tom_jhou")
write.csv(WFU_Jhou_test_df, file = "mastertable_c01_16_jhou.csv", row.names = F) 
write.csv(jhou_17_wfu_metadata, file = "jhou_17_wfu_metadata.csv", row.names = F)
write.csv(jhou_18_wfu_metadata, file = "~/Desktop/Database/csv files/u01_tom_jhou/jhou_18_wfu_metadata.csv", row.names = F)

# phenotypes
write.csv(progpun_gwas, "progpun_gwas_C1_16.csv", row.names = F) # done
write.csv(prograt_gwas, "prograt_gwas_C1_16.csv", row.names = F) # done
write.csv(delayedpun_gwas, "delayedpun_gwas_C1_16.csv", row.names = F) # done 
write.csv(locomotor_gwas, "locomotor_gwas_C1_16.csv", row.names = F) # done but is the messiest  
write.csv(runway_gwas, "runway_gwas_C1_16.csv", row.names = F) # done 
