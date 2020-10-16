## create csv files
# save file 
setwd("~/Desktop/Database/csv files/u01_tom_jhou")
write.csv(WFU_Jhou_test_df, file = "mastertable_c01_16_jhou.csv", row.names = F) 
write.csv(jhou_17_wfu_metadata, file = "jhou_17_wfu_metadata.csv", row.names = F)
