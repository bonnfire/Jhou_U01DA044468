## create csv files
# save file 
setwd("~/Desktop/Database/csv files/u01_tom_jhou")
write.csv(WFU_Jhou_test_df, file = "mastertable_c01_16_jhou.csv", row.names = F) 
write.csv(jhou_17_wfu_metadata, file = "jhou_17_wfu_metadata.csv", row.names = F)
write.csv(jhou_18_wfu_metadata, file = "~/Desktop/Database/csv files/u01_tom_jhou/jhou_18_wfu_metadata.csv", row.names = F)
write.csv(jhou_19_wfu_metadata, file = "~/Desktop/Database/csv files/u01_tom_jhou/jhou_19_wfu_metadata.csv", row.names = F)


# phenotypes
write.csv(progpun_gwas, "progpun_gwas_C1_16.csv", row.names = F) # done
write.csv(prograt_gwas, "prograt_gwas_C1_16.csv", row.names = F) # done
write.csv(delayedpun_gwas, "delayedpun_gwas_C1_16.csv", row.names = F) # done 
write.csv(locomotor_gwas, "locomotor_gwas_C1_16.csv", row.names = F) # done but is the messiest  
write.csv(runway_latency_spread_failedhab, "~/Desktop/Database/csv files/u01_tom_jhou/runway_gwas_C1_16.csv", row.names = F) # done 

## meeting 01/13/2021
runway_latency_spread_failedhab %>% select(matches("cat")) %>% mutate_all(as.factor) %>% summary()
runway_latency_spread_failedhab %>% select(matches("cat")) %>% 
  mutate_all(as.numeric) %>% 
  summarise_all(~sum(!is.na(.))) 
ggplot(runway_latency_spread_failedhab, aes(x = avg_4_last_na, y = avg_4_last_na_excel)) + geom_point() 
ggplot(runway_latency_spread_failedhab, aes(x = avg_4_last_na, y = avg_4_last_na_excel, color = cohort)) + geom_point() 
runway_latency_spread_failedhab %>% mutate(diff = avg_4_last_na_excel-avg_4_last_na) %>% ggplot()+geom_histogram(aes(x = diff))
runway_latency_spread_failedhab %>% mutate(diff_abs = abs(avg_4_last_na_excel-avg_4_last_na)) %>% subset(diff_abs > 2) %>% 
  left_join()

runway_latency_spread_failedhab %>% mutate(diff_abs = abs(avg_4_last_na_excel-avg_4_last_na)) %>% subset(diff_abs > 2) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Jhou_U01DA044468/Bonnie's Codes/QC/runway_latencies_discr.xlsx")
