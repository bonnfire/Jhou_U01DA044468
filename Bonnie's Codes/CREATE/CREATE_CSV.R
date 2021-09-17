## create csv files
# save file 
setwd("~/Desktop/Database/csv files/u01_tom_jhou")
write.csv(WFU_Jhou_test_df, file = "mastertable_c01_16_jhou.csv", row.names = F) 
write.csv(jhou_17_wfu_metadata, file = "jhou_17_wfu_metadata.csv", row.names = F)
write.csv(jhou_18_wfu_metadata, file = "~/Desktop/Database/csv files/u01_tom_jhou/jhou_18_wfu_metadata.csv", row.names = F)
write.csv(jhou_19_wfu_metadata, file = "~/Desktop/Database/csv files/u01_tom_jhou/jhou_19_wfu_metadata.csv", row.names = F)
write.csv(jhou_20_wfu_metadata, file = "~/Desktop/Database/csv files/u01_tom_jhou/jhou_20_wfu_metadata.csv", row.names = F)


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

## create gwas phenotypes table for db 
pheno_jhou <- dbGetQuery(conn = con, "select * from u01_tom_jhou.runway_phenotypes") %>% 
  setNames(tolower(gsub("(.*)", "runway_\\1", names(.)))) %>% 
  setNames(tolower(gsub("runway_(jhou_cohort|cohort|rfid|sex|labanimalid)", "\\1", names(.)))) %>% 
  full_join(dbGetQuery(conn = con, "select * from u01_tom_jhou.locomotor_phenotypes") %>% 
              setNames(tolower(gsub("(.*)", "locomotor_\\1", names(.)))) %>% 
              setNames(tolower(gsub("locomotor_locomotor_", "locomotor_", names(.)))) %>% 
              setNames(tolower(gsub("locomotor_(jhou_cohort|cohort|rfid|sex|labanimalid)", "\\1", names(.))))) %>% 
  full_join(dbGetQuery(conn = con, "select * from u01_tom_jhou.progressiveratio_phenotypes") %>% 
              setNames(tolower(gsub("(.*)", "progratio_\\1", names(.)))) %>% 
              setNames(tolower(gsub("progratio_(jhou_cohort|cohort|rfid|sex|labanimalid)", "\\1", names(.))))) %>% 
  full_join(dbGetQuery(conn = con, "select * from u01_tom_jhou.progressivepunishment_phenotypes") %>% 
              setNames(tolower(gsub("(.*)", "progpun_\\1", names(.)))) %>% 
              setNames(tolower(gsub("progpun_(jhou_cohort|cohort|rfid|sex|labanimalid)", "\\1", names(.))))) %>% 
  full_join(dbGetQuery(conn = con, "select * from u01_tom_jhou.delayedpunishment_phenotypes") %>% 
              setNames(tolower(gsub("(dp|delay)", "delayedpun", names(.)))) %>% 
              setNames(tolower(gsub("delayedpun(\\d)", "delayedpun_\\1", names(.)))) %>%
              setNames(tolower(gsub("__", "_", names(.)))) %>%
              setNames(tolower(gsub("(.*)(delayedpun_\\d+_\\d+_)(.*)", "\\2\\1\\3", names(.)))) %>% 
              rename("delayedpun_comments" = "comments", 
                     "delayedpun_resolution" = "resolution"))
pheno_jhou %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Tom_Jhou_U01DA044468_Dropbox_copy/generated/gwas_phenotypes.csv", row.names = F)

table_ID <- Id(schema = "u01_tom_jhou", table = "gwas_phenotypes")
dbWriteTable(conn = con, name = table_ID, value = read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Tom_Jhou_U01DA044468_Dropbox_copy/generated/gwas_phenotypes.csv", stringsAsFactors = F) %>% distinct, append = T)
