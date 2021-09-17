## temporary 
# create tables for summer students

# compromised tables 
Jhou_SummaryAll %>% subset(!(is.na(notesforhumans)|is.na(resolution))) %>% naniar::vis_miss()
Jhou_compromised <- Jhou_SummaryAll %>% 
  subset(!(is.na(notesforhumans)|is.na(resolution))) %>% 
  select(rfid, notesforhumans, resolution)

write.csv(Jhou_compromised, "~/Desktop/Database/csv files/u01_tom_jhou/compromised_rats_summer.csv", row.names = F)


# phenotypes 
# just the first in the experiments

# runway 
Jhou_Runway_xl_summer <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 26, 33:36, 60:61)]
names(Jhou_Runway_xl_summer) <- Jhou_Runway_xl_summer[1,] %>% unlist() %>% as.character() %>% janitor::make_clean_names()

Jhou_Runway_xl_summer <- Jhou_Runway_xl_summer %>% 
  rename("rfid" = "x16_digit_id",
         "jhou_cohort" = "shipment_cohort",
         "date" = "date_of_first_runway_test",
         "labanimalid" = "jhou_lab_id",
         "comments" = "notes_for_humans") %>% 
  mutate(cohort = paste0("C", str_pad(gsub("[.].*", "", jhou_cohort), 2, "left", "0")),
         jhou_cohort = as.character(as.numeric(jhou_cohort)),
         comments = replace(comments, grepl("Yellow", comments), NA)) %>% 
  mutate_at(vars(matches("runway")), as.numeric)

Jhou_Runway_xl_summer_df <- Jhou_Runway_xl_summer %>% mutate(labanimalid = toupper(labanimalid)) %>% 
  subset(grepl("U\\d+", labanimalid)&!is.na(rfid)) %>% 
  select(-c(jhou_cohort, cohort)) %>% 
  select(-c(comments, resolution))
# %>% 
  # mutate(cohort = replace(cohort, grepl("NA", cohort), NA)) %>% 
  # left_join(WFU_Jhou_test_df[, c("cohort", "rfid", "sex", "dob")], by = "rfid") %>% 
  # mutate(cohort = coalesce(cohort.x, cohort.y)) %>% 
  # mutate(runway_binary = as.character(runway_binary)) %>% 
  # select(-cohort.x, -cohort.y) %>% 
  # select(cohort, jhou_cohort, labanimalid, rfid, everything(), comments, resolution) %>% 
  # select(-date)

write.csv(Jhou_Runway_xl_summer_df, "~/Desktop/Database/csv files/u01_tom_jhou/phenotypes_summer.csv", row.names = F)

### drop runway phenotypes and summer phenotypes 
# upload new dataset into AWS 

# dbSendQuery(con,"drop table \"u01_tom_jhou\".\"phenotypes_summer\";") # these lines don't work, do in pgadmin
dbSendQuery(con, "CREATE TABLE \"u01_tom_jhou\".\"runway_phenotypes\" (
	cohort VARCHAR(3) NOT NULL, 
	jhou_cohort text NOT NULL, 
	rfid VARCHAR(15) NOT NULL, 
	labanimalid VARCHAR(4) NOT NULL, 
	sex VARCHAR(1) NOT NULL, 
	comments text, 
	resolution text, 
	avg_4_last_na DECIMAL, 
	latency_cat_100 BOOLEAN, 
	latency_cat_150 BOOLEAN, 
	latency_cat_200 BOOLEAN, 
	latency_cat_250 BOOLEAN, 
	latency_cat_300 BOOLEAN, 
	age_1 DECIMAL, 
	avg_4_last_na_excel DECIMAL, 
	latency_cat_excel BOOLEAN,
	PRIMARY KEY(rfid),
	CONSTRAINT compromised_rfid_fkey FOREIGN KEY (rfid) REFERENCES u01_tom_jhou.wfu_master(rfid)
);")

table_ID <- Id(schema = "u01_tom_jhou", table = "runway_phenotypes")
dbWriteTable(conn = con, name = table_ID, 
             value = read.csv("~/Desktop/Database/csv files/u01_tom_jhou/runway_gwas_C1_16.csv", stringsAsFactors = F), append = T)

# # done in pgadmin - 
# CREATE TABLE u01_tom_jhou.phenotypes_summer AS 
# SELECT
# *
#   FROM
# u01_tom_jhou.runway_phenotypes;


dbDisconnect(con)





