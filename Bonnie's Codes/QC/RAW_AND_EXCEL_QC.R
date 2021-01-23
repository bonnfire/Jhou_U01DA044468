# RAW QC AND EDITS 

library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)



################################
########### Runway #############
################################
jhou_allexps_df <- list() %>% 
  rbindlist()

################################
########### Runway #############
################################

runway_gwas <- Jhou_Runway_xl_df %>%
  subset(!cohort == "C17") %>%  #exclude bc not phenotyped yet (as of 10/16/2020)
  left_join(runway_date, by = "labanimalid") %>%
  mutate_at(vars(ends_with("date")), ~ difftime(., dob, units = "days") %>% as.numeric) %>% # probably will need manually editing the values that are missing dates/missing files
  select(-dob) %>% 
  rename_at(vars(ends_with("date")), ~ gsub("date", "age", .))

# runway_gwas %>% subset(!is.na(mean_shock_breakpoint_ma)&is.na(mean_shock_breakpoint_ma_age)) # rows where there are data but no exp age

# add date for age 
runway_date <- runwayfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>%
  mutate(labanimalid = str_extract(filename, "U\\d+"),
         date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>%
  distinct() %>%
  group_by(labanimalid) %>%
  slice(4) %>% # just get the first day of locomotor from the filename
  ungroup() %>% 
  select(-filename) %>% 
  rename("runway_4_date" = "date") 
# %>%   # remove the join because the runway phenotype is only down to one
#   left_join(runwayfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>%
#               mutate(labanimalid = str_extract(filename, "U\\d+"),
#                      date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>%
#               distinct() %>%
#               group_by(labanimalid) %>%
#               slice(8) %>% # just get the first day of locomotor from the filename
#               ungroup() %>% 
#               select(-filename) %>% 
#               rename("runway_8_date" = "date"),
#             by = "labanimalid")

# deal with the exclude cases 
runway_gwas <- runway_gwas %>% 
  mutate_if(is.numeric, ~ replace(., grepl("EXCLUDE_(RUNWAY|ALL)", resolution), NA)) %>% # if flagged, make na
  select(cohort, jhou_cohort, labanimalid, rfid, sex, comments, resolution, runway_latency_avg_sessions_4_7_seconds, runway_latency_avg_sessions_8_12_seconds, runway_4_age, runway_8_age) # ensure order for db



## join to jhou excel session by session
full_join(jhou_r_sessions_runway, runway_latency_c01_16_7_df %>% 
            mutate(filename = gsub(".*/", "", filename)), by = c("file_jhou" = "filename")) %>% 
  mutate(run_latency_raw_jhou_round = round(run_latency_raw_jhou)) %>% 
  subset(run_latency_raw_jhou_round != latency & latency != 900) %>% 
  subset(parse_number(cohort) < 17 & abs(run_latency_raw_jhou_round - latency)!= 1) %>% 
  select(labanimalid, "filename" = file_jhou, run_latency_raw_jhou_round, "latency_raw_bonnie" = latency, start_latency_raw_jhou, "location_2_bonnie" = location_2, goal_latency_raw_jhou, "reached_bonnie" = "reached") %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Jhou_U01DA044468/Bonnie's Codes/QC/runway_sessions_qc_n13.xlsx")
  



















### XX RETURN TO CLEANING DATA AFTER THE CONFERENCE




# gives counts of sessions counts 
# runway %>% group_by(labanimalid) %>% count() %>% group_by(n) %>% count()

# add more columns

# check for duplicates 

# # by filename
any(duplicated(runway$filename)) # FALSE
runway[duplicated(runway$filename),] # 0 observations
# # by weight 
## create weight table

setwd("~/Dropbox (Palmer Lab)/U01 folder/Runway")
readrunwayweight <- function(x){
  weight <- fread(paste0("awk '/WEIGHT/{print $4}' ", "'", x, "'"))
  weight$filename <- x
  return(weight)
}

runway_weight <- lapply(runwayfiles_clean, readrunwayweight) %>% rbindlist(., fill = T) 
runway_weight <- runway_weight %>% 
  rename("weight" = "V1") %>% 
  extractfromfilename() %>%
  merge(x = ., y = rfidandid[ , c("labanimalid", "sex", "dob")], by = "labanimalid", all.x=TRUE) %>% 
  mutate(experimentage = as.numeric(date - dob),
         cohort = stringr::str_match(filename, "Cohort \\d+"))

ggplot(runway_weight %>% filter(sex != "```", experimentage > 0), aes(x = experimentage, y = weight)) +
  geom_jitter(aes(color = sex), size = 0.5) + 
  facet_wrap( ~ cohort)

# # generate information for email about outliers 
outliers <- runway_weight %>% filter(sex == "```" | experimentage < 0 | weight < 100)
# ggplot(outliers) + geom_histogram(aes(x = weight))
# generate for email outliers %>% group_by(labanimalid, cohort) %>% count() %>% print.data.frame() 

# extract cohort information from directories

not7or12<- runway %>% 
  mutate(cohort = stringr::str_match(filename, "Cohort \\d+\\.*\\d*")) %>% 
  group_by(labanimalid, cohort) %>% 
  count() %>% 
  filter(n!=7, n !=12) 

# accurate cohort information  

cohort <- runway %>% 
  select(labanimalid, filename) %>% 
  mutate(cohort = stringr::str_match(filename, "Cohort \\d+\\.*\\d*")) %>% 
  distinct(labanimalid, cohort) %>% 
  filter(!((labanimalid %in% c("U311", "U312", "U313") & cohort =="Cohort 8.2") | labanimalid == "U314" & cohort == "Cohort 8.3")) %>% select(labanimalid,cohort) # corrects the misassignment, extracted correct value from the excel sheet 
# cohort$labanimalid[duplicated(cohort$labanimalid)] # three of these cases should be assigned to 8.3 and one should be 8.2 

not7or12wnotes <- left_join(not7or12, tJhou_Runway_notes, by = c("labanimalid" = "animalid"))

# observations from the plots
# outlier in cohort 7
summary(runway$elapsedtime)
runway %>% filter(elapsedtime > 1500)
# unique resolutions are not very specific to runway data

# # add raw box information to raw files 
setwd("~/Dropbox (Palmer Lab)/U01 folder/Runway")
readrunwayboxes <- function(x){
  boxes <- fread(paste0("awk '/Started/{print $(NF-1) \" \" $NF}' ", "'", x, "'"))
  boxes$filename <- x
  return(boxes)
}


runway_boxes <- lapply(runwayfiles_clean_c01_16, readrunwayboxes) %>% rbindlist(., fill = T) 
runway_boxes_df <- runway_boxes %>% 
  rename("boxstation" = "V1", 
         "boxstationnumber" = "V2") %>% 
  mutate(labanimalid = stringr::str_extract(filename, "U[[:digit:]]+[[:alpha:]]*"),
         # cohort = stringr::str_match(filename, "Cohort \\d+"),
         boxstation = paste(boxstation, boxstationnumber),
         boxstation = replace(boxstation, boxstation == "NA NA", NA)) %>% 
  # merge(x = runway, y = ., by = "filename", all.x=F) %>% 
  # select(-c(sex,boxstationnumber)) %>% # replace sex information from wfu (because ``` existence makes me question the validity of the data `) 
  left_join(., y = Jhou_SummaryAll[, c("wfucohort", "labanimalid", "sex")], by = "labanimalid") %>%  # WFU_Jhou_test_df from u01_qc from WFU github
  rename("cohort" = "wfucohort")
# make sense of box vs station 
runway_boxes_df %>% distinct(boxstation, labanimalid) %>% subset(grepl("station", boxstation))
# # check if boxes are being used by different sexes within a cohort 

boxqc_bycohort <- runway_boxes_df %>% group_by(cohort, boxstation, sex) %>% count()

ggplot(boxqc_bycohort, aes(x = boxstation, y = n, color = sex)) + geom_point() + facet_grid(. ~ cohort) + theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### COMPARING THE RAW AND EXCEL FILES 
# create a subset of data for which the columns don't match 
# # create column by column comparison and generate a column that indicates whether or not they match 9DON'T NEED BC OF ANTI JOIN)
# for(i in 1:nrow(joinrawtoexcel)){
# 
#       i = 1
#       joinrawtoexcelcols_test <- paste('comparison', i, sep= '')
#       joinrawtoexcel[[joinrawtoexcelcols_test]] <- ifelse(select(test, onlymins_excel[i]) != select(test, onlymins_raw[i]), 1, 0)
#       joinrowtoexcel[[test_anyfalse]] <- ifelse()
# }

################################
### ###### Locomotor #### ######
################################

locomotor_gwas <- Jhou_Locomotor_xl_df %>%
  subset(!cohort == "C17") %>%  #exclude bc not phenotyped yet (as of 10/16/2020)
  left_join(locomotor_date, by = "labanimalid") %>% 
  mutate_at(vars(ends_with("date")), ~ difftime(., dob, units = "days") %>% as.numeric) %>% # probably will need manually editing the values that are missing dates/missing files
  select(-dob) %>% 
  rename_at(vars(ends_with("date")), ~ gsub("date", "age", .))


# add date for age 
locomotor_date <- locomotorfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>% 
  mutate(labanimalid = str_extract(filename, "U\\d+"),
         date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>% 
  select(-filename) %>% distinct() %>% 
  group_by(labanimalid) %>% 
  slice(1) %>% # just get the first day of locomotor from the filename 
  ungroup()  %>% 
  rbind(locomotorfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>% 
          mutate(labanimalid = str_extract(filename, "U\\d+"),
                 date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>% 
          select(-filename) %>% distinct() %>% 
          group_by(labanimalid) %>% top_frac(.5, date) %>% top_n(1, date) %>% ungroup()) %>% 
  distinct() %>% 
  group_by(labanimalid) %>% 
  mutate(session = paste0("locomotor_", row_number(labanimalid), "_date")) %>% 
  ungroup() %>% 
  subset(!is.na(labanimalid)) %>%
  spread("session", "date") %>% 
  rowwise() %>% 
  mutate(locomotor_2_date = replace(locomotor_2_date, parse_number(labanimalid) < 331,locomotor_1_date), 
         locomotor_1_date = replace(locomotor_1_date, parse_number(labanimalid) < 331, NA)) %>%  # animals before U331 only had one locomotor session, after food dep
  ungroup()


## XX ask about these cases 
locomotor_gwas %>% subset(!is.na(locomotor_2_after_food_dep_photobeam_counts)&is.na(locomotor_2_age)) %>% View() 

  # left_join(Jhou_SummaryAll[, c("labanimalid", "rfid", "wfucohort")], by = "labanimalid")

# manage the resolutions/comments
locomotor_gwas <- locomotor_gwas %>% 
  mutate_if(is.numeric, ~ replace(., grepl("EXCLUDE_(LOCOMOTOR|ALL)", resolution), NA)) %>% # if flagged, make na
  select(cohort, jhou_cohort, labanimalid, rfid, sex, comments, resolution, locomotor_1_before_food_dep_photobeam_counts, locomotor_2_after_food_dep_photobeam_counts, locomotor_1_age, locomotor_2_age) # ensure order for db








### XX RETURN TO CLEANING DATA AFTER THE CONFERENCE
















rawhasbutnotexcel <- anti_join(rawfiles_locomotor_wide %>% dplyr::filter(!grepl("LOCOMOTOR", resolution)), Jhou_Locomotor %>% mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)), by = onlymins) # 34 cases

excelhasbutnotraw <- anti_join(Jhou_Locomotor %>% mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)), rawfiles_locomotor_wide, by = onlymins) # 13 cases

# variable by variable 
locomotor_nonmatches_excelhasbutnotraw <- list()
for(i in 1:length(onlymins)){
  locomotor_nonmatches_excelhasbutnotraw[[i]] <- anti_join(Jhou_Locomotor %>% mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)), rawfiles_locomotor_wide_clean__split_session, by = c("labanimalid", "session", onlymins[i])) %>% dplyr::select(labanimalid)
  names(locomotor_nonmatches_excelhasbutnotraw)[i] <- onlymins[i]
}
locomotor_nonmatches_excelhasbutnotraw %>% 
  rbindlist(idcol = 'minute') %>% 
  mutate(idnum = as.numeric(str_extract(labanimalid,"\\d+"))) %>% 
  arrange(idnum) %>% 
  select(-idnum) %>% 
  mutate(labanimalid = factor(labanimalid, levels = unique(labanimalid))) %>% # make levels to preserve order for split
  split(., .$labanimalid) #sent to Jhou's lab

test[as.numeric(test$shipmentcohort_excel) > 8, ][onlymins[-31]]
test <- setdiff(x = test[which(as.numeric(test$shipmentcohort_excel) > 8), 2:31], y = test[which(as.numeric(test$shipmentcohort_excel) > 8), 46:75])

# for email
# generate the missing files again 
# row exists in raw but not in excel (by ID)
anti_join(rawfiles_locomotor_wide, Jhou_Locomotor %>% mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)), by = 'labanimalid')
# row exists in raw but not in excel (by file)

# row exists in raw but not in excel (by session); mostly excel spaceholder
anti_join(rawfiles_locomotor_wide,Jhou_Locomotor_Excel_graph,  by = c("labanimalid", "session")) %>% tail(33-6) %>% select(labanimalid) %>% unique()

# generate the lowercase filename
rawfiles_locomotor_wide[grep("u", rawfiles_locomotor_wide$filename),]
# generate the mislabelled files
rawfiles_locomotor_wide %>% 
  mutate(labanimalidU = gsub('/u', '/U', filename), 
         labanimalidU = str_extract(labanimalidU, '(U[[:digit:]]+)'), 
         labanimalid_ = str_extract(filename, '(_[[:digit:]]+)'),
         labanimalid_ = gsub('_', 'U', labanimalid_)) %>% 
  dplyr::filter(labanimalidU != labanimalid_)

# generate the all zeroes files and the na files
rawfiles_locomotor_wide %>%
  dplyr::filter(minute1 == 0 & minute2 == 0 | is.na(minute3)) # 717 cases

# generate the excel cells that are supposed to be na's
Jhou_Locomotor %>% dplyr::filter(labanimalid %in% res$labanimalid) %>% select(labanimalid, session, minute30) # res is created in raw to know how to deal with na cases 


# check the ages of the animals within cohort
rawfiles_locomotor_wide %>% 
  group_by(shipmentcohort) %>%
  summarize(expagemean = mean(experimentage),
            expagemean = median(experimentage),
            expagemin = min(experimentage),
            expagemax = max(experimentage)) %>% 
  View()

ggplot(rawfiles_locomotor_wide, aes(shipmentcohort, experimentage)) + geom_boxplot()
# check if the cohorts were misassigned in cohort4; extract the date and time from the raw files; check for subset of the animals for protocol changes(using the min date)


rawfiles_locomotor_wide %>% dplyr::filter(shipmentcohort == "3.1")


## updated data from uploaded data (1/6)
Jhou_Raw_Locomotor



################################
#### Progressive Punishment ####
################################
progpun_gwas <- Jhou_ProgPunishment_xl_df %>%
  subset(!cohort == "C17") %>%  #exclude bc not phenotyped yet (as of 10/16/2020)
  left_join(progpun_date, by = "labanimalid") %>% 
  mutate(date = replace(date, 
                        labanimalid %in% c("U505", "U506", "U507", "U508", "U510", "U512", "U513", "U514", "U515", "U516", "U518", "U519"), 
                        as.Date("2019-12-13")),
         date = replace(date, 
                        labanimalid %in% c("U511", "U517", "U520"), 
                        as.Date("2019-12-12")),
         date = replace(date, 
                        labanimalid %in% c("U588"), 
                        as.Date("2020-03-27")),
         date = replace(date, 
                        labanimalid %in% c("U607"), 
                        as.Date("2020-05-07")),
         date = replace(date, 
                        labanimalid %in% c("U633", "U635", "U636", "U637", "U639"), 
                        as.Date("2020-08-28")),
         date = replace(date, 
                        labanimalid %in% c("U634", "U638"), 
                        as.Date("2020-08-31"))) %>%  # XX manually edit and note to jhou lab that there are missing raw files  
  mutate(mean_shock_breakpoint_ma_age = difftime(date, dob, units = "days") %>% as.numeric) %>% 
  select(-dob, -date)

progpun_gwas %>% subset(!is.na(mean_shock_breakpoint_ma)&is.na(mean_shock_breakpoint_ma_age)) # rows where there are data but no exp age

# add date for age 
progpun_date <- progpunfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>% 
  mutate(labanimalid = str_extract(filename, "U\\d+"),
         date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>% 
  select(-filename) %>% distinct() %>% 
  group_by(labanimalid) %>% 
  slice(1) %>% # just get the first day of progpun from the filename 
  ungroup()  
  # left_join(Jhou_SummaryAll[, c("labanimalid", "rfid", "wfucohort")], by = "labanimalid")


# change df based on resolution
progpun_gwas <- progpun_gwas %>% 
  mutate(resolution = replace(resolution, resolution == "601", NA)) %>% # manual change
  mutate_if(is.numeric, ~ replace(., grepl("EXCLUDE_(PROGRESSIVE_PUNISHMENT|ALL_BEHAVIORS)", resolution), NA)) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, sex, box, comments, resolution, mean_shock_breakpoint_ma, mean_shock_breakpoint_ma_age)
  




### XX RETURN TO CLEANING DATA AFTER THE CONFERENCE

# using plots to clean data
# outlier on the numtrials (mistyped 32 on the Excel sheet)
joinrawtoexcel_progpunishment[which(joinrawtoexcel_progpunishment$numtrialsatlastshock_excel == 32),]$numtrialsatlastshock_excel = joinrawtoexcel_progpunishment[which(joinrawtoexcel_progpunishment$numtrialsatlastshock_excel == 32),]$numtrialsatlastshock_raw
joinrawtoexcel_progpunishment[which(joinrawtoexcel_progpunishment$shockoflastattemptedblock_excel == 125),]$shockoflastattemptedblock_excel = joinrawtoexcel_progpunishment[which(joinrawtoexcel_progpunishment$shockoflastattemptedblock_excel == 125),]$shockoflastattemptedblock_raw

# show cases for which the excel data differ from raw for....
# shocklastcompletedblock
joinrawtoexcel_progpunishment %>% dplyr::filter(shockoflastcompletedblock_raw != shockoflastcompletedblock_excel) %>% select(labanimalid, filename, session)
# shocklastattemptedblock

# numtrials

# activepresses

# inactivepresses
joinrawtoexcel_progpunishment %>% dplyr::filter(inactivepresses_raw != inactivepresses_excel) %>% select(labanimalid, filename, session, inactivepresses_excel, inactivepresses_raw)


################################
###### Delayed Punishment ######
################################

delayedpun_gwas <- Jhou_DelayedPunishment_xl_df %>% 
  subset(!cohort == "C17") %>%  #exclude bc not phenotyped yet (as of 10/16/2020)
  left_join(delayedpun_date, by = "labanimalid") %>%  # XX not doing this until asked manually edit and note to jhou lab that there are missing raw files
  mutate_at(vars(ends_with("date")), ~ difftime(., dob, units = "days") %>% as.numeric) %>% 
  rename_at(vars(ends_with("date")), ~ gsub("date", "age", .)) %>% 
  mutate_if(is.numeric, ~ replace(., grepl("EXCLUDE_DELAYED_PUNISHMENT", resolution), NA)) %>% # if flagged, make na
  select(cohort, jhou_cohort, labanimalid, rfid, sex, comments, resolution, mean_dp0_1_lastshock, mean_dp20_1_lastshock, mean_dp0_2_lastshock, mean_dp4540_2_lastshock, delay_0_1_age, delay_20_age, delay_0_2_age, delay_4540_age) # ensure order for db


# add date for age 
delayedpun_date <- Jhou_Excel[["Delayed punishment (DP)"]][, 1:2] %>%
  rename("delay" = "...1", 
         "date" = "...2") %>% 
  # mutate(labanimalid = "NA") %>%
  mutate(labanimalid = str_extract(delay, "U\\d+")) %>% 
  fill(labanimalid) %>% 
  mutate(delay = as.numeric(delay),
         date = openxlsx::convertToDate(date)) %>% 
  subset(!is.na(delay)) %>% 
  subset(delay %in% c(20, 40, 45)) %>% 
  mutate(delay = replace(delay, delay %in% c("40", "45"), "4540")) %>% 
  group_by(labanimalid, delay) %>% 
  slice(1) %>% 
  ungroup() %>% 
  subset(!(labanimalid == "U87"&delay=="4540")) %>% # manually checked, noted in excel that the 45 delay session was supposed to be 40s, used the 40 sec date
  mutate(delay = paste0("delay_", delay, "_date")) %>% 
  rbind(Jhou_Excel[["Delayed punishment (DP)"]][, 1:2] %>% #### FOR 0'S
          rename("delay" = "...1", 
                 "date" = "...2") %>% 
          mutate(labanimalid = str_extract(delay, "U\\d+")) %>% 
          fill(labanimalid) %>% 
          mutate(delay = as.numeric(delay),
                 date = openxlsx::convertToDate(date)) %>% 
          subset(!is.na(delay)) %>% 
          subset(delay == 0) %>% distinct() %>% group_by(labanimalid) %>% top_n(-1, date) %>% ungroup() %>% 
          rbind(Jhou_Excel[["Delayed punishment (DP)"]][, 1:2] %>% ### FOR SAMPLES THAT WENT THROUGH AS MANY AS 8 SESSIONS OF 0 DELAYS
                  rename("delay" = "...1", 
                         "date" = "...2") %>% 
                  mutate(labanimalid = str_extract(delay, "U\\d+")) %>% 
                  fill(labanimalid) %>% 
                  mutate(delay = as.numeric(delay),
                         date = openxlsx::convertToDate(date)) %>% 
                  subset(!is.na(delay)) %>% 
                  subset(delay == 0) %>% distinct() %>% group_by(labanimalid) %>% top_frac(.5, date) %>% top_n(1, date) %>% ungroup()) %>% 
          distinct() %>% 
          group_by(labanimalid) %>%
          mutate(session = row_number(labanimalid)) %>% 
          ungroup() %>% 
          mutate(delay = paste0("delay_0_", session, "_date")) %>% 
          select(-session)) %>% 
  # mutate(delay = paste0("delay", as.character(delay)))
  pivot_wider(names_from = "delay", 
              values_from = "date")
  
# delayedpun_date <- delayed_punishmentfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>% 
#   mutate(labanimalid = str_extract(filename, "U\\d+"),
#          date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>% 
#   select(-filename) %>% distinct() %>% 
#   group_by(labanimalid) %>% 
#   top_n(2, date) %>% # just get the first day of prograt from the filename 
#   ungroup() %>% 
#   rbind(delayed_punishmentfiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>% 
#           mutate(labanimalid = str_extract(filename, "U\\d+"),
#                  date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>% 
#           select(-filename) %>% distinct() %>% 
#           group_by(labanimalid) %>% 
#           top_n(-2, date) %>% # just get the first day of prograt from the filename 
#           ungroup()) %>% 
#   subset(!is.na(labanimalid)) %>% 
#   distinct()




################################
#### Progressive Ratio ####
################################
prograt_gwas <- Jhou_ProgRatio_xl_df %>%
  subset(!cohort == "C17") %>%  #exclude bc not phenotyped yet (as of 10/16/2020)
  left_join(prograt_date, by = "labanimalid") %>% 
  mutate(date = replace(date,
                        labanimalid %in% c("U505", "U506", "U507", "U508", "U510", "U512", "U513", "U514", "U515", "U516", "U518", "U519"),
                        as.Date("2019-12-17")),
         date = replace(date,
                        labanimalid %in% c("U511", "U517", "U520"),
                        as.Date("2019-12-16")),
         date = replace(date,
                        labanimalid %in% c("U542"),
                        as.Date("2019-12-16")),
         date = replace(date,
                        labanimalid %in% c("U633", "U635", "U636", "U637", "U639", "U640", "U642", "U643", "U644", "U645", "U646", "U647", "U648"),
                        as.Date("2020-09-01")),
         date = replace(date,
                        labanimalid %in% c("U634", "U638", "U641"),
                        as.Date("2020-09-02"))) %>%  # XX manually edit and note to jhou lab that there are missing raw files
  mutate(mean_leverpresses_maxratio_age = difftime(date, dob, units = "days") %>% as.numeric) %>% 
  select(-dob, -date)

prograt_gwas %>% subset(!is.na(mean_leverpresses_maxratio)&is.na(mean_leverpresses_maxratio_age)) # rows where there are data but no exp age

# add date for age 
prograt_date <- progratiofiles_clean %>% as.data.frame() %>% rename("filename" = ".") %>% 
  mutate(labanimalid = str_extract(filename, "U\\d+"),
         date = str_extract(filename, "\\d+-\\d+") %>% gsub("(-\\d{2})(\\d{2})", "\\1-\\2", .) %>% as.Date) %>% 
  select(-filename) %>% distinct() %>% 
  group_by(labanimalid) %>% 
  slice(1) %>% # just get the first day of prograt from the filename 
  ungroup()  

# change df based on resolution
prograt_gwas <- prograt_gwas %>% 
  mutate(resolution = replace(resolution, resolution == "601", NA)) %>% # manual change
  mutate_if(is.numeric, ~ replace(., grepl("EXCLUDE_(PROGRESSIVE_RATIO|ALL_BEHAVIORS)", resolution), NA)) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, sex, box, comments, resolution, mean_leverpresses_maxratio, mean_leverpresses_maxratio_age)

