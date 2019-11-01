# RAW QC AND EDITS 

library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)

################################
########### Runway #############
################################

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


runway_boxes <- lapply(runwayfiles_clean, readrunwayboxes) %>% rbindlist(., fill = T) 
runway_boxes_df <- runway_boxes %>% 
  rename("boxstation" = "V1", 
         "boxstationnumber" = "V2") %>% 
  mutate(labanimalid = stringr::str_extract(filename, "U[[:digit:]]+[[:alpha:]]*"),
         cohort = stringr::str_match(filename, "Cohort \\d+"),
         boxstation = paste(boxstation, boxstationnumber),
         boxstation = replace(boxstation, boxstation == "NA NA", NA)) %>% 
  merge(x = runway, y = ., by = "filename", all.x=F) %>% 
  select(-c(sex,boxstationnumber)) %>% # replace sex information from wfu (because ``` existence makes me question the validity of the data `) 
  left_join(., y = WFU_Jhou_test_df[, c("labanimalnumber", "sex")], by = c("wakeforestid" = "labanimalnumber")) # WFU_Jhou_test_df from u01_qc from WFU github

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
