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
  merge(x = ., y = rfid[ , c("labanimalid", "sex", "dob")], by = "labanimalid", all.x=TRUE) %>% 
  mutate(experimentage = as.numeric(runway_weight$date - runway_weight$dob),
         cohort = stringr::str_match(filename, "Cohort \\d+"))

ggplot(runway_weight %>% filter(sex != "```", experimentage > 0), aes(x = experimentage, y = weight)) +
  geom_jitter(aes(color = sex)) + 
  facet_wrap( ~ cohort)

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


# CLEAN UP FILENAME.X VS FILENAME.Y HERE
runway_boxes <- lapply(runwayfiles_clean, readrunwayboxes) %>% rbindlist(., fill = T) 
runway_boxes <- runway_boxes %>% 
  rename("boxstation" = "V1", 
         "boxstationnumber" = "V2") %>% 
  mutate(labanimalid = stringr::str_extract(filename.x, "U[[:digit:]]+[[:alpha:]]*",),
         cohort = stringr::str_match(filename, "Cohort \\d+")) %>% 
  merge(x = runway, y = ., by = "labanimalid", all.x=TRUE) 
# # check if boxes are being used by different sexes within a cohort 

boxqc_bycohort <- runway_boxes %>% group_by(cohort, boxstationnumber, boxstation) %>% count
