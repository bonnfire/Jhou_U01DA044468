setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Bonnie's Codes")
### FROM EXCEL 

library(stringr)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyxl)
library(readxl)

################################
##### Delayed punishment #######
################################

################################
####### Lever training #########
################################

# Complete dataframe for lever training
Lever_training_IDs <- list.dirs(path="Lever training",full.names=F)
Lever_training_IDs <- Lever_training_IDs[-1]

# non digits \\D cases
Lever_training_IDs[grep(pattern = "\\D$", Lever_training_IDs)]

# extract just animal ID (can't use bc it excludes cases)
# string2 <- Delayed_Punishment_IDs[grepl(pattern = "\\d$", Delayed_Punishment_IDs)]

# split a string using a pattern (used for animal_ID column)
animal_ID <- strsplit(Lever_training_IDs, "\\D$") %>% unlist()

# extract just the condition
animal_ID_cond <- stringr::str_match(Lever_training_IDs, "\\D$") %>%
  as.vector()

# extract date of experiment 
# (uncomment when dropbox syncs) dp_files<-list.files(path="/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U0_Dropbox_copy1/U01 folder/Delayed punishment",recursive = T,pattern=".txt",full.names = T)
lt_files<-list.files(path="/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01/Lever training",recursive = T,pattern=".txt",full.names = T)
year_monthday_char <- stringr::str_extract_all(lt_files, "[[:digit:]]{4}-[[:digit:]]{4}") %>% unlist()
# year_monthday_char_test <- gsub("(-[[:digit:]]{2})", "\\1-", year_monthday_char) #not needed for ymd function
## use lubridate to change to POSITX
ymd_dateformat <- ymd(year_monthday_char)

#extract time of experiment
hoursecond_char <- stringr::str_extract_all(lt_files, "[[:digit:]]{4}_") %>% unlist()
hoursecond_char_test <- gsub("_", "", hoursecond_char)
hoursecond_char_test <- sub("([[:digit:]]{2})", "\\1:", hoursecond_char_test) #sub is just first match
## use lubridate to change to time format 
hoursecond_timeformat <- hm(hoursecond_char_test)

################################
####### Locomotor ##############
################################
Locomotor_IDs <- list.dirs(path="Locomotor",full.names=F)[-1] #remove "" from recursive

# non digits \\D cases
Locomotor_IDs[grep(pattern = "\\D$", Locomotor_IDs)]

# split a string using a pattern (used for animal_ID column)
animal_ID <- strsplit(Locomotor_IDs, "\\D$") %>% unlist()

# extract just the condition
animal_ID_cond <- stringr::str_match(Locomotor_IDs, "\\D$") %>%
  as.vector()

# extract date of experiment 
loc_files<-list.files(path="/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01/Locomotor",recursive = T,pattern=".txt",full.names = T)
#extract time of experiment

################################
##### Progressive Punishment ###
################################
ProgP_IDs <- list.dirs(path="Progressive punishment",full.names=F)[-1] #remove "" from recursive 

# non digits \\D cases
ProgP_IDs[grep(pattern = "\\D$", ProgP_IDs)]

# split a string using a pattern (used for animal_ID column)
animal_ID <- strsplit(ProgP_IDs, "\\D$") %>% unlist()

# extract just the condition
animal_ID_cond <- stringr::str_match(ProgP_IDs, "\\D$") %>%
  as.vector()

# extract date of experiment 
# XX NO TXT FILES? 
progpun_files<-list.files(path="/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01/Progressive punishment",recursive = T,pattern=".txt",full.names = T)
#extract time of experiment
# CAN'T DO WITHOUT FILES

################################
#### Progressive Ratio #########
################################

ProgR_IDs <- list.dirs(path="Progressive ratio",full.names=F)[-1] #remove "" from recursive 

# non digits \\D cases
ProgR_IDs[grep(pattern = "\\D$", ProgR_IDs)]

# split a string using a pattern (used for animal_ID column)
animal_ID <- strsplit(ProgR_IDs, "\\D$") %>% unlist()

# extract just the condition
animal_ID_cond <- stringr::str_match(ProgR_IDs, "\\D$") %>%
  as.vector()

# extract date of experiment 
# XX NO TXT FILES? 
prograt_files<-list.files(path="/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01/Progressive ratio",recursive = T,pattern=".txt",full.names = T)
#extract time of experiment
# CAN'T DO WITHOUT FILES

################################
########### Runway #############
################################
# Full filename along with directory
Runway_IDs <- list.files(path="Runway", recursive = F, full.names=T) # T full (Runway/Cohort 1), F full (Cohort 1) 
# Basename 
basename(Runway_IDs) 
# Animal ID 
Runway_animal_ID <- list.dirs(path="Runway", recursive = T)
Runway_animal_ID <- stringr::str_extract_all(Runway_animal_ID, "U[[:digit:]]+") %>% unlist()

# check for all runway/cohort
CohortVSU <- list.files(path="Runway", recursive = T, pattern = ".txt")
CohortVSU_df <- data.frame("U ID" = stringr::str_extract_all(CohortVSU, "U[[:digit:]]+") %>% unlist(),
                           "File ID" = stringr::str_extract_all(CohortVSU, "_[[:digit:]]+") %>% unlist())
# cases in which they don't match 
CohortVSU_df <- CohortVSU_df %>% 
  mutate(File.ID= paste0("U", gsub("_", "", File.ID))) %>% 
  filter(U.ID != File.ID) 

# Trailing 
Runway_animal_ID[grep(pattern = "\\D$", Runway_animal_ID)] # no trailing
Runway_animal_ID_cond <- stringr::str_match(Runway_animal_ID, "\\D$") %>%
  as.vector()

# Month
Runway_files <- list.files(path="Runway",full.names=T, recursive = T, pattern = ".txt")
Runway_year_monthday_char <- stringr::str_extract_all(Runway_files, "[[:digit:]]{4}-[[:digit:]]{4}") %>% unlist()
Runway_ymd_dateformat <- ymd(year_monthday_char) ## XX What format does SQL/Rat Genome take in? 

# Time
Runway_files <- list.files(path="Runway",full.names=T, recursive = T, pattern = ".txt")
Runway_hoursecond_char <- stringr::str_extract_all(lt_files, "[[:digit:]]{4}_") %>% unlist()
Runway_hoursecond_char_test <- gsub("_", "", Runway_hoursecond_char)
Runway_hoursecond_char_test <- sub("([[:digit:]]{2})", "\\1:", Runway_hoursecond_char_test) #sub is just first match
## use lubridate to change to time format 
Runway_hoursecond_timeformat <- hm(Runway_hoursecond_char_test)

# Cohort
Runway_Cohort <- list.files(path="Runway",full.names=F) # 29 cohorts XX Should cohorts all be whole numbers? 


# Write a function to include all directories into one dataframe 

trailing_ids <- function(path){
  IDs <- list.dirs(path = path, full.names = F)
  return(IDs[grep(pattern = "\\D$", IDs)])
}

trailing_ids("Progressive punishment")

Tom_Jhou_U01_experiments_all <- data.frame("dir_filename" = files,
                                           "basename" = ,
                                           "animal_ID" = ,
                                           "animal_ID_condition" = ,
                                           "date_experiment" = ,
                                           "time_experiment" = , 
                                           "cohort" = )

Tom_Jhou_U01$dir_filename 

#column1: full filename along with directory
#column2: basename()
#column3 extract animal ID
#column4: e or i
#column5: y/M/d
#column6: time
#column7: cohort
#count number of unique files for each animals

##########################
#### FROM RAW FILES ######
##########################

# self defined functions 

# extract info from filename
extractfromfilename <- function(df){
  if(df == "rawfiles_prepcalc"){
    df$cohort <- stringr::str_extract(df$filename, "Cohort [[:digit:]]")
  }
  df$labanimalid <- stringr::str_extract(df$filename, "U[[:digit:]]+[[:alpha:]]*")
  df$date <- gsub("[-]([[:digit:]]{2})([[:digit:]]{2})", "-\\1-\\2", stringr::str_extract(df$filename, "[[:digit:]]{4}[-][[:digit:]]{4}"))
  df$date <- as.POSIXct(df$date, tz = "UTC")
  df$time <- stringr::str_extract(df$filename, "[[:digit:]]{4}(?=_)")
  df$time <- gsub('([[:digit:]]{2})([[:digit:]]{2})', '\\1:\\2', df$time)
  df <- df[order(df$filename), ]
  return(df)
} 

# extracted the function from u01_qc file
uniform.var.names.testingu01_df <- function(df) {
  if(grepl("Parent", names(df)) %>% any()){
    names(df)[1:2] <- df[1,][1:2] %>% as.character()
    df <- df[-1, ]
  }
  names(df) <- mgsub::mgsub(names(df),
                            c(" |\\.", "[(|)]", "#", "Transponder |16 digit", "Date of Wean|Wean Date","Animal"),
                            c("", "_", "Number", "RF", "DOW","LabAnimal"))
  names(df) <- mgsub::mgsub(names(df),
                            c("DateofShipment", "LabAnimalID"), 
                            c("ShipmentDate", "LabAnimalNumber"))
  names(df) <- tolower(names(df))
  df
}


# using Dropbox copy setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy")

# process the master excel file (mainly for comments and resolutions columns)
setwd("~/Dropbox (Palmer Lab)/U01 folder")
summaryall <- readxl::read_excel("U01 Master sheet_readonly.xlsx")
names(summaryall) <- summaryall[1, ] %>% as.character()
summaryall <- summaryall[-1, ]
summaryall <- uniform.var.names.testingu01_df(summaryall)

# rfidandid <- dplyr::select(summaryall, jhoulabid, sex, rfid, shipmentcohort) # NOT SURE WHY DPLYR DOESN'T WORK
rfidandid <- subset(summaryall, select = c("jhoulabid", "shipmentcohort", "wakeforestid", "sex", "rfid", "dob", "notesforhumans:", "resolution:")) # BASE SUBSET WORKS
rfidandid <- rfidandid %>%
  mutate(shipmentcohort = shipmentcohort %>% as.numeric() %>% round(digits = 3),
         dob = as.POSIXct(as.numeric(dob) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d"),
         wakeforestid = gsub(".*-->", "", rfidandid$wakeforestid)) %>%
  rename(labanimalid = jhoulabid,
         comment = `notesforhumans:`,
         resolution =`resolution:`) %>% 
  dplyr::filter(!is.na(wakeforestid))



# using original copy latest updates 

# trying to get the color formats 
setwd("~/Dropbox (Palmer Lab)/U01 folder")
Jhou_Master <- tidyxl::xlsx_cells("U01 Master sheet_readonly.xlsx", sheets = NA) # read all sheets, sheets = NA
Jhou_Master_formats<-tidyxl::xlsx_formats("U01 Master sheet_readonly.xlsx")
# Jhou_Master_formats$local$font$color$rgb %>% unique shades of pink, orange, and red
red_index<-which(Jhou_Master_formats$local$font$color$rgb=="FF000000")
# Jhou_Master_formats$local$font$color$rgb %>% as.factor() %>% summary() used the most frequent to query
redJhou_Master <- Jhou_Master[ Jhou_Master$local_format_id %in% red_index,  ]
redJhou_Progpun <- redJhou_Master %>% 
  filter(sheet == "Progressive Punishment")
# redJhou_Progpun$data_type %>% as.factor() %>% summary()


# use the red value from excel sheet to find patterns and omit values from the raw files

# read in all sheets 
master_sheetnames <- excel_sheets("U01 Master sheet_readonly.xlsx")
Jhou_master_filelist <- lapply(master_sheetnames, read_excel, path = "U01 Master sheet_readonly.xlsx")
names(Jhou_master_filelist) <- master_sheetnames
redJhou_Master_df <- Jhou_master_filelist$`Progressive Punishment`[redJhou_Progpun$row, redJhou_Progpun$col] # don't run, may not work





## Text file preparation

################################
### RAW TEXT Runway ############
################################

### EXP 1: runway 
setwd("~/Dropbox (Palmer Lab)/U01 folder/Runway")

# reach time 
readrunway <- function(x){
  runway <- fread(paste0("awk '/REACHED/{print $1}' ", "'", x, "'"))
  runway$filename <- x
  return(runway)
}

runwayfiles_clean <- list.files(path=".", pattern=".*RUNWAY.*.txt", full.names=TRUE, recursive=TRUE) # note the 4221 id in one file, but seems to be no error files so below code is unneeded
# files_clean <-  files[ ! grepl("error", files, ignore.case = TRUE) ] 
# runwayfiles_clean <- gsub(" ", "\\\\ ", runwayfiles_clean) # not the issue for not being able to access the files

runway_reach <- lapply(runwayfiles_clean, readrunway)
  # data$filename <- sub(" ", "", as.character(data$filename)) # get rid of all spaces in filenames
 #  return(data)
# }) # cannot assign the colnames in fxn bc of one vs two column setup 

runway_reach_df <- rbindlist(runway_reach, fill = T) 
runway_reach_df <- runway_reach_df %>% 
 #  select(-RUNWAY) %>% 
  rename("reachtime" = "V1") # remove RUNWAY bc df has 3187 rows and RUNWAY holds 3187 NA's # this iteration didn't have RUNWAY variable 

# evaluate NA reachtime cases
runway_reachtimeNA <- subset(runway_reach_df, is.na(reachtime)==T) ## XX Made note to Maya already

# location2 time 
readrunwayloc2 <- function(x){
  runwayloc2 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t2\" ", "'", x, "'"))
  runwayloc2$filename <- x
  return(runwayloc2)
}

runway_loc2 <- lapply(runwayfiles_clean, readrunwayloc2)
  # data$filename <- sub(" ", "", as.character(data$filename)) # get rid of all spaces in filenames
#   return(data)
# }) # cannot assign the colnames in fxn bc of one vs two column setup 

runway_loc2_df <- rbindlist(runway_loc2, fill = T) 
runway_loc2_df <- runway_loc2_df %>% 
  rename("loc2time" = "V1",
         "location" = "V2",
         "locationnum" = "V3") %>%
  select(loc2time, location, locationnum, filename) %>% 
  mutate(loc2time = as.numeric(as.character(loc2time)))

# evaluate non location 2 cases 

loc2non2 <- subset(runway_loc2_df, is.na(locationnum)) # Noted to Maya and Alen # originally != 2 but changed to is.na because of summary(runway_loc2_df$locationnum) 

# location2 <-"find -type f -iname \"*RUNWAY*.txt\" -print0 | xargs -0 grep -P -m 1 \"LOCATION\\s\\t2\" > location2times.txt"
# system(location2)
# rawfiles_location2 <- read.csv("location2times.txt", head = F)
# rawfiles_location2_clean <- separate(rawfiles_location2, V1, into = c("filename", "loc2time"), sep = "[:]")
# rawfiles_location2_clean$loc2time <- gsub("\\t.+", "", rawfiles_location2_clean$loc2time, perl = T) %>% 
#   as.numeric()

# join together loc2 and reach, calculate elapsed time, and join to shipping data

# rawfiles_prepcalc <- left_join(rawfiles_reach, rawfiles_location2_clean, by = "filename")
rawfiles_calc <- left_join(runway_reach_df, runway_loc2_df, by = "filename") %>% 
  #filter(locationnum == 2) %>% 
  mutate(elapsedtime = trunc(reachtime) - trunc(loc2time)) %>%  # turn to transmute once all edges are smooth 
  arrange(filename) %>% 
  extractfromfilename %>%  # sort by ascending filename and get animal id and exp date/time 
  left_join(., rfidandid, by = "labanimalid") # add rfid column # created rfidandid from the master shipment file, the first page -- summaryall

# rfid and lab animal id ## NO LONGER USING BC IT IS NOT A DIRECT TRANSLATION
# rfidandid <- WFU_Jhou_test_df %>% 
#   select(labanimalnumber, rfid) %>% 
#   transmute(labanimalid = paste0("U", stringr::str_extract(labanimalnumber, "[1-9]+[0-9]*")),
#             rfid = rfid) # labanimalid vs labanimalnumber

# graphics for email: subset(rawfiles_prepcalc, is.na(rfid)) %>% select(labanimalid) %>% unique AND subset(rawfiles_prepcalc, is.na(loc2time))$filename

## RUNWAY REVERSAL
# redo reversals since it is only the number of lines of reversals between start and reaching goalbox 

read_runwayrevs <- function(x){
  reversals <- fread(paste0("sed -n '/OPENING/,/REACHED/p' ", "'", x, "'", " | grep -c \"REVERSAL\""))
  reversals$filename <- x 
  return(reversals)
  }

runway_reversals <- lapply(runwayfiles_clean, read_runwayrevs) %>% rbindlist(fill = T)
names(runway_reversals) <- c("reversals", "filename")
  
# reversals <- "find -type f -iname \"*.txt\" -print0 | xargs -0 grep -c \"REVERSAL\" > reversals.txt"
# system(reversals)
# reversals <- read.csv("reversals.txt", head = F)
# reversals <- separate(reversals, V1, into = c("filename", "reversals"), sep = "[:]")

# bind reversals with runway data 
runway <- left_join(rawfiles_calc, runway_reversals, by = "filename") %>% # clean out upstream find -name regular expression to exclude files that don't contain the exp name; see subset(., is.na(rfid))
  mutate(experimentage = as.numeric(date - dob)) %>%
  select(-c(date, dob)) # calculate age and remove date 


# *optional* add session info
# # make session variable to make into long data 
# i <- 1
# j <- 1
# repeat {
#   rawfiles_prepcalc$session[i] <- paste("session", j)
#   i = i + 1
#   j = j + 1
#   if (rawfiles_prepcalc$labanimalid[i] != rawfiles_prepcalc$labanimalid[i-1]){
#     j = 1
#   }
# } #add session information
# 
# rawfiles_prepcalc_wide <- spread(rawfiles_prepcalc, session, diff)

################################
### RAW TEXT  Locomotor ########
################################
### EXP 2: locomotor 

setwd("~/Dropbox (Palmer Lab)/U01 folder/Locomotor") 
# extract bincounts from second column with data
read_locomotor <- function(x){
  # locomotor <- fread(paste0("awk '/^[1-9][0-9]*/{print $2}' ", "'", x, "'"))
  locomotor <- fread(paste0("sed -n /MINUTE/,/END/p ", "'", x, "'", " | awk '/^[0-9]/{print $2}'"))
  locomotor$filename <- x
  return(locomotor)
}


# sed -n /MINUTE/,/END/p 2019-1016-1111_456_LOCOMOTOR_BASIC.txt | awk '/^[0-9]/{print $2}'

locomotorfiles <- list.files(path=".", pattern=".*LOCOMOTOR.*.txt", full.names=TRUE, recursive=TRUE) # note the 4221 id in one file, but seems to be no error files so below code is unneeded
locomotorfiles_clean <-  locomotorfiles[ ! grepl("error", locomotorfiles, ignore.case = TRUE) ]
locomotorfiles_clean[grepl("[(]", locomotorfiles_clean)] # there is one duplicate file


duplicatedfiles <- grep("[(]\\d[)]", locomotorfiles_clean, value = T) %>% 
  gsub(" [(]\\d[)]", "", .) # go through 764 all files; create 1 files that have duplicates
removeduplicatefiles <- subset(locomotorfiles_clean, !(locomotorfiles_clean %in% duplicatedfiles)) #763, found 1 to remove

rawfiles_locomotor <- lapply(locomotorfiles_clean, read_locomotor) %>% 
  rbindlist(fill = T) %>%
  select(V1, filename) %>%
  rename("bincounts" = "V1") %>%
  mutate(bincounts = as.numeric(bincounts)) # using sed rather than awk results in less NA (only 26 here); XX 10/29 ADDED TO DOCUMENT FILE NA IS COMING FROM FILES NOT HAVING END SESSION

# rawfiles_locomotor %>% filter(is.na(bincounts)) %>% count(filename) %>% summary(n) some are missing one and other files are missing all data

# rawfiles_locomotor_long <- rawfiles_locomotor[!grepl("[[:punct:]]", as.character(rawfiles_locomotor$bincounts)), ] # clean out invalid observations (timestamps) 
# rawfiles_locomotor_long <- droplevels(rawfiles_locomotor_long) #(from 243 levels to 117)
i <- 1
j <- 1
repeat {
  rawfiles_locomotor$minute[i] <- paste0("minute", j)
  i = i + 1
  j = j + 1
  if (rawfiles_locomotor$filename[i] != rawfiles_locomotor$filename[i-1]){
    j = 1
  }
} #add session information

# library(tidyverse)
rawfiles_locomotor_wide <- spread(rawfiles_locomotor, minute, bincounts) %>% # spread from long to wide
  as.data.table()
# get code from https://stackoverflow.com/questions/57037860/how-to-reorder-column-names-in-r-numerically
# reorder the columns
setcolorder(rawfiles_locomotor_wide, c(1, order(as.numeric(gsub("minute", "", names(rawfiles_locomotor_wide)[-1]))) + 1))

# deal with na values first before calculating sums and means 


## to do: 
# starting U294, add the first 15 minutes average and last 15 minutes average
# for U311, U312 and starting U315 to U328, take the same two averages for the second session


# investigate the number of na's and compare that to how they are coding na's
rawfiles_locomotor_wide[-1] <- as.numeric(is.na(rawfiles_locomotor_wide[!complete.cases(rawfiles_locomotor_wide[,-32])]))
as.numeric(is.na(res[-1]))

data.frame(ID = rawfiles_locomotor_wide$labanimalid, ques = apply(rawfiles_locomotor_wide, 1, function(x) 
  paste0(names(which(is.na(x))), collapse = ",")))

#################### might delete later 11/1 waiting to hear back from jhou's team; excel sheet seems to have just moved it #######################################
rawfiles_locomotor_wide[13,]$minute30 <- rawfiles_locomotor_wide[13,]$minute31
rawfiles_locomotor_wide <- select(rawfiles_locomotor_wide, -minute31)

res <- as.data.frame(rawfiles_locomotor_wide)[!complete.cases(as.data.frame(rawfiles_locomotor_wide)),]
res[-1] <- as.numeric(is.na(res[-1]))
res
################################

# add means and sums as jhou's lab does
rawfiles_locomotor_wide[, `:=`(bintotal = rowSums(.SD, na.rm=T),
                               binmeans = rowMeans(.SD, na.rm=T)), .SDcols=names(rawfiles_locomotor_wide)[-1]]

# remove the rows with almost all na's # two files ./U175/2019-0209-1959_175_LOCOMOTOR_BASIC.txt(all); ./U412/2019-0918-1102_412_LOCOMOTOR_BASIC.txt (almost all)
# remove the rows with all 0's
# remove the rows that they have noted to remove EXCLUDE_LOCOMOTOR
# remove duplicated files
rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>% 
  dplyr::filter(!(is.na(minute1) | is.na(minute2) | is.na(minute3))) %>% 
  dplyr::filter(bintotal != 0) %>% 
  dplyr::filter(!grepl("EXCLUDE_LOCOMOTOR", resolution)) 
rawfiles_locomotor_wide <- rawfiles_locomotor_wide[!duplicated(rawfiles_locomotor_wide[-1]),]

# only one case for which the minute 31 appears, ./U112/2019-0121-0939_112_LOCOMOTOR_BASIC.txt
# rawfiles_locomotor_wide <- extractfromfilename(rawfiles_locomotor_wide) %>%
#   mutate(labanimalid = gsub('/u', '/U', filename),
#          labanimalid = str_extract(labanimalid, '(U[[:digit:]]+)')) %>%
#   left_join(., rfidandid, by = "labanimalid") %>%  # extract file information for preparation for appending to rfid
#   mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)) # add rfid colum # add cohort column (XX WERE THESE DIVIDED INTO COHORTS) # this code changes it back to dataframe

# use to merge but extract labanimalid NOT FROM U
rawfiles_locomotor_wide <- extractfromfilename(rawfiles_locomotor_wide) %>%
  mutate(labanimalid = str_extract(filename, '(_[[:digit:]]+)'),
         labanimalid = gsub('_', 'U', labanimalid)) %>% 
  left_join(., rfidandid, by = "labanimalid") %>%  # extract file information for preparation for appending to rfid  # add rfid colum # add cohort column (XX WERE THESE DIVIDED INTO COHORTS) # this code changes it back to dataframe
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid))


# check the number of files is even before adding session info 
rawfiles_locomotor_wide %>% add_count(labanimalid) %>% dplyr::filter(n != 1, n!=2, n!=4) %>% View()

# remove the first file for the 5 file cases (11/1 remove two files)
rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>% 
  group_by(labanimalid) %>% 
  do(tail(., 4)) 

######################################################### WAIT UNTIL RESPONSE
# TEMPORARILY REMOVE THE TWO CASES: 371 AND 271
rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>% 
  dplyr::filter(!labanimalid %in% c("U371", "U271"))
##########################################################

# with no more 3 or 5 file cases...
# add count and assigning the session based on the number of counts 
rawfiles_locomotor_wide$session <- NA 
bincounts <- c("Binned Counts","Binned Counts1",  "Binned Counts2","Binned Counts1a","Binned Counts1b","Binned Counts2a","Binned Counts2b") %>% 
  data.frame() %>% 
  rename("session" = ".") %>%
  mutate(session = as.character(session))

# add count and assigning the session based on the number of counts
# extract from between _id_ rather than from Uid # exclude those that are resolved to be exclude
rawfiles_locomotor_wide_split <- split(rawfiles_locomotor_wide, rawfiles_locomotor_wide$labanimalid)
rawfiles_locomotor_wide_wsession <- lapply(rawfiles_locomotor_wide_split, function(x){
  x <- x %>% 
    arrange(date, time)
  if(nrow(x) == 2){
      cbind(x, tail(head(bincounts,3),2))
    }
  else if(nrow(x) == 4){
    cbind(x, tail(bincounts,4))
  }
  else{
    cbind(x,head(bincounts,1))
  }
}) %>% 
  rbindlist() 


# to do: append bodyweightperc

################################
### RAW TEXT  Prog punish ######
################################
### EXP 3: progressive punishment 

# shocks (extract last and second to last for each session)
# get row numbers from awk nr (create_progpuntable function)
# use in sed to find the number of LEFTPRESSES
# assign yes or no to complete or attempt columns 
# group by and tail
setwd("~/Dropbox (Palmer Lab)/U01 folder/Progressive punishment") # using their copy to prevent any copy issues
progpunfiles <- list.files(path=".", pattern=".*CONFLICT.*.txt", full.names=TRUE, recursive=TRUE) # some don't have the U designation bc they are not placed into the folders yet
progpunfiles_clean <-  files[ ! grepl("error", files, ignore.case = TRUE) ] # clean out errors # XX DO LATER, CREATE SUBSET OF ONES THAT DON'T FOLLOW FORMAT
# no results for grepl("[(]) so no duplicate files? 
create_progpuntable <- function(x){
  thistrialrownumandshock = fread(paste0("awk '/THIS TRIAL/{print $1 \" \" $2 \",\" $13 \",\" NR}' ","'",x,"'"), header=F, fill=T, showProgress = F, verbose = F)  
  thistrialrownumandshock$filename<-x
  return(thistrialrownumandshock)
}

progpunishment_df = lapply(files_clean, create_progpuntable) %>%
  rbindlist(fill = T)
colnames(progpunishment_df) = c("trialnum", "shockma", "rownum", "filename") # this line isn't working for some files for which the values cannot be found 10/28 WORKING 
# summary(progpunishment_df) # 60 NA's
# progpunishment_df

progpunishment_df_complete <- progpunishment_df %>% 
  dplyr::filter(complete.cases(.)) %>%  
  group_by(filename) %>% 
  do(tail(., 2)) #limit the calculations of the number of LEFTPRESSES to two per filename # XX SCREENSHOT ADDED TO DOCUMENT ONLY ON CASES THAT HAVE THE VALUES WE NEED; 6193 cases

progpunishment_df %>% count(filename) %>% subset(n!=2) # XX made note to Alen 10/18, follow up again 
oneobservation_details <- progpunishment_df %>% 
  group_by(filename) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count != 2); head(oneobservation_details) # 96 observations

data_df_valid <- progpunishment_df_complete %>% 
  group_by(filename) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  dplyr::filter(count == 2) # use the valid files before they respond about the other ones 6170

# create categorization table 
create_progpuntable_tocategorize <- function(x){
  numofsessions <- list()
  for(i in seq(1,nrow(x),2)){
    
    numleftpressesbwlasttwo = fread(paste0("sed -n ", x$rownum[i], ",", x$rownum[i+1], "p ", "'", x$filename[i], "'", " | grep -c \"LEFTPRESSES\""), header=F, fill=T, showProgress = F, verbose = F) %>% data.frame()
    numleftpressesbwlasttwo$filename <-x$filename[i]   
    
    numleftpresseslast = fread(paste0("sed -n '", x$rownum[i + 1] , ",/ENDING/p' ", "'", x$filename[i], "'", " | grep -c \"LEFTPRESSES\""), header=F, fill=T, showProgress = F, verbose = F) %>% data.frame()
    numleftpresseslast$filename <-x$filename[i] 
    
    secondtolastshock = fread(paste0("awk 'NR == " , x$rownum[i]," {print $13}' ",  "'", x$filename[i], "'"), header=F, fill=T, showProgress = F, verbose = F) %>% data.frame()
    # might use reg exp (?:([1-9]?[0-9])[a-zA-Z ]{0,20}(?:arrests|arrested))
    secondtolastshock$filename <-x$filename[i] 
    
    lastshock = fread(paste0("awk 'NR == " , x$rownum[i+1]," {print $13}' ",  "'", x$filename[i], "'"), header=F, fill=T, showProgress = F, verbose = F) %>% data.frame()
    # might use reg exp (?:([1-9]?[0-9])[a-zA-Z ]{0,20}(?:arrests|arrested))
    lastshock$filename <-x$filename[i]

    filelasttwoandlast <- merge(numleftpressesbwlasttwo,numleftpresseslast, by = "filename") %>% 
      rename("numleftpressesbwlasttwo" = "V1.x", 
             "numleftpresseslast" = "V1.y") %>% # for each file, merge the count of LEFTPRESSES occurences
      merge(., secondtolastshock, by = "filename") %>% # %>% # for each file, merge the count of LEFTPRESSES occurences
      merge(., lastshock, by = "filename") %>% 
      rename("secondtolastshock" = "V1.x", 
             "lastshock" = "V1.y") # for each file, merge the MA value (last two)

    # names(filelasttwoandlast) = c("filename", "numleftpressesbwlasttwo","numleftpresseslast", "secondtolastshock", "lastshock")
    # numofsessions[[i]] <- filelasttwoandlast # add to list 
    
    # turning off this part of function because functions with blank spaces there are messing up the function
    # if(grepl("delayed", filelasttwoandlast$filename, ignore.case = T)){
    #   delay = fread(paste0("awk 'NR == " , x$rownum[i]," {print $18}' ",  "'", x$filename[i], "'"), header=F, fill=T, showProgress = F, verbose = F,blank.lines.skip=TRUE)
    #   delay$filename <-x$filename[i]
    #   filelasttwoandlast <- merge(filelasttwoandlast, delay, by = "filename") %>%
    #     rename("delay" = "V1")
    # }

    numofsessions[[i]] <- filelasttwoandlast
  }
  numofsessions_df = do.call(rbind, numofsessions)
  # numofsessions <- as.data.frame(numofsessions)
  return(numofsessions_df)
}

# data2_valid_subset <- data2_valid[1:100,]
# data_categories_test = create_progpuntable_tocategorize(data2_valid_subset) # test subset 

progpundata_categories = create_progpuntable_tocategorize(data_df_valid) # test on valid datapoints until Jhou team returns comment 

progpundata_categories_wcat <- progpundata_categories %>% 
  mutate(secondtolastshock_cat = ifelse(numleftpressesbwlasttwo > 3, "Complete", "Attempt"),
         lastshock_cat = ifelse(numleftpresseslast >= 3, "Complete", "Attempt"))

progpun_presses <- function(x){
  presses <- fread(paste0("tac ", "'", x, "'", " | awk '/LEFTPRESSES/ {print $4 \",\" $6; exit}'"), header=F, fill=T, showProgress = F, verbose = F)
  presses$V1[nrow(presses) == 0] <- NA
  presses$V2[nrow(presses) == 0] <- NA
  presses$filename <- x
  return(presses)
}
progpun_presses_df = lapply(files_clean, progpun_presses) %>% 
  rbindlist(fill = T) # 3168 cases, 47 na and left presses min 55 max 434
colnames(progpun_presses_df) = c("activepresses", "inactivepresses", "filename")


progpun_boxes <- function(x){
  boxandstations <- fread(paste0("awk '/Started script/{print $(NF-1) \" \" $NF}' ", "'", x, "'"), header=F, fill=T, showProgress = F, verbose = F) %>% as.data.frame()
  boxandstations$filename <- x
  return(boxandstations)
}

progpun_boxesandstations_df = lapply(files_clean, progpun_boxes) %>% 
  rbindlist(fill = T) # 3168 cases, no na, and left presses min 1 max 8
colnames(progpun_boxesandstations_df) = c("boxorstation", "boxorstationumber", "filename")

# XXX concern (some animals have more than one box designation): 
# boxesandstations_df$labanimalid <- stringr::str_extract(boxesandstations_df$filename, "U[[:digit:]]+[[:alpha:]]*")
# morethanone <- boxesandstations_df %>%
#   select(labanimalid, boxorstationumber) %>% 
#   unique() 
# morethanone2 <- morethanone %>% count(labanimalid) %>% filter(n != 1)
# cases <- subset(rawfiles_box, rawfiles_box$labanimalid %in% morethanone2$labanimalid)
# cases # just 6 and 8 now XX already made note to Tom Jhou's team

progressivepunishment <- left_join(x = progpundata_categories_wcat, y = progpun_presses_df, by = "filename") %>% # XX TOOK SCREENSHOT OF NA DELAY'S AND SENT THEM TO TOM'S TEAM
  left_join(., progpun_boxesandstations_df, by = "filename") %>% # dim is all over the place (using the most limiting 3085, resulting has no na)
  extractfromfilename() %>% # extract file information for preparation for appending to rfid
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid) ) %>% 
  mutate(shockoflastcompletedblock = ifelse(lastshock_cat == "Complete", lastshock, secondtolastshock),
         shockoflastattemptedblock = lastshock) %>%
  select(-c(numleftpressesbwlasttwo, lastshock_cat, secondtolastshock_cat, secondtolastshock, lastshock))


################################
### RAW TEXT  Prog ratio #######
################################

### EXP 4: Progressive ratio
# max ratio
setwd("~/Dropbox (Palmer Lab)/U01 folder/Progressive ratio")
progratiofiles_clean <- list.files(path=".", pattern=".*RATIO.*.txt", full.names=TRUE, recursive=TRUE) 
progratiofiles_clean <- progratiofiles_clean[ ! grepl("error", progratiofiles_clean, ignore.case = TRUE) ] 
readmaxratio <- function(x){
  maxratio <- fread(paste0("grep -B1 \"TIMEOUT\\\\s\\\\s\" " , "'", x, "'", " | awk '{print $4; exit}'"))
  maxratio$filename <- x
  return(maxratio)
}
# get the max ratio from the line before TIMEOUT
progratio_maxratio <- lapply(progratiofiles_clean, readmaxratio) 
progratio_maxratio_df <- rbindlist(progratio_maxratio, fill = T) %>%
  rename("maxratio" = "V1") %>% 
  select(-NUMBER) %>%
  mutate(maxratio = as.numeric(maxratio)) # XX why are there 30 NA's

# rawfiles_maxratio <- extractfromfilename(rawfiles_maxratio)
# rawfiles_maxratio %>% summary() seems like the machine generated two trials of data, so we will remove the initial one with the next line
# rawfiles_maxratio <- rawfiles_maxratio[!(rawfiles_maxratio$labanimalid=="U187" & rawfiles_maxratio$maxratio==2),]

# presses get the left and right values at the last trial
readpresses <- function(x){
  presses <- fread(paste0("grep \"TIMEOUT\\\\s\\\\s\" " , "'", x, "'", " | awk '{print $7 \",\" $9; exit}'"))
  presses$filename <- x
  return(presses)
}
# get the max ratio from the line before TIMEOUT
progratio_presses <- lapply(progratiofiles_clean, readpresses) 
progratio_presses_df <- rbindlist(progratio_presses, fill = T) %>%
  rename("activepresses" = "V1",
         "inactivepresses" = "V2") # again there are 30 NA's  

# join to create final raw df
progratio <- left_join(progratio_maxratio_df, progratio_presses_df, by = "filename") # 10/28 bring to Alen's attention -- these cases for which there are only timeout lines and no pre-timeout value so no maxratio value 
progratio <- extractfromfilename(progratio)
progratio$labanimalid <- gsub('(U)([[:digit:]]{1})$', '\\10\\2', progratio$labanimalid) # better organizational for ordering and consider this for other files; ADD BEGINNING ZEROES TO COHORT/ID'S
progratio <- progratio[order(progratio$labanimalid), ] # reorder based on labanimalid  

################################
### RAW TEXT  Delayed pun ######
################################

# EXP 5: Delayed punishment
# setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/Delayed punishment/")
# find -type f -iname "*PUNISHMENT*.txt" ! -path "*error*" -exec awk '/ENDING/{print FILENAME}' {} \; 
setwd("~/Dropbox (Palmer Lab)/U01 folder/Delayed punishment") # while dropbox is updating the clone

delayed_punishmentfiles <- list.files(path=".", pattern=".*DELAYED.*.txt", full.names=TRUE, recursive=TRUE) # exclude existing txt files and include any corrective "qualifiers" # 5670 counts
delayed_punishmentfiles_clean <-  delayed_punishmentfiles[ ! grepl("error", delayed_punishmentfiles, ignore.case = TRUE) ]  # exclude files that have errors (labelled by Jhou's team) # 3168 counts
# read_delayedpresses<-function(x){
#   data = fread(paste0("tac ","'",x,"'", "| awk '/LEFTPRESSES/{print $4 \",\" $6; exit}'"), header=F, fill=T, showProgress = F)  
#   data$id<-x
#   return(data)
# } # trying prog pun function; doesn't work because we need to add a function to extract the time delay


create_delayedpuntable <- function(x){
  thistrialrownumandshock = fread(paste0("awk '/THIS TRIAL/{print $1 \" \" $2 \",\" $13 \",\" $18 \",\" NR}' ","'",x,"'"), header=F, fill=T, showProgress = F, verbose = F)  
  thistrialrownumandshock$filename<-x
  return(thistrialrownumandshock)
}


delayedpunishment_df <- lapply(delayed_punishmentfiles_clean, create_delayedpuntable) %>% 
  rbindlist(fill = T) %>% 
  mutate(delay = str_extract(delayed_data_df_valid$delay, "[0-9]{1,2}") %>% as.numeric)
# delayedpunishment_df <- rbindlist(delayedpunishment, fill = T)
colnames(delayedpunishment_df) = c("trialnum", "shockma", "delay", "rownum", "filename") # this line isn't working for some files for which the values cannot be found 10/28 WORKING 
delayedpunishment_df <- delayedpunishment_df %>% 
  group_by(filename) %>% 
  do(tail(., 2)) #limit the calculations of the number of LEFTPRESSES/SESSIONS to two per filename 


delayed_data_df_valid <- delayedpunishment_df %>% 
  group_by(filename) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  dplyr::filter(count == 2)

# 10/29 SAVE SCREENSHOT IN DOCUMENT FOR TOM delayed_data_df_valid %>% dplyr::filter(is.na(delay)) %>% group_by(filename) %>% mutate(count = n()) %>% as.data.frame() 

# prog pun function seems to work for delayed prog categories, presses, and box
delayed_data_categories = create_progpuntable_tocategorize(delayed_data_df_valid) # test on valid datapoints until Jhou team returns comment (cannot use odd numbers as subset!! )
# summary looks ok; 5478 files

delayed_data_categories_wcats <- delayed_data_categories %>% 
  mutate(secondtolastshock_cat = ifelse(numleftpressesbwlasttwo > 3, "Complete", "Attempt"),
         lastshock_cat = ifelse(numleftpresseslast >= 3, "Complete", "Attempt"))

# there are files within the clean file names that are EMPTY
duplicatedfiles <- grep("[(]\\d[)]", delayed_punishmentfiles_clean, value = T) %>% 
  gsub(" [(]\\d[)]", "", .) # 5574 all files; create 26 files that have duplicates
removeduplicatefiles <- subset(delayed_punishmentfiles_clean, !(delayed_punishmentfiles_clean %in% duplicatedfiles)) #5555, found 19 to remove
delayedpresses_df = lapply(removeduplicatefiles, progpun_presses) %>% rbindlist(fill = T) # summary looks okay, no na and left presses min 55 max 130
colnames(delayedpresses_df) = c("activepresses", "inactivepresses", "filename") # 38 NA's, 5574 files ; was 50 NA without the removal of the duplicated, empty files

delayedboxesandstations_df = lapply(removeduplicatefiles, progpun_boxes) %>% 
  rbindlist(fill = T)
colnames(delayedboxesandstations_df) = c("boxorstation", "boxorstationumber", "filename") # 5555 boxes found, no NA's

delays <- delayed_data_df_valid %>% 
  select(delay, filename) %>% 
  dplyr::filter(!is.na(delay)) %>% 
  group_by(filename, delay) %>% 
  unique()

delayedpunishment <- left_join(x = delayed_data_categories_wcats, y = delays, by = "filename") %>% # XX TOOK SCREENSHOT OF NA DELAY'S AND SENT THEM TO TOM'S TEAM
  left_join(., delayedpresses_df, by = "filename") %>% 
  left_join(., delayedboxesandstations_df, by = "filename") %>% # dim is all over the place 
  extractfromfilename() %>% # extract file information for preparation for appending to rfid
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid) ) %>% 
  mutate(shockoflastcompletedblock = ifelse(lastshock_cat == "Complete", lastshock, secondtolastshock),
            shockoflastattemptedblock = lastshock) %>%
  select(-c(numleftpressesbwlasttwo, lastshock_cat, secondtolastshock_cat, secondtolastshock, lastshock))

# to do: check the validity of the columns and the cell formatting 







##################################
# Create list of all experiments
##################################
Jhou_raw <- list(
  "runway" = runway,
  "progratio" = progratio,
  "delayedpunishment" = delaypunishment,
)

## Appending info from WFU data 
# Add rfid, sex, cohort
WFUjoin.raw <- function(rawdf){
  joindf <- merge(x = DF1, y = DF2[ , c("Client", "LO")], by = "Client", all.x=TRUE)
  return(joindf)
} 
