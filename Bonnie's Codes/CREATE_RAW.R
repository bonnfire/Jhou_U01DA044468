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
         dob = as.POSIXct(as.numeric(dob) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) %>%
  rename(labanimalid = jhoulabid,
         comment = `notesforhumans:`,
         resolution =`resolution:`)



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


# gives counts of sessions counts 
runway %>% group_by(labanimalid) %>% count() %>% group_by(n) %>% count()

# observations from the plots
# outlier in cohort 7
summary(runway$elapsedtime)
runway %>% filter(elapsedtime > 1500)
# unique resolutions are not very specific to runway data

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


### EXP 2: locomotor 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Locomotor")
bindata <- "find -type f -iname \"*LOCOMOTOR*.txt\" -exec awk '/^[1-9][0-9]*/{print FILENAME \",\" $2}' {} \; > bindata.txt" #extract the LOCOMOTOR COUNTS
system(bindata)
rawfiles_locomotor <- read.csv("bindata.txt", head = F)
colnames(rawfiles_locomotor) <- c("filename", "bincounts") 
rawfiles_locomotor_long <- rawfiles_locomotor[!grepl("[[:punct:]]", as.character(rawfiles_locomotor$bincounts)), ] # clean out invalid observations (timestamps) 
rawfiles_locomotor_long <- droplevels(rawfiles_locomotor_long) #(from 243 levels to 117)
i <- 1
j <- 1
repeat {
  rawfiles_locomotor_long$minute[i] <- paste("minute", j)
  i = i + 1
  j = j + 1
  if (rawfiles_locomotor_long$filename[i] != rawfiles_locomotor_long$filename[i-1]){
    j = 1
  }
} #add session information
rawfiles_locomotor_wide <- spread(rawfiles_locomotor_long, minute, bincounts) # spread from long to wide
cols.num <- paste("minute", c(1:30) %>% as.character())
rawfiles_locomotor_wide[cols.num] <- sapply(rawfiles_locomotor_wide[cols.num], function(x) { as.numeric(levels(x))[x]})
rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>% 
  mutate(binmeans = rowMeans(rawfiles_locomotor_wide[, names(rawfiles_locomotor_wide) != "filename"]),
         bintotal = rowSums(rawfiles_locomotor_wide[, names(rawfiles_locomotor_wide) != "filename"]))
rawfiles_locomotor_wide$labanimalid <- stringr::str_extract(rawfiles_locomotor_wide$filename, "U[[:digit:]]+[[:alpha:]]*")
rawfiles_locomotor_wide <- left_join(rawfiles_locomotor_wide, rfidandid, by = "labanimalid") # add rfid colum # add cohort column (XX WERE THESE DIVIDED INTO COHORTS)


locomotorsessionstest <- rawfiles_locomotor_wide %>%
  add_count(labanimalid) %>%
  subset(n != 1) # XX see cases of 3 and 5 and need to know how to populate the session designation; expected pairs, 2 for 1 and 2 and 4 for 1a 2a and 1b 2b

# XX only missing session (see above issue) and bodyweightperc

### EXP 3: progressive punishment 
# shocks (extract last and second to last for each session)

# get row numbers from awk nr (create_progpuntable function)
# use in sed to find the number of LEFTPRESSES
# assign yes or no to complete or attempt columns 
# group by and tail
setwd("~/Dropbox (Palmer Lab)/U01 folder/Progressive punishment") # using their copy to prevent any copy issues
files <- list.files(path=".", pattern=".*CONFLICT.*.txt", full.names=TRUE, recursive=TRUE) # some don't have the U designation bc they are not placed into the folders yet
files_clean <-  files[ ! grepl("error", files, ignore.case = TRUE) ] # clean out errors # XX DO LATER, CREATE SUBSET OF ONES THAT DON'T FOLLOW FORMAT

create_progpuntable <- function(x){
  thistrialrownumandshock = fread(paste0("awk '/THIS TRIAL/{print $1 \" \" $2 \",\" $13 \",\" NR}' ","'",x,"'"), header=F, fill=T, showProgress = F, verbose = F)  
  thistrialrownumandshock$filename<-x
  return(thistrialrownumandshock)
  }
  # thistrialrownumandshock <- as.data.frame(thistrialrownumandshock)
  # thistrialrownumandshock <- bind_rows(thistrialrownumandshock)
  # if(nrow(thistrialrownumandshock) != 0)
#   colnames(thistrialrownumandshock) = c("trialnum", "shockma", "rownum", "filename") # this line isn't working for some files for which the values cannot be found 
#   thistrialrownumandshock <- thistrialrownumandshock %>% 
#     group_by(filename) %>% 
#     tail(2) #limit the calculations of the number of LEFTPRESSES to two per filename 
#   return(thistrialrownumandshock)
# }

# before they respond, just exclude those files 

files_clean_subset <- files_clean[1:5]
data_subset = lapply(files_clean_subset, create_progpuntable)
data_subset_df <- do.call(rbind, data_subset)

data = lapply(files_clean, create_progpuntable) 
data_df <- do.call(rbind, c(data, fill = T))
colnames(data_df) = c("trialnum", "shockma", "rownum", "filename") # this line isn't working for some files for which the values cannot be found 
data_df <- data_df %>% 
  group_by(filename) %>% 
  do(tail(., 2)) #limit the calculations of the number of LEFTPRESSES to two per filename 

oneobservation <- data_df %>% count(filename) %>% subset(n!=2) # XX made note to Alen 10/18, follow up again 
oneobservation_details <- data_df %>% 
  group_by(filename) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count != 2); head(oneobservation_details) # 96 observations

data_df_valid <- data_df %>% 
  group_by(filename) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count == 2) # use the valid files before they respond about the other ones

# create categorization table 

# numofsessions <- list() # test if inside or outside function

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
    
    filelasttwoandlast <- merge(numleftpressesbwlasttwo,numleftpresseslast, by = "filename") # for each file, merge the count of LEFTPRESSES occurences
    filelasttwoandlast <- merge(filelasttwoandlast, secondtolastshock, by = "filename") # for each file, merge the MA value (last two)
    filelasttwoandlast <- merge(filelasttwoandlast, lastshock, by = "filename")
    
    names(filelasttwoandlast) = c("filename", "numleftpressesbwlasttwo","numleftpresseslast", "secondtolastshock", "lastshock")
    numofsessions[[i]] <- filelasttwoandlast # add to list 
  }
  numofsessions_df = do.call(rbind, numofsessions)
  # numofsessions <- as.data.frame(numofsessions)
  return(numofsessions_df)
}

# data2_valid_subset <- data2_valid[1:100,]
# data_categories_test = create_progpuntable_tocategorize(data2_valid_subset) # test subset 

data_categories = create_progpuntable_tocategorize(data_df_valid) # test on valid datapoints until Jhou team returns comment 

data_categories_wcat <- data_categories %>% 
  mutate(secondtolastshock_cat = ifelse(numleftpressesbwlasttwo > 3, "Complete", "Attempt"),
         lastshock_cat = ifelse(numleftpresseslast == 3, "Complete", "Attempt"))

progpun_presses <- function(x){
  presses <- fread(paste0("tac ", "'", x, "'", " | awk '/LEFT/ {print $4 \",\" $6; exit}'"), header=F, fill=T, showProgress = F, verbose = F) %>% as.data.frame()
  presses$V1[length(presses$V1) == 0] <- NA
  presses$V2[length(presses$V2) == 0] <- NA
  presses$filename <- x
  return(presses)
}
presses = lapply(files_clean, progpun_presses)
presses_df <- do.call(rbind, presses) # summary looks okay, no na and left presses min 55 max 130
colnames(presses_df) = c("activepresses", "inactivepresses", "filename")


progpun_boxes <- function(x){
  boxandstations <- fread(paste0("awk '/Started script/{print $(NF-1) \" \" $NF}' ", "'", x, "'"), header=F, fill=T, showProgress = F, verbose = F) %>% as.data.frame()
  boxandstations$filename <- x
  return(boxandstations)
}

boxesandstations = lapply(files_clean, progpun_boxes)
boxesandstations_df <- do.call(rbind, boxesandstations) # summary looks okay, no na and left presses min 55 max 130
colnames(boxesandstations_df) = c("boxorstation", "boxorstationumber", "filename")

# XXX concern (some animals have more than one box designation): 
boxesandstations_df$labanimalid <- stringr::str_extract(boxesandstations_df$filename, "U[[:digit:]]+[[:alpha:]]*")
morethanone <- boxesandstations_df %>%
  select(labanimalid, boxorstationumber) %>% 
  unique() 
morethanone2 <- morethanone %>% count(labanimalid) %>% filter(n != 1)
cases <- subset(rawfiles_box, rawfiles_box$labanimalid %in% morethanone2$labanimalid)
cases # just 6 and 8 now XX already made note to Tom Jhou's team


left_join(data_categories_wcat, progpun_presses, by = filename)  

rawfiles_shock <- extractfromfilename(rawfiles_shock)
# rawfiles_shock$orderofshocks <- with(rawfiles_shock, ave(shocks, cumsum(shocks == 0), FUN = seq_along)) - 1 # some cases in which shocks == 0 occurs consecutively

# box info (assign to each U animal)
box <- "find -type f -iname \"*CONFLICT*.txt\" -exec awk '/Started script/{print FILENAME \",\" $(NF-1) \" \" $NF}' {} \\; > box.txt"
system(box)
rawfiles_box <- read.csv("box.txt", head = F)
colnames(rawfiles_box) <- c("filename", "box") 
rawfiles_box$labanimalid <- stringr::str_extract(rawfiles_box$filename, "U[[:digit:]]+[[:alpha:]]*")
rawfiles_box <- rawfiles_box %>% 
  group_by(labanimalid) %>% 
  select(labanimalid, box) %>% 
  unique() 
rawfiles_box<- tidyr::separate(rawfiles_box, col = box, into = c("boxorstation", "boxnumber"), sep = "[[:space:]]") # include box/station info just in case it is needed for future clarification



# merge all data to create final raw table
# rawfiles_pp <- rawfiles_shock %>%
#   group_by(filename) %>% 
#   slice(tail(row_number(), 1))
# completedshocks <- rawfiles_shock %>%
#   group_by(filename) %>% 
#   slice(tail(row_number(), 2)) %>% 
#   group_by(filename) %>%
#   slice(head(row_number(), 1)) %>% 
#   select(filename, shocks)
# rawfiles_pp <- left_join(rawfiles_pp, completedshocks, by = "filename")  %>% 
#   rename(shocksattempted = shocks.x,
#          shockscompleted = shocks.y) 
# XX ATTACH CODE BACK IF APPLICABLE ONCE YOU GET THE OK FROM JHOU LAB
#  %>% mutate(box = rawfiles_box[which(rawfiles_box$labanimalid == rawfiles_pp$labanimalid), ]$boxnumber) #cannot assign box numbers is more than one for every id
subset(rawfiles_pp, shocksattempted > shockscompleted) %>% dim() #complete number of completedshocks (=2781) 
# this code allows for those that don't have any more than just one value, so same value for completed and attempted

rawfiles_pp <- left_join(rawfiles_pp, rawfiles_leverpresses_attempted, by = "filename") # add left and right presses 

### EXP 4: Progressive ratio
# max ratio
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Progressive ratio")
maxratio <- "find -type f -iname \"*RATIO*.txt\" -print0 | xargs -0 awk '/TIMEOUT/{print a \",\" FILENAME}{a=$4}' | awk '!/IS/' > maxratio.txt"
system(maxratio)
rawfiles_maxratio <- read.csv("maxratio.txt", head = F)
colnames(rawfiles_maxratio) <- c("maxratio", "filename") 
rawfiles_maxratio <- extractfromfilename(rawfiles_maxratio)
# rawfiles_maxratio %>% summary() seems like the machine generated two trials of data, so we will remove the initial one with the next line
rawfiles_maxratio <- rawfiles_maxratio[!(rawfiles_maxratio$labanimalid=="U187" & rawfiles_maxratio$maxratio==2),]

# presses
presses <- "find -type f -iname \"*RATIO*.txt\" -print0 | xargs -0 awk '/TIMEOUT/{print FILENAME \",\" $7 \"\" $9}' | awk '!/IS/' > landrpresses.txt"
rawfiles_randl <- read.csv("landrpresses.txt", head = F)
colnames(rawfiles_randl) <- c("filename", "Lpresses", "Rpresses") 

# join to create final raw df
rawfiles_pr <- left_join(rawfiles_maxratio, rawfiles_randl, by = "filename")
rawfiles_pr$labanimalid <- gsub('(U)([[:digit:]]{1})$', '\\10\\2', rawfiles_pr$labanimalid) # better organizational for ordering and consider this for other files
rawfiles_pr <- rawfiles_pr[order(rawfiles_pr$labanimalid), ]  


# EXP 5: Delayed punishment
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/Delayed punishment/")
setwd("~/Dropbox (Palmer Lab)/U01 folder/Delayed punishment") # while dropbox is updating the clone

# find -type f -iname "*PUNISHMENT*.txt" ! -path "*error*" -exec awk '/ENDING/{print FILENAME}' {} \; 

files <- list.files(path=".", pattern=".*DELAYED.*.txt", full.names=TRUE, recursive=TRUE) # exclude existing txt files and include any corrective "qualifiers" 
files_clean <-  files[ ! grepl("error", files, ignore.case = TRUE) ]  # exclude files that have errors (labelled by Jhou's team)
read_delayedpresses<-function(x){
  data = fread(paste0("tac ","'",x,"'", "| awk '/LEFTPRESSES/{print $4 \",\" $6; exit}'"), header=F, fill=T, showProgress = F)  
  data$id<-x
  data <- as.data.frame(data)
  return(data)
}
rawfiles_dpresses <- lapply(files_clean, read_delayedpresses)
rawfiles_dpresses_df <- bind_rows(rawfiles_dpresses) 
# rawfiles_dpresses <- read.csv("delayed_presses.txt", head = F) # no longer using this text file because it was based on the line above TIMEOUT
colnames(rawfiles_dpresses_df) <- c("Lpresses", "Rpresses", "filename") 
#  rawfiles_dpresses_test <- extractfromfilename(rawfiles_dpresses_df) # do this at the very end

read_delayedshocks <- function(x){
  data = fread(paste0("tac ","'",x,"'", "| grep -m2 \"MA\" | awk '{print $13}'"), header=F, fill=T, showProgress = F)  
  data$id<-x
  data <- as.data.frame(data)
  return(data)
} #get shocks 
rawfiles_dpresses <- lapply(files_clean, read_delayedpresses)


dshocks <- "find -type f -iname \"*.txt\" -exec awk '/THIS TRIAL/{print FILENAME \",\" $13}' {} + > alldelayed_shocks.txt"
system(dshocks)
rawfiles_dshocks <- read.csv("alldelayed_shocks.txt", head = F)
colnames(rawfiles_dshocks) <- c("filename", "shocks")
rawfiles_dshocks <- extractfromfilename(rawfiles_dshocks)
rawfiles_dshocks_test <- rawfiles_dshocks %>%
  group_by(filename) %>% do(tail(., n=2)) # extract last two shock values in each file
rawfiles_dshocks_test <- rawfiles_dshocks_test %>% 
  mutate(shocktype = rep(c("completed", "attempted"), length.out = n())) %>% 
  group_by(shocktype) %>%
  mutate(id = row_number()) %>%
  spread(shocktype, shocks) %>% 
  select(-id) #separate every other row to two columns
rawfiles_dshocks_check <- subset(rawfiles_dshocks_test2, attempted < completed); rawfiles_dshocks_check # check for no cases of completed > attempted (0 cases)

rawfiles_dboxes <- read.csv("delayed_boxes.txt", head = F)
colnames(rawfiles_dboxes) <- c("filename", "box")

delays <- "find -type f -iname \"*.txt\" -exec awk '/THIS TRIAL/{print FILENAME \",\" $(NF-3) \",\" $(NF-2); exit}' {} \\; > delayed_delays.txt"
system(delays)
rawfiles_ddelays <- read.csv("delayed_delays.txt", head = F)
colnames(rawfiles_ddelays) <- c("filename", "delay")
rawfiles_ddelays$delay <- as.character(rawfiles_ddelays$delay)
rawfiles_ddelays_test <- rawfiles_ddelays %>% 
  mutate(delay2 = ifelse(grepl("\\d.*SEC", rawfiles_ddelays$delay), grep("\\d.*SEC$", rawfiles_ddelays$delay, value = T), NA)) # Clean up variable bc 'delay' variable takes messy data (like EARLYSHOCK _ SEC, 33 MS, etc.)
# Clarify with Tom and then add these columns together

rawfiles_ddelays_test <- extractfromfilename(rawfiles_ddelays_test)


## Appending info from WFU data 
# Add rfid, sex, cohort
WFUjoin.raw <- function(rawdf){
  joindf <- merge(x = DF1, y = DF2[ , c("Client", "LO")], by = "Client", all.x=TRUE)
  return(joindf)
} 
