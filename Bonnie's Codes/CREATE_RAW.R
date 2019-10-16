setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Bonnie's Codes")
### FROM EXCEL 

library(stringr)
library(dplyr)
library(data.table)
library(tidyr)

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


#### FROM RAW FILES 
## Text file preparation
### EXP 1: runway 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/Runway")
# reach time 
reachtime <- "find -type f -iname \"*.txt\" -print0 | xargs -0 awk '/REACHED/{print $1 \", \" FILENAME}' > reachrunway.txt"
system(reachtime)
rawfiles_reach <- read.csv("reachrunway.txt", head = F)
colnames(rawfiles_reach) <- c("reachtime", "filename") 
rawfiles_reach$filename <- as.character(rawfiles_reach$filename)
rawfiles_reach$filename <- sub(" ", "", rawfiles_reach$filename)
rawfiles_reach$reachtime <- as.numeric(as.character(rawfiles_reach$reachtime))

# location2.txt
location2 <-"find -type f -iname \"*.txt\" -print0 | xargs -0 grep -P -m 1 \"LOCATION\\s\\t2\" > location2times.txt"
system(location2)
rawfiles_location2 <- read.csv("location2times.txt", head = F)
rawfiles_location2_clean <- separate(rawfiles_location2, V1, into = c("filename", "loc2time"), sep = "[:]")
rawfiles_location2_clean$loc2time <- gsub("\\t.+", "", rawfiles_location2_clean$loc2time, perl = T) %>% 
  as.numeric()

# join together and calculate 
rawfiles_prepcalc <- left_join(rawfiles_reach, rawfiles_location2_clean, by = "filename")
rawfiles_prepcalc$diff = trunc(rawfiles_prepcalc$reachtime) - trunc(rawfiles_prepcalc$loc2time)
# sort by ascending filename and get animal id and exp date/time
rawfiles_prepcalc <- arrange(rawfiles_prepcalc, filename)
rawfiles_prepcalc <- extractfromfilename(rawfiles_prepcalc)
# rfid and lab animal id 
rfidandid <- WFU_Jhou_test_df %>% 
  select(labanimalnumber, rfid) %>% 
  transmute(labanimalid = paste0("U", stringr::str_extract(labanimalnumber, "[1-9]+[0-9]*")),
            rfid = rfid) # labanimalid vs labanimalnumber
rawfiles_prepcalc <- left_join(rawfiles_prepcalc, rfidandid, by = "labanimalid") # add rfid column 

# graphics for email: subset(rawfiles_prepcalc, is.na(rfid)) %>% select(labanimalid) %>% unique AND subset(rawfiles_prepcalc, is.na(loc2time))$filename

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

## RUNWAY REVERSAL
reversals <- "find -type f -iname \"*.txt\" -print0 | xargs -0 grep -c \"REVERSAL\" > reversals.txt"
system(reversals)
reversals <- read.csv("reversals.txt", head = F)
reversals <- separate(reversals, V1, into = c("filename", "reversals"), sep = "[:]")

# bind reversals with runway data 
runway <- left_join(rawfiles_prepcalc, reversals, by = "filename")

### EXP 2: locomotor 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Locomotor")
bindata <- "find -type f -iname \"*.txt\" -exec awk '/^[1-9][0-9]*/{print FILENAME \",\" $2}' {} \; > bindata.txt" #extract the LOCOMOTOR COUNTS
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
# add rfid once I understand how TJ values are generated
rawfiles_locomotor_wide <- left_join(rawfiles_locomotor_wide, rfidandid, by = "labanimalid") # add rfid column
rawfiles_locomotor_wide <- left_join(rawfiles_locomotor_wide, rfidandid, by = "labanimalid") # add cohort column (xx WERE THESE DIVIDED INTO COHORTS)


locomotorsessionstest <- rawfiles_locomotor_wide %>%
  add_count(labanimalid) %>%
  subset(n != 1) # XX see cases of 3 and 5 and need to know how to populate the session designation

### EXP 3: progressive punishment 
# shocks (extract last and second to last for each session)
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/Progressive punishment")
startshock <- "find -type f -iname \"*.txt\" -exec awk '/THIS TRIAL/{print FILENAME \",\" $4 \",\" $6 \",\" $13}' {} \\; > startshock.txt"
system(startshock)
rawfiles_shock <- read.csv("startshock.txt", head = F)
colnames(rawfiles_shock) <- c("filename", "shocks") 
extractfromfilename <- function(df){
  if(df == "rawfiles_prepcalc"){
    df$cohort <- stringr::str_extract(df$filename, "Cohort [[:digit:]]")
  }
  df$labanimalid <- stringr::str_extract(df$filename, "U[[:digit:]]+[[:alpha:]]*")
  df$date <- stringr::str_extract(df$filename, "[[:digit:]]{4}[-][[:digit:]]{4}")
  df$date <- as.Date(df$date, "%Y-%m%d") 
  df$time <- stringr::str_extract(df$filename, "[[:digit:]]{4}(?=_)")
  df$time <- gsub('([[:digit:]]{2})([[:digit:]]{2})', '\\1:\\2', df$time)
  df <- df[order(df$filename), ]
  return(df)
} 
rawfiles_shock <- extractfromfilename(rawfiles_shock)
# rawfiles_shock$orderofshocks <- with(rawfiles_shock, ave(shocks, cumsum(shocks == 0), FUN = seq_along)) - 1 # some cases in which shocks == 0 occurs consecutively

# lever presses (import all presses, but extract last attempted active and inactive for each session)
levelpresses <- "find -type f -iname \"*.txt\" -exec awk '/THIS TRIAL/{print FILENAME \",\" $4 \",\" $6}' {} \\; > leverpresses.txt"
system(levelpresses)
rawfiles_leverpresses <- read.csv("leverpresses.txt", head = F)
colnames(rawfiles_leverpresses) <- c("filename", "activepresses", "inactivepresses") 
# rawfiles_leverpresses <- extractfromfilename(rawfiles_leverpresses)
rawfiles_leverpresses_attempted <- rawfiles_leverpresses %>% group_by(filename) %>% slice(tail(row_number(), 1))

# box info (assign to each U animal)
box <- "find -type f -iname \"*.txt\" -exec awk '/Started script/{print FILENAME \",\" $(NF-1) \" \" $NF}' {} \\; > box.txt"
system(box)
rawfiles_box <- read.csv("box.txt", head = F)
colnames(rawfiles_box) <- c("filename", "box") 
rawfiles_box$labanimalid <- stringr::str_extract(rawfiles_box$filename, "U[[:digit:]]+[[:alpha:]]*")
rawfiles_box <- rawfiles_box %>% 
  group_by(labanimalid) %>% 
  select(labanimalid, box) %>% 
  unique() 
rawfiles_box<- tidyr::separate(rawfiles_box, col = box, into = c("boxorstation", "boxnumber"), sep = "[[:space:]]") # include box/station info just in case it is needed for future clarification

# XXX concern (some animals have more than one box designation): 
morethanone <- rawfiles_box %>%
  group_by(labanimalid) %>% 
  select(labanimalid, boxnumber) %>% 
  unique() 
morethanone2 <- morethanone %>% count(labanimalid) %>% filter(n != 1)
cases <- subset(rawfiles_box, rawfiles_box$labanimalid %in% morethanone2$labanimalid)
cases

# merge all data to create final raw table
rawfiles_pp <- rawfiles_shock %>%
  group_by(filename) %>% 
  slice(tail(row_number(), 1))
completedshocks <- rawfiles_shock %>%
  group_by(filename) %>% 
  slice(tail(row_number(), 2)) %>% 
  group_by(filename) %>%
  slice(head(row_number(), 1)) %>% 
  select(filename, shocks)
rawfiles_pp <- left_join(rawfiles_pp, completedshocks, by = "filename")  %>% 
  rename(shocksattempted = shocks.x,
         shockscompleted = shocks.y) 
# XX ATTACH CODE BACK IF APPLICABLE ONCE YOU GET THE OK FROM JHOU LAB
#  %>% mutate(box = rawfiles_box[which(rawfiles_box$labanimalid == rawfiles_pp$labanimalid), ]$boxnumber) #cannot assign box numbers is more than one for every id
subset(rawfiles_pp, shocksattempted > shockscompleted) %>% dim() #complete number of completedshocks (=2781) 
# this code allows for those that don't have any more than just one value, so same value for completed and attempted

### EXP 4: Progressive ratio
# max ratio
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Progressive ratio")
maxratio <- "find -type f -iname \"*.txt\" -print0 | xargs -0 awk '/TIMEOUT/{print a \",\" FILENAME}{a=$4}' | awk '!/IS/' > maxratio.txt"
system(maxratio)
rawfiles_maxratio <- read.csv("maxratio.txt", head = F)
colnames(rawfiles_maxratio) <- c("maxratio", "filename") 
rawfiles_maxratio <- extractfromfilename(rawfiles_maxratio)
# rawfiles_maxratio %>% summary() seems like the machine generated two trials of data, so we will remove the initial one with the next line
rawfiles_maxratio <- rawfiles_maxratio[!(rawfiles_maxratio$labanimalid=="U187" & rawfiles_maxratio$maxratio==2),]

# presses
rawfiles_randl <- read.csv("landrpresses.txt", head = F)
colnames(rawfiles_randl) <- c("filename", "Lpresses", "Rpresses") 

# join to create final raw df
rawfiles_pr <- left_join(rawfiles_maxratio, rawfiles_randl, by = "filename")
rawfiles_pr$labanimalid <- gsub('(U)([[:digit:]]{1})$', '\\10\\2', rawfiles_pr$labanimalid) # better organizational for ordering and consider this for other files
rawfiles_pr <- rawfiles_pr[order(rawfiles_pr$labanimalid), ]  


# EXP 5: Delayed punishment
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Delayed punishment/")
files <- list.files(path=".", pattern=".*DELAYED.*.txt", full.names=TRUE, recursive=TRUE) # exclude existing txt files and include any corrective "qualifiers" 
read_delayedpresses<-function(x){
  data = fread(paste0("tac ","'",x,"'", "| awk '/LEFTPRESSES/{print $4 \",\" $6; exit}'"), header=F, fill=T, showProgress = F)  
  data$id<-x
  data <- as.data.frame(data)
  return(data)
}
rawfiles_dpresses <- lapply(files, read_delayedpresses)
rawfiles_dpresses_test <- bind_rows(rawfiles_dpresses) 
# rawfiles_dpresses <- read.csv("delayed_presses.txt", head = F) # no longer using this text file because it was based on the line above TIMEOUT
colnames(rawfiles_dpresses_test) <- c("Lpresses", "Rpresses", "filename") 
rawfiles_dpresses_test <- extractfromfilename(rawfiles_dpresses_test)


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

delays <- "find -type f -iname \"*.txt\" -exec awk '/THIS TRIAL/{print FILENAME \",\" $(NF-3) \" \" $(NF-2); exit}' {} \\; > delayed_delays.txt"
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
