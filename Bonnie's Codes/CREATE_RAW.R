setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Bonnie's Codes")
### FROM EXCEL 

library(stringr)

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
### runway 

### locomotor 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Locomotor")
rawfiles_locomotor <- read.csv("bindata.txt", head = F)
colnames(rawfiles_locomotor) <- c("filename", "bincounts") 
rawfiles_locomotor_long <- rawfiles_locomotor[!grepl("[[:punct:]]", as.character(rawfiles_locomotor$bincounts)), ] # clean out invalid observations (timestamps) 
rawfiles_locomotor_long <- droplevels(rawfiles_locomotor_long) #(from 243 levels to 117)
i <- 1
j <- 1
repeat {
  rawfiles_locomotor_long$minute[i] <- j
  i = i + 1
  j = j + 1
  if (rawfiles_locomotor_long$filename[i] != rawfiles_locomotor_long$filename[i-1]){
    j = 1
  }
} #add session information
rawfiles_locomotor_wide <- spread(rawfiles_locomotor_long, minute, bincounts) # spread from long to wide
cols.num <- c(1:30) %>% as.character()
rawfiles_locomotor_wide[cols.num] <- sapply(rawfiles_locomotor_wide[cols.num], function(x) { as.numeric(levels(x))[x]})
rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>% 
  mutate(binmeans = rowMeans(rawfiles_locomotor_wide[, names(rawfiles_locomotor_wide) != "filename"]),
         bintotal = rowSums(rawfiles_locomotor_wide[, names(rawfiles_locomotor_wide) != "filename"]))
rawfiles_locomotor_wide$labanimalid <- stringr::str_extract(rawfiles_locomotor_wide$filename, "U[[:digit:]]+[[:alpha:]]*")

# rawfiles_locomotor_melt <- melt(rawfiles_locomotor_wide,  id.vars = 'filename', variable.name = 'animals')
# ggplot(rawfiles_locomotor_melt, aes(filename,value)) + geom_line(aes(colour = animals))
ggplot(data = rawfiles_locomotor_long, aes(x = minute, y = bincounts, color = filename, group = filename))+
  geom_path(show.legend = FALSE) + geom_point(show.legend = F)


### shocks (extract last and second to last for each session)
rawfiles_shock <- read.csv("startshock.txt", head = F)
colnames(rawfiles_shock) <- c("filename", "shocks") 
extractfromfilename <- function(df){
  df$labanimalid <- stringr::str_extract(df$filename, "U[[:digit:]]+[[:alpha:]]*")
  df$date <- stringr::str_extract(df$filename, "[[:digit:]]{4}[-][[:digit:]]{4}")
  df$date <- as.Date(df$date, "%Y-%m%d") 
  df$time <- stringr::str_extract(df$filename, "[[:digit:]]{4}(?=_)")
  df$time <- gsub('([[:digit:]]{2})([[:digit:]]{2})', '\\1:\\2', df$time)
  df <- df[order(df$filename), ]
  return(df)
}

rawfiles_shock <- extractfromfilename(rawfiles_shock)
rawfiles_shock$orderofshocks <- with(rawfiles_shock, ave(shocks, cumsum(shocks == 0), FUN = seq_along)) - 1
head(rawfiles_shock, 20)

# define session
# XX 

### lever presses (extract last active and inactive for each session)
rawfiles_leverpresses <- read.csv("leverpresses.txt", head = F)
colnames(rawfiles_leverpresses) <- c("filename", "activepresses", "inactivepresses") 
rawfiles_leverpresses <- extractfromfilename(rawfiles_leverpresses)
i <- 1
j <- 0
repeat {
  rawfiles_leverpresses$trialorder[i] <- j
  i = i + 1
  j = j + 1
  if (rawfiles_leverpresses$filename[i] != rawfiles_leverpresses$filename[i-1]){
    j = 0
  }
}
head(rawfiles_leverpresses, 20)

### box info (assign to each U animal)
rawfiles_box <- read.csv("box.txt", head = F)
colnames(rawfiles_box) <- c("filename", "box") 
rawfiles_box$labanimalid <- stringr::str_extract(rawfiles_box$filename, "U[[:digit:]]+")
rawfiles_box <- rawfiles_box %>% 
  group_by(labanimalid) %>% 
  select(labanimalid, box) %>% 
  unique() 
## XX BRING UP TO TOM JHOU #######################
countid <- rawfiles_box %>% count(labanimalid)
subset(countid, n != 1)
##################################################

# when all files are ready to be created into the prog punishment table 
attemptedshocks <- rawfiles_shock %>% 
  group_by(filename) %>% 
  slice(which.max(orderofshocks)) %>% 
  ungroup()
attemptedshocks <- rename(attemptedshocks, attemptedshocks = shocks)  

completedshocks <- rawfiles_shock %>% 
  group_by(filename) %>% 
  slice(which.max(orderofshocks)-1) %>% 
  ungroup()
completedshocks <- rename(completedshocks, completedshocks = shocks)  

attemptedpresses <- rawfiles_leverpresses %>% 
  group_by(filename) %>% 
  slice(which.max(trialorder))

rawfiles_pp <- merge(attemptedshocks, completedshocks[ , c("filename", "completedshocks")], by = "filename", all.x = T) %>% merge(attemptedpresses) 
subset(rawfiles_pp, completedshocks < attemptedshocks) %>% dim() #complete number of completedshocks (=2781)

# template if needed (unused function and data) 
shocks_raw.UNUSED <- as.data.frame(setNames(replicate(7,numeric(0), simplify = F), c("session", "date", "lastcompletedintensity", "lastattempedintensity","numberoftrialsatlastshockintensity","attemptedactivepresses","attemptedinactivepresses")))
# seems like it is completedactivepresses
extract.raw.pp.UNUSED <- function(experiment_name) {
  setwd(paste0("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/", experiment_name,"/"))
  files <- list.files(path=".", pattern=".txt", full.names=T, recursive=TRUE)[1:100]
  files <- gsub(" ", "\\ ", files, fixed = T)
  read_leverpresses<-function(x){
    df <- fread(paste0("awk '/THIS TRIAL/{print $13}'", "'", x,"'","| tail -n 2 | head -n 1"), header=F, fill=T, sep = " ")
    df$id <- x
    return(df)
  }
  all <- lapply(files, read_leverpresses)
  return(all)
}

# Progressive ratio
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Progressive ratio")
rawfiles_maxratio <- read.csv("maxratio.txt", head = F)
colnames(rawfiles_maxratio) <- c("maxratio", "filename") 
rawfiles_maxratio <- extractfromfilename(rawfiles_maxratio)
# rawfiles_maxratio %>% summary() seems like the machine generated two trials of data, so we will remove the initial one with the next line
rawfiles_maxratio <- rawfiles_maxratio[!(rawfiles_maxratio$labanimalid=="U187" & rawfiles_maxratio$maxratio==2),]

rawfiles_randl <- read.csv("landrpresses.txt", head = F)
colnames(rawfiles_randl) <- c("filename", "Lpresses", "Rpresses") 
rawfiles_randl <- extractfromfilename(rawfiles_randl)

rawfiles_pr <- merge(rawfiles_maxratio, rawfiles_randl[, c("filename", "Lpresses", "Rpresses")], by = "filename") 
rawfiles_pr$labanimalid <- gsub('(U)([[:digit:]]{1})$', '\\10\\2', rawfiles_pr$labanimalid)
rawfiles_pr <- rawfiles_pr[order(rawfiles_pr$labanimalid), ]  


# Delayed punishment
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Delayed punishment/")
rawfiles_dpresses <- read.csv("delayed_presses.txt", head = F)
colnames(rawfiles_dpresses) <- c("filename", "Lpresses", "Rpresses") 
rawfiles_dpresses <- extractfromfilename(rawfiles_dpresses)

rawfiles_dshocks <- read.csv("alldelayed_shocks.txt", head = F)
colnames(rawfiles_dshocks) <- c("filename", "shocks")
rawfiles_dshocks <- extractfromfilename(rawfiles_dshocks)
rawfiles_dshocks_test <- rawfiles_dshocks %>%
  group_by(filename) %>% do(tail(., n=2)) # extract interested shock values 
rawfiles_dshocks_test <- rawfiles_dshocks_test %>% 
  mutate(shocktype = rep(c("completed", "attempted"), length.out = n())) %>% 
  group_by(shocktype) %>%
  mutate(id = row_number()) %>%
  spread(shocktype, shocks) %>% 
  select(-id) #separate every other row to two columns
rawfiles_dshocks_check <- subset(rawfiles_dshocks_test2, attempted < completed); rawfiles_dshocks_check # check for no cases of completed > attempted (0 cases)

rawfiles_dboxes <- read.csv("delayed_boxes.txt", head = F)
colnames(rawfiles_dboxes) <- c("filename", "box")

rawfiles_ddelays <- read.csv("delayed_delays.txt", head = F)
colnames(rawfiles_ddelays) <- c("filename", "delay")
rawfiles_ddelays$delay <- as.character(rawfiles_ddelays$delay)
rawfiles_ddelays_test <- rawfiles_ddelays %>% 
  mutate(delay2 = ifelse(grepl("\\d.*SEC", rawfiles_ddelays$delay), grep("\\d.*SEC$", rawfiles_ddelays$delay, value = T), NA))
# Clarify with Tom and then add these columns together

rawfiles_ddelays_test <- extractfromfilename(rawfiles_ddelays_test)
## Appending info from WFU data 
# Add rfid, sex, cohort
WFUjoin.raw <- function(rawdf){
  joindf <- merge(x = DF1, y = DF2[ , c("Client", "LO")], by = "Client", all.x=TRUE)
  return(joindf)
} 

hist(iris$Sepal.Length)
