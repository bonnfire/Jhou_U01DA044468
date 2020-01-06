setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Bonnie's Codes")
### FROM EXCEL 

library(stringr)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyxl)
library(readxl)

##### U01 Behavioral Protocols

# Each rat receives the following schedule of tests: 
#   
#   Intravenous catheter implantation, followed by 1 week recovery. 
# 
#   Runway testing. 3 habituation sessions + 12 cocaine sessions. At 3-4 sessions/day, this takes 4-5 days. 
# 
#   Locomotor testing. 2 sessions (instituted starting with cohort 8.3, U311, with 1 session, then extended to 2 sessions thereafter). 
# 
#   Food deprivation until rat reaches 85% of initial body weight (3 days). Rats are maintained at 85% hereafter until completion of testing schedule. 
# 
#   Lever-press training (approximately 12-16 sessions) 
# 
#   Progressive Shock Task (7 sessions) 
# 
#   Progressive Ratio (4 sessions) 
# 
#   Locomotor testing (2 sessions) 
# 
#   Delayed Punishment Task (8 sessions) 


## self-defined functions 

# extract info from filename
extractfromfilename <- function(df){
  # if(df == "rawfiles_prepcalc"){
  #   df$cohort <- stringr::str_extract(df$filename, "Cohort [[:digit:]]")
  # }
  df$labanimalid <- stringr::str_extract(toupper(df$filename), "U[[:digit:]]+[[:alpha:]]*")
  df$date <- gsub("[-]([[:digit:]]{2})([[:digit:]]{2})", "-\\1-\\2", stringr::str_extract(df$filename, "[[:digit:]]{4}[-][[:digit:]]{4}"))
  df$date <- as.POSIXct(df$date, tz = "UTC")
  df$time <- stringr::str_extract(df$filename, "[[:digit:]]{4}(?=_)")
  df$time <- gsub('([[:digit:]]{2})([[:digit:]]{2})', '\\1:\\2', df$time)
  df <- df[order(df$filename), ]
  return(df)
} 

################################
##### RAW TEXT DATE/TIME #######
################################

setwd("~/Dropbox (Palmer Lab)/U01 folder/")
allexpfiles_raw <- list.files(path=".", pattern="*.txt", full.names=TRUE, recursive=TRUE) 
allexpfiles_raw <- allexpfiles_raw[ ! grepl("error", allexpfiles_raw, ignore.case = TRUE) ] 
x <- c("unclassified", "error", "[()]")
allexpfiles_raw_clean <- allexpfiles_raw[! grepl(paste(x, collapse = "|"), allexpfiles_raw)] 
readdate <- function(x){
  experimentdatetime <- fread(paste0("grep -m1 -o \"[0-9]\\\\{2\\\\}/[0-9]\\\\{2\\\\}/[0-9]\\\\{4\\\\}\" ", "'", x, "'"), header = F, fill = T)
  experimentdatetime$filename <- x
  return(experimentdatetime)
}

readtime <- function(x){
  experimentdatetime <- fread(paste0("grep -m1 -o \"[0-9]\\\\{2\\\\}:[0-9]\\\\{2\\\\}:[0-9]\\\\{2\\\\}\\\\.[0-9]\\\\{3\\\\}\" ", "'", x, "'"), header = F, fill = T)
  experimentdatetime$filename <- x
  return(experimentdatetime)
}


allexpfiles_date <- lapply(allexpfiles_raw_clean, readdate) %>% rbindlist(fill = T) %>% rename("dateinfile" = "V1") # 21131 files in raw clean files vector; 2 NA ./Delayed punishment/U417/2019-0920-1109_417_DELAYED PUNISHMENT.txt ./Delayed punishment/U418/2019-0920-1109_418_DELAYED PUNISHMENT.txt
allexpfiles_date[which(allexpfiles_date$filename == "./Delayed punishment/U417/2019-0920-1109_417_DELAYED PUNISHMENT.txt"),]$dateinfile <- "09/20/2019"
allexpfiles_date[which(allexpfiles_date$filename == "./Delayed punishment/U418/2019-0920-1109_418_DELAYED PUNISHMENT.txt"),]$dateinfile <- "09/20/2019"

allexpfiles_time <- lapply(allexpfiles_raw_clean, readtime) %>% rbindlist(fill = T) %>% rename("timeinfile" = "V1") # same two were empty
allexpfiles_time[which(allexpfiles_time$filename == "./Delayed punishment/U417/2019-0920-1109_417_DELAYED PUNISHMENT.txt"),]$timeinfile <- "11:09:57.860"
allexpfiles_time[which(allexpfiles_time$filename == "./Delayed punishment/U418/2019-0920-1109_418_DELAYED PUNISHMENT.txt"),]$timeinfile <- "11:09:57.875"


allexpfiles_datetime <- left_join(allexpfiles_date, allexpfiles_time, by = "filename") %>% 
  extractfromfilename() %>%
  rename("date_filename" = "date",
         "timefilename" = "time",
         "subdirectoryname" = "labanimalid") %>%
  mutate(dateinfile = as.POSIXct(dateinfile, tz = "UTC", format = "%m/%d/%Y"),
         timeinfile = str_extract(timeinfile, "\\d+:\\d+")) 
allexpfiles_datetime %>% 
  dplyr::filter(dateinfile != date_filename | 
           timeinfile != timefilename)

allexpfiles_datetime %>% 
  dplyr::filter(timeinfile != timefilename) # results saved in Dropbox documentation (Questions_1)


# know the number of na's in dataframe (currently 0) colSums(is.na(allexpfiles_datetime))

################################
### RAW EXCEL Weights ##########
################################

gs_auth(new_user = TRUE)

  ################################
### RAW TEXT Runway ############
################################

### EXP 1: runway 

# 3 habituation sessions + 12 cocaine sessions. At 3-4 sessions/day, this takes 4-5 days.

# collect habituation data, label not habituated, use id's that did habituate to extract reach time, location 2, number of reversals from the Runway directory
setwd("~/Dropbox (Palmer Lab)/U01 folder/Runway habituation")
runwayhab_files_2 <- system("grep -rnwe 'CALCULATED SYRINGE DURATION USING 10ML SYRINGE IS 0 SECONDS'", intern = T) %>% 
  gsub(":.*", "", x = .) %>% 
  paste0("./", .)

setdiff(runwayhab_files_clean, runwayhab_files_2)


# runway hab files should only be 0 weights, so the non zero cases need to be noted 
# readrunwayhab_weight <- function(x){
#   runwayhab_weight <- fread(paste0("awk '/WEIGHT/{print $4}' ", "'", x, "'"), fill = T)
#   runwayhab_weight$filename <- x
#   return(runwayhab_weight)
# }
runwayhab_files <- list.files(path = "." , pattern = "*.txt", full.names = T, recursive = T)  # 1271 raw text files 

str_detect(runwayhab_files, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_RUNWAY.txt", negate = T) %>% any() # find strings that don't match the expected template
runwayhab_files_clean <- runwayhab_files[str_detect(runwayhab_files, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_RUNWAY.txt", negate = F)] # 1269 files turn negate into T to see the different cases; turned into comment temporarily until _RUNWAY_ case is solved (U273)
# runwayhab_files_clean <- grep("^((?!error).)*$", runwayhab_files, value = T, perl = T) # mimic inverse matching with negative look arounds  # filter for clean filenames 
# runwayhab_weights <- lapply(runwayhab_files_clean, readrunwayhab_weight) %>% 
#   rbindlist(fill = T) %>% 
#   rename("ratweight" = "V1")
# runwayhab_weights_validfiles <- runwayhab_weights %>% extractfromfilename() %>% dplyr::filter(ratweight == 0) %>% select(filename) # 1262 valid files with 0 weights 
runwayhab_weights_validfiles <- runwayhab_files_clean[which(runwayhab_files_clean %in% runwayhab_files_2)] # 1367 valid files with 0 parameters

# readrunwayhab <- function(x){
#   runwayhab_reach <- fread(paste0("awk '/REACHED/{print $1}' ", "'", x, "'"), fill = T)
#   runwayhab_reach$filename <- x
#   if(grepl("U194|U197|U198|U199|U262|U415|U96", x, ignore.case = T)){
#     runwayhabloc2 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t3\" ", "'", x, "'"), fill = T) # XX figure out how this vector of id's is being generated 
#   } else{
#   runwayhabloc2 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t2\" ", "'", x, "'"), fill = T)
#   }
#   runwayhabloc2$filename <- x
#   
#   runwayhab <- merge(runwayhab_reach, runwayhabloc2, by = "filename", all = T)
#   return(runwayhab)
# }

readrunwayhab <- function(x){
  runwayhab_reach <- fread(paste0("awk '/REACHED/{print $1}' ", "'", x, "'"), fill = T)
  runwayhab_reach$filename <- x
  
  runwayhabloc1 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t1\" ", "'", x, "'"))
  runwayhabloc1$filename <- x
  
    if(grepl("U194|U197|U198|U199|U262|U415|U96", x, ignore.case = T)){
      runwayhabloc2 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t3\" ", "'", x, "'"), fill = T) # XX figure out how this vector of id's is being generated
    } else{
    runwayhabloc2 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t2\" ", "'", x, "'"), fill = T)
    }
  runwayhabloc2$filename <- x
  
  runwayhablocs <- merge(runwayhab_reach, merge(runwayhabloc1, runwayhabloc2, by = "filename"), by = "filename")
  
  return(runwayhablocs)
}

runwayhab_v_ <- lapply(runwayhab_weights_validfiles, readrunwayhab) %>% 
  rbindlist(fill = T) 
runwayhab_v_ %<>% 
  rename("hab_reachtime" = "V1",
         "hab_loc1" = "V1.x",
         "hab_location1" = "V2.x",
         "hab_locationnum1" = "V3.x",
         "hab_loc2_3_reachtime" = "V1.y",
         "hab_location2" = 'V2.y', 
         "hab_locationnum2" = "V3.y"
         #,"whatisthis" = "V1"
         ) # 1262 files

runwayhab_v_ %>% naniar::vis_miss()
runwayhab_v_ %>% dplyr::filter(is.na(hab_reachtime)) %>% select(filename) %>% unlist() %>% as.character()
runwayhab_v_ %>% dplyr::filter(!is.na(V2)) 
runwayhab_v_ %>% dplyr::filter(is.na(hab_loc1)|is.na(hab_reachtime)) 

# %>% select(filename) %>% unlist() %>% as.character()


runwayhab_reversals <- lapply(runwayhab_weights_validfiles, read_runwayrevs) %>% rbindlist(fill = T) %>% rename("reversals" = "V1")


# vis_miss(runwayhab_v_) wherever hab_reachtime is na so is hab_loc2_reachtime
# runwayhab_v_ %>% dplyr::filter(is.na(hab_reachtime)) %>% vis_miss()
runwayhab_v_removelastcolumn <- runwayhab_v_ %>% 
  dplyr::filter(is.na(whatisthis), !is.na(hab_reachtime) ) %>% 
  select(-whatisthis) %>% 
  mutate(elapsedtime = trunc(hab_reachtime) - trunc(hab_loc2_3_reachtime)) %>% 
  left_join(., runwayhab_reversals, by = "filename")  %>%
  extractfromfilename()
  #1205 files (complete cases)


# 1/2 Maya email: "when there are more than 2, yes you can extract the last consecutive 2 sessions"
runwayhab_v_tail2 <- runwayhab_v_removelastcolumn %>%
  extractfromfilename() %>%
  group_by(labanimalid) %>%
  do(tail(., n=2)) %>% #878 lab animalid
  ungroup()


# include the files that were lost in the box/dropbox transition
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Tom_Jhou_U01DA044468_Dropbox_copy/Runway")
u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 
runwayhab_missing <- u01.importxlsx('U01 HS missing data habituation.xlsx')
runwayhab_missing[[1]]$sex <- 'M'
runwayhab_missing[[2]]$sex <- 'F'
runwayhab_missing %<>% 
  rbindlist() 
names(runwayhab_missing) <- tolower(names(runwayhab_missing) )
runwayhab_missing %<>%   
  dplyr::filter(grepl("Habituation", trial, ignore.case = T)) 
runwayhab_missing_rat_indices <- grep("U\\d+", runwayhab_missing$rat, ignore.case = T)
runwayhab_missing_split <- split(runwayhab_missing, cumsum(1:nrow(runwayhab_missing) %in% runwayhab_missing_rat_indices))
runwayhab_missing_formatted <- lapply(runwayhab_missing_split, function(x){
  x <- x %>% 
    mutate(rat = head(rat, 1))
  return(x)
}) %>% rbindlist() %>% 
  mutate(date = as.POSIXct(as.numeric(date) * (60*60*24), origin = "1899-12-30", tz="GMT")) %>% 
  rename("labanimalid" = "rat",
         "hab_loc1_reachtime" = "start latency", 
         "hab_loc2_3_reachtime" = "start latency 2",
         "hab_reachtime" = "goal times", 
         "elapsedtime" = "run time", 
         "reversals" = "# of reversals")
# runwayhab_notes_fromexcel <- tJhou_Runway_notes %>% dplyr::filter(grepl("habituate", notes, ignore.case = T )) 
# 
# runwayhab_v_explainna <- runwayhab_v_removelastcolumn %>%
#   extractfromfilename() %>%
#   mutate(notedinexcel = ifelse(labanimalid %in% runwayhab_notes_fromexcel$animalid, runwayhab_notes_fromexcel$notes, NA),
#          cannotfindreachtime = ifelse(labanimalid %in% notexplainedinexcelbutmissing$labanimalid,"Cannot find reach time", NA))
# 
# notexplainedinexcelbutmissing <- runwayhab_v_explainna %>%   
#   dplyr::filter((is.na(hab_reachtime) | is.na(hab_loc2_3_reachtime)), is.na(notedinexcel) ) 
# 
# # animals that only have two files but one is na so what to do? 
# runwayhab_v_explainna %>% dplyr::filter(!is.na(cannotfindreachtime)) %>% group_by(labanimalid) %>% add_count() %>% dplyr::filter(n == 2) %>% select(labanimalid) %>% unique()
# # gives you the context for which you are missing the files from
# runwayhab_v_explainna %>% dplyr::filter(!is.na(cannotfindreachtime)) %>% group_by(labanimalid) %>% add_count() %>% rename("numberoffilesindir"= "n") %>% dplyr::filter(is.na(hab_reachtime)) %>% add_count() %>% rename("numberofnafilesindir" = "n") %>% View()
# 

## extract last two relevant files; include this subset in email as concern
runwayhab_v_removelastcolumn %>% add_count(labanimalid) %>% dplyr::filter( n == 1) %>% select(filename)

## combine two sources of data
runwayhab_merge <- rbindlist(list(runwayhab_v_tail2, runwayhab_missing_formatted), fill = T) %>% 
  mutate_at(c("hab_reachtime", "hab_loc2_3_reachtime", "elapsedtime","reversals"), as.numeric)









setwd("~/Dropbox (Palmer Lab)/U01 folder/Runway")

# reach time and qc the times from in files
readrunway <- function(x){
  runway <- fread(paste0("awk '/REACHED/{print $1}' ", "'", x, "'"), fill = T)
  runway$filename <- x
  return(runway)
}


runwayfiles_clean <- list.files(path=".", pattern=".*RUNWAY.*.txt", full.names=TRUE, recursive=TRUE) #3983 files 
# runwayfiles_clean[grepl("^m.*\\.log",runwayfiles_clean)] # remove error and duplicate files

str_detect(runwayfiles_clean, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_RUNWAY.txt", negate = T) %>% any() # find strings that don't match the expected template
runwayfiles_clean <- runwayfiles_clean[str_detect(runwayfiles_clean, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_RUNWAY.txt", negate = F)] # 3929 files turn negate into T to see the different cases; turned into comment temporarily until _RUNWAY_ case is solved (U273)

# note the 4221 id in one file, but seems to be no error files so below code is unneeded
# files_clean <-  files[ ! grepl("error", files, ignore.case = TRUE) ] 
# runwayfiles_clean <- gsub(" ", "\\\\ ", runwayfiles_clean) # not the issue for not being able to access the files

runway_reach <- lapply(runwayfiles_clean, readrunway) 
runway_reach_df <- runway_reach %>% 
  rbindlist(fill = T) %>% # run on runwayfiles_clean[3000:3100] for test # 12/3 - 36 warnings of returning NULL data table
  rename("reachtime" = "V1")

# evaluate NA reachtime cases
runway_reachtimeNA <- subset(runway_reach_df, is.na(reachtime)==T) ## XX Made note to Maya already; renoted 12/3; create subset to go back for reference; reassign the data var
runway_reach_df %<>% dplyr::filter(!is.na(reachtime))

# location2 and location3 (use to sub loc2 when na) time + add loc1 (1/2/20)
readrunwayloc2_3 <- function(x){
  runwayloc1 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t1\" ", "'", x, "'"))
  runwayloc1$filename <- x
  
  runwayloc2 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t2\" ", "'", x, "'"))
  runwayloc2$filename <- x
  
  runwayloc3 <- fread(paste0("grep -P -m 1 \"LOCATION\\s\\t3\" ", "'", x, "'"))
  runwayloc3$filename <- x
  
  runwaylocs <- merge(runwayloc1, merge(runwayloc2, runwayloc3, by = "filename"), by = "filename")
  
  return(runwaylocs)
}

runway_loc2_3 <- lapply(runwayfiles_clean, readrunwayloc2_3) # test with runwayfiles_clean[1:10]


# lapply("./U102/2018-1201-1732_102_RUNWAY.txt", readrunwayloc2_3)

runway_loc2_3_df <- lapply(runway_loc2_3, function(x){
  # names(x) <- mgsub::mgsub(names(x),
  #                          c("\\.x", "1\\.y", "2\\.y", "3\\.y"),
  #                          c("", "4", "5", "6"))
  if(ncol(x) == 10){
  # x <- x %>% rename("loc2_time" = "V1",
  #             "location2" = "V2",
  #             "locationnum2" = "V3",
  #             "loc3_time" = "V4",
  #             "location3" = "V5",
  #             "locationnum3" = "V6")
    names(x) <- c("filename", "loc1_time", "location1", "locationnum1", 
                  "loc2_time", "location2", "locationnum2",
                  "loc3_time", "location3", "locationnum3")
  # names(x) = c("filename", "loc2_time",
  #                     "location2",
  #                     "locationnum2",
  #                     "loc3_time",
  #                     "location3",
  #                     "locationnum3")
  } 
  else if(ncol(x) == 7){ 
    # names(x) = c("filename", "loc2_time",
    #              "location2",
    #              "locationnum2")
    # x <- x %>%  rename("loc2_time" = "V1",
    #             "location2" = "V2",
    #             "locationnum2" = "V3")
    names(x) <- c("filename", "loc1_time", "location1", "locationnum1", 
                  "loc2_time", "location2", "locationnum2")
  }
  return(x)  
})

runway_loc2_3_df <- rbindlist(runway_loc2_3_df, fill = T) %>% 
  mutate_at(c("loc1_time", "loc2_time", "loc3_time"),  as.numeric)

naniar::vis_miss(runway_loc2_3_df)

# make sure that loc2 and loc3 are expected values; and then use 3 to sub in for 2 
# loc2non2 <- subset(runway_loc2_df, is.na(locationnum)) # Noted to Maya and Alen # originally != 2 but changed to is.na because of summary(runway_loc2_df$locationnum) 
# run this code to view cases for which the data was imputed by the photobeam 3
runway_loc2_3_df %>% dplyr::filter(locationnum2 != 2|locationnum3 != 3) # 20 cases; all loc 3 are being moved to loc 2 automatically 

runway_loc2_3_df %>% dplyr::filter(is.na(loc2_time)) %>% naniar::vis_miss() # if missing loc2, doesn't have loc3

# bc of runway_loc2_3_df %>% dplyr::filter(!is.na(V1))
runway_loc2_3_df %<>% 
  mutate(loc1_time = ifelse(is.na(loc1_time) == T & V3 == 1, V1, loc1_time),
         locationnum1 = ifelse(is.na(locationnum1) == T & V3 == 1, V3, locationnum1),
         loc2_time = ifelse(is.na(loc2_time) == T & V3 == 2, V1, loc2_time),
         locationnum2 = ifelse(is.na(locationnum2) == T & V3 == 2, V3, locationnum2),
         loc3_time = ifelse(is.na(loc3_time) == T & V3 == 3, V1, loc3_time),
         locationnum3 = ifelse(is.na(locationnum3) == T & V3 == 3, V3, locationnum3))

rawfiles_calc <- left_join(runway_reach_df, runway_loc2_3_df, by = "filename") %>% 
  #filter(locationnum == 2) %>% 
  mutate(elapsedtime = trunc(reachtime) - trunc(loc2_time)) %>%  # turn to transmute once all edges are smooth 
  arrange(filename) %>% 
  extractfromfilename %>%  # sort by ascending filename and get animal id and exp date/time 
  left_join(., Jhou_SummaryAll[,c("labanimalid", "rfid", "shipmentcohort", "dob")], by = "labanimalid") %>% # add rfid column, extracted from the Jhou_SummaryAll **note the swap situation has not been fixed 
  mutate(experimentage = as.numeric(date - dob)) %>%
  select(rfid, labanimalid, shipmentcohort, date, time, experimentage, loc1_time, elapsedtime, filename) %>% 
  dplyr::filter(!rfid %in% Jhou_SummaryAll[,c("rfid", "resolution")][which(Jhou_SummaryAll$resolution %in% c("EXCLUDE_ALL_BEHAVIORS","EXCLUDE_RUNWAY")),]$rfid)
# the rfid's being removed are all complete cases though

naniar::vis_miss(rawfiles_calc) 

subset(rawfiles_calc, is.na(elapsedtime)) 
subset(rawfiles_calc, experimentage < 0) 
rawfiles_calc_upload <- subset(rawfiles_calc, !is.na(elapsedtime)&elapsedtime > 0&!is.na(experimentage)&experimentage > 0 ) # we only want nonzero positive elapsedtimes and experimentages; will allow na's in location1

# Jhou_SummaryAll %>% dplyr::filter(labanimalid == "U429") # bday seems correct 

## RUNWAY REVERSAL
# redo reversals since it is only the number of lines of reversals between start and reaching goalbox 

read_runwayrevs <- function(x){
  reversals <- fread(paste0("sed -n '/OPENING/,/REACHED/p' ", "'", x, "'", " | grep -c \"REVERSAL\""))
  reversals$filename <- x 
  return(reversals)
  }

runway_reversals <- lapply(runwayfiles_clean, read_runwayrevs) %>% rbindlist(fill = T)
names(runway_reversals) <- c("reversals", "filename")
naniar::vis_miss(runway_reversals) # complete cases

# bind reversals with runway data ; leave rawfiles_calc with the questions on the dropbox 1/3 XX haven't asked Maya 
# runway <- left_join(rawfiles_calc, runway_reversals, by = "filename") %>%  # clean out upstream find -name regular expression to exclude files that don't contain the exp name; see subset(., is.na(rfid))
#   select(rfid, labanimalid, shipmentcohort, date, time, experimentage, elapsedtime, reversals, filename) 
  

# bind reversals with runway data
runway <- left_join(rawfiles_calc_upload, runway_reversals, by = "filename") %>%  # clean out upstream find -name regular expression to exclude files that don't contain the exp name; see subset(., is.na(rfid))
  select(rfid, labanimalid, shipmentcohort, date, time, experimentage, loc1_time, elapsedtime, reversals, filename)
naniar::vis_miss(runway)

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


# runway habituation 
# from the runway protocols file, a disqualifying runway habituation file is one for which "the animals take more than a minute"..
# we repeat the habituation sessions until the animal takes less than a minute 


################################
### RAW TEXT  Locomotor ########
################################
### EXP 2: locomotor 

# Starting with U311, there 

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
# locomotorfiles_clean <- locomotorfiles[str_detect(locomotorfiles, "/{U}\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_LOCOMOTOR_BASIC.txt", negate = F)] # turning off and allowing for code below until XX resolved u subdirectories
locomotorfiles_clean <- locomotorfiles[str_detect(locomotorfiles, "/[Uu]\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_LOCOMOTOR_BASIC(_| corrected)?.txt", negate = F)] # 


rawfiles_locomotor <- lapply(locomotorfiles_clean, read_locomotor) %>% 
  rbindlist(fill = T) %>%
  select(V1, filename) %>%
  rename("bincounts" = "V1") %>%
  mutate(bincounts = as.numeric(bincounts)) %>%  # using sed rather than awk results in less NA (only 26 here); XX 10/29 ADDED TO DOCUMENT FILE NA IS COMING FROM FILES NOT HAVING END SESSION
  dplyr::filter(!is.na(bincounts))
# post call (11/7) changes: minute31 in U112 into minute30 (do later); and remove files that don't have all thirty values (more robust)
# 12/13 changes: do minute31 to minute30 change above and remove files that don't have all thirty values (more robust)

# rawfiles_locomotor %>% dplyr::filter(!is.na(bincounts)) %>% group_by(filename) %>% add_count %>% ungroup() %>% select(n) %>% table()
# rawfiles_locomotor %>% dplyr::filter(!is.na(bincounts)) %>% group_by(filename) %>% add_count %>% ungroup() %>% dplyr::filter(n == 31)

# rawfiles_locomotor %<>% group_by(filename) %<>% add_count %<>% dplyr::filter(n>=30) %<>% select(-n) %<>% ungroup()

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

rawfiles_locomotor_wide <- spread(rawfiles_locomotor, minute, bincounts) %>% # spread from long to wide
  as.data.table()
# get code from https://stackoverflow.com/questions/57037860/how-to-reorder-column-names-in-r-numerically
# reorder the columns
setcolorder(rawfiles_locomotor_wide, c(1, order(as.numeric(gsub("minute", "", names(rawfiles_locomotor_wide)[-1]))) + 1))

## to do: 
# starting U294, add the first 15 minutes average and last 15 minutes average
# for U311, U312 and starting U315 to U328, take the same two averages for the second session
# investigate the number of na's and compare that to how they are coding na's (as blanks)

# see the incomplete cases
res <- as.data.frame(rawfiles_locomotor_wide)[!complete.cases(as.data.frame(rawfiles_locomotor_wide)),]
res[-1] <- as.numeric(is.na(res[-1]))
res


# add means and sums as jhou's lab does
rawfiles_locomotor_wide[, `:=`(bintotal = rowSums(.SD, na.rm=T),
                               binmeans = rowMeans(.SD, na.rm=T)), .SDcols=names(rawfiles_locomotor_wide)[-1]]

# only one case for which the minute 31 appears, ./U112/2019-0121-0939_112_LOCOMOTOR_BASIC.txt
locomotor_raw <- extractfromfilename(rawfiles_locomotor_wide) %>%
  #mutate(labanimalid = toupper(rawfiles_locomotor_wide$filename) %>% str_extract('(U[[:digit:]]+)')) %>%   
  WFUjoin.raw() %>% #append to wfu info
  select(labanimalid, rfid, shipmentcohort, date, time, everything()) %>% # code changes data.table back to data.frame
  select(-filename, filename) %>% 
  dplyr::filter(bintotal != 0) %>% 
  dplyr::filter(rowSums(is.na(.[grep("minute", names(.))])) < 2) %>% 
  dplyr::filter(!rfid %in% Jhou_SummaryAll[,c("rfid", "resolution")][grepl("EXCLUDE_LOCOMOTOR2", Jhou_SummaryAll$resolution),]$rfid)

locomotor_raw %>% add_count(labanimalid) %>% select(labanimalid, n) %>% ggplot() + geom_histogram(aes(n))

## to assign session


# XX review and remove after the call
# # remove the rows with almost all na's # two files ./U175/2019-0209-1959_175_LOCOMOTOR_BASIC.txt(all); ./U412/2019-0918-1102_412_LOCOMOTOR_BASIC.txt (almost all)
# # remove the rows with all 0's
# # remove the rows that they have noted to remove EXCLUDE_LOCOMOTOR
# # remove duplicated files
# # add the experiment age
# rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>%
#   dplyr::filter(bintotal != 0) %>%
#   dplyr::filter(!grepl("EXCLUDE_LOCOMOTOR", resolution)) %>%
#   dplyr::mutate(experimentage = as.numeric(difftime(date, dob, units = "days")))
# 
# rawfiles_locomotor_wide <- rawfiles_locomotor_wide[!duplicated(rawfiles_locomotor_wide[-1]),]
# 
# # check the number of files is even before adding session info
# rawfiles_locomotor_wide %>% add_count(labanimalid) %>% dplyr::filter(n != 1, n!=2, n!=4) %>% View()
# ## look into these three files cases???? 11/7
# 
# 
# remove the first file for the 5 file cases (11/1 remove two such animals)
# rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>%
#   group_by(labanimalid) %>%
#   do(tail(., 4))



######################################################### WAIT UNTIL RESPONSE
# TEMPORARILY REMOVE THE TWO CASES: 371 AND 271
rawfiles_locomotor_wide <- rawfiles_locomotor_wide %>% 
  dplyr::filter(!labanimalid %in% c("U371", "U271"))
##########################################################

# with no more 3 or 5 file cases...
# add count and assigning the session based on the number of counts 
# rawfiles_locomotor_wide$session <- NA 
bincounts <- c("Binned Counts","Binned Counts1",  "Binned Counts2","Binned Counts1a","Binned Counts1b","Binned Counts2a","Binned Counts2b") %>% 
  data.frame() %>% 
  rename("session" = ".") %>%
  mutate(session = as.character(session))

# add count and assigning the session based on the number of counts
# extract from between _id_ rather than from Uid # exclude those that are resolved to be exclude
rawfiles_locomotor_split <- split(locomotor_raw, locomotor_raw$labanimalid)
Jhou_Raw_Locomotor <- lapply(rawfiles_locomotor_split, function(x){
  x <- x %>% 
    arrange(date, time)
  if(nrow(x) == 2){
    cbind(x, tail(head(bincounts,3),2))
    }
  else if(nrow(x) == 4){
    cbind(x, tail(bincounts,4))
  }
  else if(nrow(x) == 1){
    cbind(x,head(bincounts,1))
  }
}) %>% 
  rbindlist()  %>% #append to wfu info
  select(labanimalid, rfid, shipmentcohort, date, time, session, everything()) %>% 
  subset(experimentage > 0)

# investigate minute 30 na comparison to the excel
# which ones are na in raw but are not na in excel
Jhou_Raw_Locomotor %>% subset(minute30 %>% is.na()) %>% dplyr::filter(!labanimalid %in% subset(Jhou_Locomotor, is.na(minute30))$labanimalid)
# which ones are na in excel but not in raw

# fixes the one na in raw but not na in excel
# Jhou_Raw_Locomotor[which(Jhou_Raw_Locomotor$filename == "./U54/2018-1022-1248_54_LOCOMOTOR_BASIC.txt"),]$minute30 <- 4 # this file doens't have "END SESSION" # not needed anymore because id only needed to be reformatted 

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
progpunfiles_clean <- progpunfiles[str_detect(progpunfiles, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_FOOD[[:space:]]?CONFLICT(_corrected)?.txt", negate = F)] 

create_progpuntable <- function(x){
  thistrialrownumandshock = fread(paste0("awk '/THIS TRIAL/{print $1 \" \" $2 \",\" $13 \",\" NR}' ","'",x,"'"), header=F, fill=T, showProgress = F, verbose = F)  
  thistrialrownumandshock$filename<-x
  return(thistrialrownumandshock)
}

progpunishment_df = lapply(progpunfiles_clean, create_progpuntable) %>%
  rbindlist(fill = T) #100% present
colnames(progpunishment_df) = c("trialnum", "shockma", "rownum", "filename")

progpunishment_df_complete <- progpunishment_df %>% 
  dplyr::filter(complete.cases(.)) %>%  
  group_by(filename) %>% 
  do(tail(., 2)) #limit the calculations of the number of LEFTPRESSES to two per filename # XX SCREENSHOT ADDED TO DOCUMENT ONLY ON CASES THAT HAVE THE VALUES WE NEED; 6553 cases

progpunishment_df_complete %>% count(filename) %>% subset(n!=2) # XX made note to Alen 10/18, follow up again; 12/13 only one case XX ask about during call
# oneobservation_details <- progpunishment_df %>% 
#   group_by(filename) %>%
#   mutate(count = n()) %>% 
#   ungroup() %>% 
#   filter(count != 2); head(oneobservation_details) # 96 observations

# data_df_valid <- progpunishment_df_complete %>% 
#   group_by(filename) %>%
#   mutate(count = n()) %>% 
#   ungroup() %>% 
#   dplyr::filter(count == 2) # use the valid files before they respond about the other ones 6170

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
progpunishment_df_complete <- progpunishment_df_complete %>% add_count(filename) %>% subset(n==2) %>% select(-n)
progpundata_categories = create_progpuntable_tocategorize(progpunishment_df_complete) # test on valid datapoints until Jhou team returns comment #100% present

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
progpun_presses_df = lapply(progpundata_categories_wcat$filename, progpun_presses) %>% 
  rbindlist(fill = T) %>% # 3168 cases, 47 na and left presses min 55 max 434
  merge(progpundata_categories_wcat, .) %>% 
  rename("activepresses" = "V1", 
         "inactivepresses" = "V2")


progpun_boxes <- function(x){
  boxandstations <- fread(paste0("awk '/Started script/{print $(NF-1) \" \" $NF}' ", "'", x, "'"), header=F, fill=T, showProgress = F, verbose = F) %>% as.data.frame()
  boxandstations$filename <- x
  return(boxandstations)
}

progpun_boxesandstations_df = lapply(files_clean, progpun_boxes) %>% 
  rbindlist(fill = T) # 3168 cases, no na, and left presses min 1 max 8
colnames(progpun_boxesandstations_df) = c("boxorstation", "boxorstationumber", "filename")


readbox <- function(x){
  boxes <- fread(paste0("grep -oEm1 \"(box|station) [0-9]+\" ", "'", x, "'"))
  return(boxes)
}
progpun_raw <- sapply(progpun_presses_df$filename, readbox) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "filename") %>% 
  mutate(box = paste(V1, as.character(V2))) %>% 
  select(-c(V1, V2)) %>% 
  merge(progpun_presses_df,.) %>% 
  extractfromfilename() %>% 
  WFUjoin.raw() %>% 
  mutate(shockoflastcompletedblock = ifelse(lastshock_cat == "Complete", lastshock, secondtolastshock),
         shockoflastattemptedblock = lastshock) %>% 
  rename("numtrialsatlastshock" = "numleftpresseslast") %>% 
  select(-c(numleftpressesbwlasttwo, lastshock_cat, secondtolastshock_cat, secondtolastshock, lastshock)) %>% 
  group_by(labanimalid) %>% 
  mutate(session = as.character(dplyr::row_number() - 1)) %>%
  select(shipmentcohort, labanimalid, rfid, date, time, session, box, everything()) %>% 
  select(-filename, filename)

# XXX concern (some animals have more than one box designation): 
# boxesandstations_df$labanimalid <- stringr::str_extract(boxesandstations_df$filename, "U[[:digit:]]+[[:alpha:]]*")
# morethanone <- boxesandstations_df %>%
#   select(labanimalid, boxorstationumber) %>% 
#   unique() 
# morethanone2 <- morethanone %>% count(labanimalid) %>% filter(n != 1)
# cases <- subset(rawfiles_box, rawfiles_box$labanimalid %in% morethanone2$labanimalid)
# cases # just 6 and 8 now XX already made note to Tom Jhou's team
# 
# progressivepunishment <- left_join(x = progpundata_categories_wcat, y = progpun_presses_df, by = "filename") %>% # XX TOOK SCREENSHOT OF NA DELAY'S AND SENT THEM TO TOM'S TEAM
#   left_join(., progpun_boxesandstations_df, by = "filename") %>% # dim is all over the place (using the most limiting 3085, resulting has no na)
#   extractfromfilename() %>% # extract file information for preparation for appending to rfid
#   # mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid) ) %>% 
#   mutate(shockoflastcompletedblock = ifelse(lastshock_cat == "Complete", lastshock, secondtolastshock),
#          shockoflastattemptedblock = lastshock) %>% 
#   rename("numtrialsatlastshock" = "numleftpresseslast") %>% 
#   select(-c(numleftpressesbwlasttwo, lastshock_cat, secondtolastshock_cat, secondtolastshock, lastshock)) %>% 
#   group_by(labanimalid) %>% 
#   mutate(session = as.character(dplyr::row_number() - 1)) %>%  # add session 
#   ungroup() %>%
#   left_join(., rfidandid, by = "labanimalid") 

################################
### RAW TEXT  Prog ratio #######
################################

### EXP 4: Progressive ratio
# max ratio
setwd("~/Dropbox (Palmer Lab)/U01 folder/Progressive ratio")
progratiofiles <- list.files(path=".", pattern=".*RATIO.*.txt", full.names=TRUE, recursive=TRUE) 
progratiofiles_clean <- progratiofiles[str_detect(progratiofiles, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_PROGRESSIVE RATIO(_|_corrected)?.txt", negate = F)]

# progratiofiles[str_detect(progratiofiles, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_PROGRESSIVE RATIO(_corrected)?.txt", negate = T)] gives the subset of filenames that don't follow the format

readmaxratio <- function(x){
  maxratio <- fread(paste0("grep -B1 \"TIMEOUT\\\\s\\\\s\" " , "'", x, "'", " | awk '{print $4; exit}'"))
  maxratio$filename <- x
  return(maxratio)
} # get the max ratio from the line before TIMEOUT
progratio_maxratio <- lapply(progratiofiles_clean, readmaxratio) 
progratio_maxratio_rm <- progratio_maxratio[sapply(progratio_maxratio, function(x) ncol(x)) > 1] # remove the null datatables
progratio_maxratio_df <- rbindlist(progratio_maxratio_rm, fill = T) %>%
  rename("maxratio" = "V1") %>% 
  select(-NUMBER) %>% # 100% empty column 
  mutate(maxratio = as.numeric(maxratio)) 

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
progratio_presses <- lapply(progratio_maxratio_df$filename, readpresses) 
progratio_presses_rm <- progratio_presses[sapply(progratio_presses, function(x) ncol(x)) > 1] # remove the null datatables
progratio_presses_df <- rbindlist(progratio_presses_rm, fill = T) %>%
  rename("activepresses" = "V1",
         "inactivepresses" = "V2") # again there are 30 NA's  

# join to create final raw df
progratio <- left_join(progratio_maxratio_df, progratio_presses_df, by = "filename") # 10/28 bring to Alen's attention -- these cases for which there are only timeout lines and no pre-timeout value so no maxratio value 
progratio_raw <- progratio %>% 
 #  dplyr::filter(!grepl("error", filename), !is.na(activepresses)) %>%
  extractfromfilename() %>%
  group_by(labanimalid) %>% 
  mutate(session = as.character(dplyr::row_number())) %>% 
  ungroup() %>% 
  WFUjoin.raw() %>% 
  select(shipmentcohort, labanimalid, rfid, date, time, session, maxratio, everything())  %>% 
  select(-filename, filename) %>%
  arrange(labanimalid, session)  # 100% present data

aggregate(session ~ labanimalid, data = progratio_raw, max) %>% mutate(session = as.numeric(session)) %>% select(session) %>% table()
progratio_subjects_tempremove <- aggregate(session ~ labanimalid, data = progratio_raw, max) %>% mutate(session = as.numeric(session)) %>% subset(session != 4) %>% select(labanimalid) %>% unlist() %>% as.character()

progratio_raw_upload <- progratio_raw %>% subset(!labanimalid %in% progratio_subjects_tempremove)

  # mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)) %>%
  # arrange(labanimalid)
# reorder based on labanimalid  
#  progratio %>% group_by(labanimalid) %>% add_count(n = n()) %>% dplyr::filter(max(as.numeric(session)) != n) %>% select(labanimalid) %>% unique()

################################
### RAW TEXT  Delayed pun ######
################################

# EXP 5: Delayed punishment
# setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/Delayed punishment/")
# find -type f -iname "*PUNISHMENT*.txt" ! -path "*error*" -exec awk '/ENDING/{print FILENAME}' {} \; 
setwd("~/Dropbox (Palmer Lab)/U01 folder/Delayed punishment") # while dropbox is updating the clone

delayed_punishmentfiles <- list.files(path=".", pattern=".*DELAYED.*.txt", full.names=TRUE, recursive=TRUE) # 5781 files exclude existing txt files and include any corrective "qualifiers" # 5670 counts
delayed_punishmentfiles_clean <- delayed_punishmentfiles[str_detect(delayed_punishmentfiles, "/U\\d+/\\d{4}-\\d{4}-\\d{4}_\\d+_DELAYED PUNISHMENT(_corrected)?.txt", negate = F)] # 5873 files
# delayed_punishmentfiles_clean <-  delayed_punishmentfiles[ ! grepl("error", delayed_punishmentfiles, ignore.case = TRUE) ]  # exclude files that have errors (labelled by Jhou's team) 

create_delayedpuntable <- function(x){
  thistrialrownumandshock = fread(paste0("awk '/THIS TRIAL/{print $1 \" \" $2 \",\" $13 \",\" NR}' ","'",x,"'"), header=F, fill=T, showProgress = F, verbose = F)  
  thistrialrownumandshock$filename<-x
  return(thistrialrownumandshock)
}

delayedpunishment_df = lapply(delayed_punishmentfiles_clean, create_delayedpuntable) %>%
  rbindlist(fill = T) # 52389 THIS TRIAL LINES from 5873 unique files
colnames(delayedpunishment_df) = c("trialnum", "shockma", "rownum", "filename") 
# summary(delayedpunishment_df) # 73 NA's
# naniar::vis_miss(delayedpunishment_df)

delayedpunishment_df_complete <- delayedpunishment_df %>% 
  dplyr::filter(complete.cases(.)) %>%  # since the 73 na cases seem to be from the same file, same trial, all blocked off 
  group_by(filename) %>% 
  do(tail(., 2)) #limit the calculations of the number of LEFTPRESSES to the last two per filename # 11127 THIS TRIAL LINES (TTL) from 5,575 unique files (should be 11150, or 5575*2, so we are missing cases )

delayedpunishment_df_complete %>% count(filename) %>% subset(n!=2) ## added to notes for jhou team 

delayed_data_df_valid <- delayedpunishment_df_complete %>% 
  group_by(filename) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  dplyr::filter(count == 2) # temporarily use the valid files before they respond 11104 TTL from 5,575 unique files from 5,552 unique files (removed the 23 cases)

# create categorization table 
create_delayedpuntable_tocategorize <- function(x){
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
  return(numofsessions_df)
}

# data2_valid_subset <- data2_valid[1:100,]
# data_categories_test = create_progpuntable_tocategorize(data2_valid_subset) # test subset 

delayedpundata_categories = create_delayedpuntable_tocategorize(delayed_data_df_valid) # test on valid datapoints until Jhou team returns comment 
# naniar::vis_miss(delayedpundata_categories) # 100% present
delayedpundata_categories_wcat <- delayedpundata_categories %>% 
  mutate(secondtolastshock_cat = ifelse(numleftpressesbwlasttwo > 3, "Complete", "Attempt"),
         lastshock_cat = ifelse(numleftpresseslast >= 3, "Complete", "Attempt"))

delayedpun_presses <- function(x){
  presses <- fread(paste0("tac ", "'", x, "'", " | awk '/LEFTPRESSES/ {print $4 \",\" $6; exit}'"), header=F, fill=T, showProgress = F, verbose = F)
  presses$V1[nrow(presses) == 0] <- NA
  presses$V2[nrow(presses) == 0] <- NA
  presses$filename <- x
  return(presses)
}
delayedpun_presses_df = lapply(unique(delayed_data_df_valid$filename), delayedpun_presses) %>% 
  rbindlist(fill = T) # 5552 files, 0 na and left presses min 30 max 602
colnames(delayedpun_presses_df) = c("activepresses", "inactivepresses", "filename")

delayedpun_delays <- function(x){
  delays <- fread(paste0("grep -m1 -o -E '[0-9]+[[:space:]]?SEC' ", "'", x, "'"), header=F, fill=T, showProgress = F, verbose = F)
  delays$filename <- x 
  return(delays)
}
delayedpun_delays_df <- lapply(unique(delayed_data_df_valid$filename), delayedpun_delays) %>% 
  rbindlist(fill = T)
delayedpun_delays_df %<>% 
  select(-V2) %<>%
  rename("delay" = "V1") %<>% 
  mutate(delay = gsub("SEC", "", delay))

readbox <- function(x){
  boxes <- fread(paste0("grep -oEi \"(box|station) [0-9]+\" ", "'", x, "'"))
  return(boxes)
}
# delayedpun_boxes_df <- sapply(delayedpun_delays_df$filename, readbox) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   tibble::rownames_to_column(var = "filename") %>% 
#   mutate(box = paste(V1, as.character(V2))) %>% 
#   select(-c(V1, V2)) # 5552 files, no na, and boxes min 1 max 8

delayedpun_boxes <- lapply(delayedpun_delays_df$filename, readbox) 
names(delayedpun_boxes) <- delayedpun_delays_df$filename
delayedpun_boxes_df <- delayedpun_boxes %>%  
  rbindlist(idcol = "filename") %>% 
  group_by(filename) %>% 
  mutate(position = paste0("find_", 1:n())) %>% 
  ungroup() %>% 
  mutate(boxstation = paste(V1, V2)) %>% 
  select(-c(V1, V2)) %>% 
  spread(position, boxstation) 

# within the same file 
delayedpun_boxes_df %>% mutate(find_1_digit = readr::parse_number(find_1), find_2_digit = readr::parse_number(find_2)) %>% dplyr::filter(find_1_digit != find_2_digit) # looks good
delayedpun_runindiffboxes <- delayedpun_boxes_df %>% mutate(find_1_digit = readr::parse_number(find_1), 
                               find_2_digit = readr::parse_number(find_2), 
                               subdir_id = str_extract(delayedpun_boxes_df$filename, regex("U\\d+[A-z]?", ignore_case = T))) %>% 
  group_by(subdir_id) %>% 
  mutate(uniquebox = n_distinct(find_1)) %>% 
  ungroup() %>% 
  group_by(subdir_id, find_1) %>% 
  mutate(boxcount = n()) %>% 
  ungroup() %>% 
  dplyr::filter(uniquebox != 1)

# %>% 
#   merge(test_df,.) %>% 
#   extractfromfilename() %>% 
#   WFUjoin.raw() %>% 
#   select(shipmentcohort, labanimalid, rfid, date, time, completedtrials, totaltrials, box, filename)


# 1/3 turned into comment to avoid the labanimalid
# XXX concern (some animals have more than one box designation):
# delayedpun_boxes_df$labanimalid <- stringr::str_extract(delayedpun_boxes_df$filename, regex("U[[:digit:]]+[[:alpha:]]*", ignore_case=T))
# delayedpun_boxes_df %>% select(-filename) %>% distinct() %>% add_count(labanimalid) %>% dplyr::filter(n!=1) %>% as.data.frame()
  
# morethanone <- delayedpun_boxesandstations_df %>%
#   select(labanimalid, boxorstationumber) %>%
#   unique() %>% count(labanimalid) %>% filter(n != 1)
# cases <- subset(rawfiles_box, rawfiles_box$labanimalid %in% morethanone2$labanimalid)
# cases # just 6 and 8 now XX already made note to Tom Jhou's team

delayedpunishment <- delayedpun_delays_df %>% 
  dplyr::filter(!is.na(delay)) %>%    
  left_join(., delayedpun_boxes_df[,c("filename", "find_1")], by = "filename") %>% # dim is all over the place (using the most limiting 3085, resulting has no na)
  left_join(., delayedpundata_categories_wcat, by = "filename") %>% # XX TOOK SCREENSHOT OF NA DELAY'S AND SENT THEM TO TOM'S TEAM
  left_join(., delayedpun_presses_df, by = "filename") %>% 
  extractfromfilename() %>% # extract file information for preparation for appending to rfid
  # mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid) ) %>% 
  mutate(shockoflastcompletedblock = ifelse(lastshock_cat == "Complete", lastshock, secondtolastshock),
         shockoflastattemptedblock = lastshock) %>% 
  rename("box" = "find_1",
    "numtrialsatlastshock" = "numleftpresseslast") %>% 
  select(-c(numleftpressesbwlasttwo, lastshock_cat, secondtolastshock_cat, secondtolastshock, lastshock)) %>% 
  group_by(labanimalid) %>% 
  mutate(session = as.character(dplyr::row_number())) %>%  
  ungroup() %>%
  WFUjoin.raw() %>%
  select(shipmentcohort, labanimalid, rfid, date, time, session, delay, everything())  %>% 
  select(-filename, filename)
# dplyr::filter(resolution != "EXCLUDE_ALL_BEHAVIORS"|is.na(resolution)) # remove two animals (U84 and U85 bc exclude all behaviors)

# consistent within session
# delayedpun_boxes_df %>% mutate(number1 = str_sub(find_1, -1), number2 = str_sub(find_2, -1) ) %>% dplyr::filter(number1 != number2) # so that's why we can use find_1 as proxy since the only difference is in station vs box 

delayedpunishment %>% summary 
delayedpunishment %>% naniar::vis_miss() #100% now 
# to do: check the validity of the columns and the cell formatting 
## and use resolutions column to filter out data

# subset(delayedpunishment, is.na(numtrialsatlastshock)) FIND OUT WHY THIS CODE ISN'T WORKING FOR U481 FOR EXAMPLE


################################
### RAW TEXT  Lever training ###
################################
# extract all file names, split into the cohorts mentioned in the protocols sheet, and write separate functions for them
setwd("~/Dropbox (Palmer Lab)/U01 folder/Lever training")
lever_trainingfiles <- list.files(path=".", pattern=".*LEVER.*.txt", full.names=TRUE, recursive=TRUE) # 5736 files exclude existing txt files and include any corrective "qualifiers" # 5670 counts
lever_trainingfiles_clean <- lever_trainingfiles[str_detect(lever_trainingfiles, "/U\\d+(\\D+)?/\\d{4}-\\d{4}-\\d{4}_\\d+_LEVER TRAINING(_corrected)?.txt", negate = F)] # 5721 files # including the files that contain PP ??? XX 

readlevertraining <- function(x){
  levertraining <- fread(paste0("grep -o '[0-9]* OUT OF [0-9]*' ", "'", x, "'"), fill = T)
  levertraining$filename <- x
  return(levertraining)
}

# test <- lapply(lever_trainingfiles_clean[1:100], readlevertraining) 
# test_rm <- test[sapply(test, function(x) ncol(x)) > 1] # remove the null datatables
# test_df <- test_rm %>% 
#   rbindlist(fill = T) %>% 
#   select(-c(V2, V3)) %>% 
#   rename("completedtrials" = "V1",
#          "totaltrials" = "V4")

lt <- lapply(lever_trainingfiles_clean, readlevertraining) 
lt_rm <- lt[sapply(lt, function(x) ncol(x)) > 1] # remove the null datatables
lt_df <- lt_rm %>% 
  rbindlist(fill = T) %>% 
  select(-c(V2, V3)) %>% 
  rename("completedtrials" = "V1",
         "totaltrials" = "V4")

readbox <- function(x){
  boxes <- fread(paste0("grep -oEm1 \"(box|station) [0-9]+\" ", "'", x, "'"))
  return(boxes)
}
levertraining_raw <- sapply(lt_df$filename, readbox) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "filename") %>% 
  mutate(box = paste(V1, as.character(V2))) %>% 
  select(-c(V1, V2)) %>% 
  merge(lt_df,.) %>% 
  extractfromfilename() %>% 
  WFUjoin.raw() %>% 
  select(shipmentcohort, labanimalid, rfid, date, time, completedtrials, totaltrials, box, experimentage, filename)


levertraining_raw %>% naniar::vis_miss()
levertraining_raw %>% summary()

subset(levertraining_raw, experimentage < 0) 
levertraining_raw_upload <- subset(levertraining_raw, experimentage > 0) 

##################################
# Create list of all experiments
##################################
# Jhou_raw <- list(
#   "runway" = runway,
#   "progratio" = progratio,
#   "delayedpunishment" = delaypunishment,
# )

## Appending info from WFU data 
# Add rfid, sex, cohort
WFUjoin.raw <- function(rawdf){
  joindf <- merge(Jhou_SummaryAll[,c("labanimalid", "rfid", "shipmentcohort", "dob")], rawdf, by = "labanimalid") %>% 
    mutate(experimentage = as.numeric(date - dob)) %>%
    select(-dob)
  return(joindf)
} 


#####################################################################################################################################################################3
## CREATE BY BATCHES

### 

assign()



#################################################################################################
## CREATE FIRST SQL FILE FOR RUNWAY DATA
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='password', dbname='U01')
dbListTables(con)

#insert data into mytable from data frame
# dbWriteTable(con, "mytable", df, append = TRUE, row.names = FALSE)
dbWriteTable(con, c("u01_tom_jhou", "jhou_runway"), value = runway)
dbExistsTable(con, c("u01_tom_jhou", "jhou_runway"))

# pretenddata <- runway[0,] # create empty df with the same columns
pretenddata <- runway %>% tail(1)
dbWriteTable(myCon, "myTable", myTable, append = TRUE)


# runway[ runway == "NA" ] <- NA
# is.na(runway) <- runway == "NA


dbWriteTable(con, c("public", "iris"), value = head(iris,10), overwrite = T)
dbWriteTable(con, c("public", "iris"), value = head(iris,20), append = T)
# dbWriteTable(con, c("public", "iris"), value = tail(head(iris, 20), 10), append = T, row.names = F)
dbWriteTable(con, c("public", "iris"), value = pretend, overwrite = T, row.names = F)



## SEND DATA TO POSTGRESQL DATABASE 
dbWriteTable(con, c("u01_tom_jhou", "jhou_levertraining"), value = levertraining_raw_upload, row.names = F)
dbExistsTable(con, c("u01_tom_jhou", "jhou_levertraining"))


dbWriteTable(con, c("u01_tom_jhou", "jhou_delayedpunishment"), value = delayedpunishment, row.names = F)
dbExistsTable(con, c("u01_tom_jhou", "jhou_delayedpunishment")) #5935


dbWriteTable(con, c("u01_tom_jhou", "jhou_progressivepunishment"), value = progpun_raw, row.names = F)
dbExistsTable(con, c("u01_tom_jhou", "jhou_progressivepunishment")) #3276

dbWriteTable(con, c("u01_tom_jhou", "jhou_locomotor"), value = Jhou_Raw_Locomotor, row.names = F)
dbExistsTable(con, c("u01_tom_jhou", "jhou_locomotor")) #895

dbWriteTable(con, c("u01_tom_jhou", "jhou_progressiveratio"), value = progratio_raw_upload, row.names = F)
dbExistsTable(con, c("u01_tom_jhou", "jhou_progressiveratio")) #1848

