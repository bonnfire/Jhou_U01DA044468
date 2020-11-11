setwd("~/Dropbox (Palmer Lab)/U01 folder")

# Load libraries
library(tidyverse) #loads dplyr, tidyr, ggplot2, purrr, etc
require(mgsub)
library(readxl)
library(lubridate)
library(openxlsx)
library(stringr)
library(data.table)
library(tidyxl)

u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 

# Jhou_Excel <- u01.importxlsx("U01 Master sheet_readonly.xlsx") # 1/2 this file has disappeared
# Jhou_Excel_updated <- u01.importxlsx("Copy of U01 Master sheet_NEW11_18.xlsx") # since 12/11 this file has disappeared 1/2 reappeared 
# Jhou_Excel <- u01.importxlsx("Copy of U01 Master sheet_NEW11_18.xlsx")
# Jhou_Excel <- u01.importxlsx("U01 Master sheet (Jhou Lab's conflicted copy 2020-02-17).xlsm") # updated 2/18??? 
Jhou_Excel <- u01.importxlsx("U01 Master sheet.xlsm") # 08/27/2020

################################
########### Summary All ########
################################
Jhou_SummaryAll <- Jhou_Excel[["Summary all"]] 
Jhou_SummaryAll <- Jhou_SummaryAll %>% select_if(~sum(!is.na(.)) > 0) # select columns that aren't all na
# Jhou_SummaryAll <- Jhou_SummaryAll[, c(1:13, 52,53)]
names(Jhou_SummaryAll) <-  Jhou_SummaryAll[1,] %>% as.character()
Jhou_SummaryAll <- Jhou_SummaryAll[-1, ] 
names(Jhou_SummaryAll) <- mgsub::mgsub(names(Jhou_SummaryAll),
                                 c(" |\\.|:", "#", "16 digit ID", "Date of Wean|Wean Date","Jhou lab", "Date of Ship", "Dams"),
                                 c("", "Number", "RFID", "DOW","LabAnimal", "ShipmentDate", "Dames")) 
Jhou_SummaryAll <- Jhou_SummaryAll %>% 
  janitor::clean_names()
# # clean up variables
Jhou_SummaryAll$coatcolor <- mgsub::mgsub(Jhou_SummaryAll$coatcolor, 
                                  c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood", "[A|a]lbino"), 
                                  c("BROWN", "BLACK", "HOOD", "ALBINO"))
Jhou_SummaryAll$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", Jhou_SummaryAll$coatcolor)
Jhou_SummaryAll$coatcolor <- toupper(Jhou_SummaryAll$coatcolor)

datecols <- c("dob", "dow", "shipment_date")
Jhou_SummaryAll <- Jhou_SummaryAll %>% 
  mutate_at(.vars = vars(datecols), .funs = openxlsx::convertToDate)
  
# Jhou_SummaryAll[which(is.na(Jhou_SummaryAll$sex)|Jhou_SummaryAll$sex == "```"),] # NA values are from empty observations so you can omit those for now

Jhou_SummaryAll %<>% 
  mutate(
    # wakeforest_id = gsub(".*-->", "", wakeforest_id),
         shipment_cohort = as.character(round(as.numeric(shipment_cohort), 2)),
         wfucohort = as.character(round(as.numeric(shipment_cohort)),3)) %<>%
  rename("labanimalid" = "lab_animal_id") %<>% 
  select(labanimalid, shipment_cohort, wfucohort, everything()) %<>% 
  dplyr::filter(!is.na(rfid)) 

# account for the switch that Alen documented over email
# newrow <- Jhou_SummaryAll[53,]
# Jhou_SummaryAll <- rbind(Jhou_SummaryAll[1:52,],newrow,Jhou_SummaryAll[-(1:52),])
# Jhou_SummaryAll[54, ] <- Jhou_SummaryAll %>% 
#   slice(54) %>% 
#   mutate(labanimalid = "U52", 
#          wakeforestid = "TJ053")
# Jhou_SummaryAll[52, ] <- Jhou_SummaryAll %>% 
#   slice(52) %>% 
#   mutate(labanimalid = "U53", 
#          wakeforestid = "TJ052")
# Jhou_SummaryAll <- Jhou_SummaryAll[-53,]
# extract their copy of the WFU shipment information and qc in QC_Jhou_WFU.R; left the ``` sex 
Jhou_SummaryAll %<>% 
  mutate(notesforhumans = gsub("(Yellow background .*)|(Behavior values .*)", NA, notesforhumans)) # remove the notes for humans that are relevant to the animal
# clean up extra spaces 
Jhou_SummaryAll <- Jhou_SummaryAll %>% 
  mutate(box = gsub(" ", "", box))
# organize cohort so that the graphs are in order 
Jhou_SummaryAll <- Jhou_SummaryAll %>% 
  mutate(wfucohort = factor(wfucohort, levels = sort(unique(as.numeric(Jhou_SummaryAll$wfucohort)))))


################################
###(update) Summary All ######## # since 12/11 the file is no longer there
################################

WFU_Jhou_df <-  WFU_Jhou_test_df %>% 
mutate(cohort = stringr::str_match(cohort, "#(\\d+).*?")[,2],
       cohort = ifelse(nchar(cohort) > 1, cohort, gsub('([[:digit:]]{1})$', '0\\1', cohort))) # reformat wfu cohorts, add leading zeros
WFU_Jhou_df

# # see below for the summary all with updated link -- not the right information for some 

Jhou_SummaryAll_updated <- Jhou_Excel_updated[["Summary all"]] 
Jhou_SummaryAll_updated <- Jhou_SummaryAll_updated[, c(1:13, 52,53)]
names(Jhou_SummaryAll_updated) <-  Jhou_SummaryAll_updated[1,] %>% as.character()
Jhou_SummaryAll_updated <- Jhou_SummaryAll_updated[-1, ] 
names(Jhou_SummaryAll_updated) <- mgsub::mgsub(names(Jhou_SummaryAll_updated),
                                       c(" |\\.|:", "#", "16 digit ID", "Date of Wean|Wean Date","Jhou lab", "Date of Ship", "Dams"),
                                       c("", "Number", "RFID", "DOW","LabAnimal", "ShipmentDate", "Dames")) 
names(Jhou_SummaryAll_updated) %<>% tolower
# # clean up variables
Jhou_SummaryAll_updated$coatcolor <- mgsub::mgsub(Jhou_SummaryAll_updated$coatcolor, 
                                          c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood", "[A|a]lbino"), 
                                          c("BROWN", "BLACK", "HOOD", "ALBINO"))
Jhou_SummaryAll_updated$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", Jhou_SummaryAll_updated$coatcolor)
Jhou_SummaryAll_updated$coatcolor <- toupper(Jhou_SummaryAll_updated$coatcolor)

datecols <- c("dob", "dow", "shipmentdate")
datefunction <- function(x){
  if(is.POSIXct(x) == F){
    as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
  } else x
}
Jhou_SummaryAll_updated <- Jhou_SummaryAll_updated %>% 
  mutate_at(.vars = vars(datecols), .funs = datefunction)

Jhou_SummaryAll_updated[which(is.na(Jhou_SummaryAll_updated$sex)|Jhou_SummaryAll_updated$sex == "```"),] # NA values are from empty observations so you can omit those for now

Jhou_SummaryAll_updated %<>% 
  mutate(wakeforestid = gsub(".*-->", "", wakeforestid),
         shipmentcohort_wfu = as.character(round(as.numeric(shipmentcohort)),3)) %<>%
  dplyr::filter(!is.na(wakeforestid)) 

# extract their copy of the WFU shipment information and qc in QC_Jhou_WFU.R; left the ``` sex 



################################
########### Runway #############
################################
setwd("~/Dropbox (Palmer Lab)/U01 folder")

Jhou_Runway_xl <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 26, 35, 56:57)]
names(Jhou_Runway_xl) <- Jhou_Runway_xl[1,] %>% unlist() %>% as.character() %>% janitor::make_clean_names()

Jhou_Runway_xl <- Jhou_Runway_xl %>% 
  rename("rfid" = "x16_digit_id",
         "jhou_cohort" = "shipment_cohort",
         "date" = "date_of_first_runway_test",
         "labanimalid" = "jhou_lab_id",
         "comments" = "notes_for_humans") %>% 
  mutate(cohort = paste0("C", str_pad(gsub("[.].*", "", jhou_cohort), 2, "left", "0")),
         jhou_cohort = as.character(as.numeric(jhou_cohort)),
         comments = replace(comments, grepl("Yellow", comments), NA)) %>% 
  mutate_at(vars(matches("runway")), as.numeric)

Jhou_Runway_xl_df <- Jhou_Runway_xl %>% mutate(labanimalid = toupper(labanimalid)) %>% 
  subset(grepl("U\\d+", labanimalid)&!is.na(rfid)) %>% 
  mutate(cohort = replace(cohort, grepl("NA", cohort), NA)) %>% 
  left_join(WFU_Jhou_test_df[, c("cohort", "rfid", "sex", "dob")], by = "rfid") %>% 
  mutate(cohort = coalesce(cohort.x, cohort.y)) %>% 
  select(-cohort.x, -cohort.y) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, everything(), comments, resolution) %>% 
  select(-date)












## XX QC AFTER CONFERENCE 


## Runway
Jhou_Runway_trials_xl <- Jhou_Excel[["Runway"]] %>% as.data.table()
Jhou_Runway_C01_16_names <- data.frame(labanimalid = Jhou_Excel[["Runway"]][,1:713] %>% names %>% as.character) %>% subset(!grepl("Animal ID", labanimalid)) %>% mutate(labanimalid = toupper(labanimalid))
Jhou_Runway_trials_C01_16 <- Jhou_Runway_trials_xl[1:22, ] %>% t() %>% as.data.frame() %>% mutate_all(as.character)

colnames(Jhou_Runway_trials_C01_16) <- as.character(Jhou_Runway_trials_C01_16[1,]) %>% janitor::make_clean_names()
Jhou_Runway_trials_C01_16 <- Jhou_Runway_trials_C01_16[-1,] %>%  # data.table cannot delete rows by reference with tJhou_Runway[1 := NULL,] 
  mutate_at(vars(-one_of("gender", "notes")), as.numeric) %>% 
  head(712) # ends at U712, end of Cohort 16

Jhou_Runway_trials_C01_16_df <- cbind(Jhou_Runway_trials_C01_16, Jhou_Runway_C01_16_names)  # reorder and drop "na" column

## qc xl data with xl data (add rfid and verify/fill sex by joining by labanimalid) 
Jhou_Runway_trials_C01_16_df <- Jhou_Runway_trials_C01_16_df %>% 
  left_join(Jhou_SummaryAll[c("rfid", "sex", "labanimalid")], by = "labanimalid") %>% 
  left_join(WFU_Jhou_test_df[, c("rfid", "cohort")], by = "rfid") # wfucohort had na's from a batch that died in the heat, using this instead

# create xl of misgendered animals 
Jhou_Runway_trials_C01_16_df %>% 
  subset(sex != gender) %>% select(cohort, labanimalid, rfid, sex, gender) %>%
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Jhou_U01DA044468/Bonnie's Codes/QC/runway_c01_16_misgendered.xlsx")
Jhou_Runway_trials_C01_16_df %>% 
  subset(is.na(gender)) %>% select(cohort, labanimalid, rfid, sex, gender) %>%
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Jhou_U01DA044468/Bonnie's Codes/QC/runway_c01_16_misssinggender.xlsx")


# select(-na, -gender) %>% 
  # select(cohort, rfid, sex, labanimalid, notes, everything()) 


## commented out old code for reversals -- no longer extracting 11/04/2020
# # append reversals to column names
# reversalstartindex <- which(grepl("^Hab", colnames(tJhou_Runway)))[3] # find the third occurence of habituation 1,2,1,2
# reversalendindex <- which(grepl("^Coc.*12$", colnames(tJhou_Runway)))[2] # find the second occurence of cocaine12 (1, 2)
# 
# colnames(tJhou_Runway)[reversalstartindex:reversalendindex] <- paste0(colnames(tJhou_Runway)[reversalstartindex:reversalendindex], "Reversals") # append reversals
# 
# 
# # separate reversals from elapsed time information
# 
# tJhou_Runway_vars_nonrevers <- grep("^(Gender|Habituation \\d|Coc|Animal)", colnames(tJhou_Runway), value = T) %>% 
#   grep('\\d+(?!Reversals)$', . , value = T, perl=T) 
# tJhou_Runway_nonreverswide <- tJhou_Runway[, tJhou_Runway_vars_nonrevers, with=FALSE]
# names(tJhou_Runway_nonreverswide) <- ifelse(str_count(tJhou_Runway_vars_nonrevers, "\\d")==1, gsub(" (\\d)", "0\\1", tJhou_Runway_vars_nonrevers), gsub(" ", "", tJhou_Runway_vars_nonrevers))
# tJhou_Runway_nonreverswide[, animalid := names(Jhou_Runway)[-1]] # remove animal id element but retain the animal ids for the data
# 
# 
# tJhou_Runway_vars_reversals <- grep("^(Gender|Habituation \\d|Coc|Animal)", colnames(tJhou_Runway), value = T) %>% 
#   grep('\\d+(?=Reversals)', . , value = T, perl=T) 
# tJhou_Runway_reverswide <- tJhou_Runway[, tJhou_Runway_vars_reversals, with=FALSE]
# setnames(tJhou_Runway_reverswide, gsub("Reversals", "", colnames(tJhou_Runway_reverswide)) )
# names(tJhou_Runway_reverswide) <- ifelse(str_count(tJhou_Runway_vars_reversals, "\\d")==1, gsub(" (\\d)", "0\\1", names(tJhou_Runway_reverswide)), gsub(" ", "", names(tJhou_Runway_reverswide)))
# tJhou_Runway_reverswide[, animalid := names(Jhou_Runway)[-1]] # remove animal id element but retain the animal ids for the data
# 
# 
# # convert wide to long formats for both reversals and elapse  d time datasets 
# 
# tJhou_Runway_nonreverslong <- gather(tJhou_Runway_nonreverswide, session, elapsedtime, `Habituation01`:`Cocaine12`, factor_key=F) 
# tJhou_Runway_reverslong <- gather(tJhou_Runway_reverswide, reversalsession, numreversals, `Habituation01`:`Cocaine12`, factor_key=F) 
# 
# ############################## PICK UP FROM HERE: SHOULD ONLY BE 6608 VALUES BUT WE FIND 92512 CASES AFTER UNSUCCESSFUL MERGE
# tJhou_Runway_data <- left_join(tJhou_Runway_nonreverslong, tJhou_Runway_reverslong, by = c("animalid", "session" = "reversalsession")) %>% 
#   # mutate_all() %>% ## XX 
#   arrange(animalid, session) # all ids are represented 14 times
# 
# # extract the notes (create specific comments table)
# tJhou_Runway_notes <- tJhou_Runway[, "notes", with = FALSE]
# tJhou_Runway_notes[, animalid := names(Jhou_Runway)[-1]]



################################
########### LOCOMOTOR ##########
################################
setwd("~/Dropbox (Palmer Lab)/U01 folder")

Jhou_Locomotor_xl <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 43:44, 55:56)]
names(Jhou_Locomotor_xl) <- Jhou_Locomotor_xl[1,] %>% unlist() %>% as.character() %>% janitor::make_clean_names()

Jhou_Locomotor_xl <- Jhou_Locomotor_xl %>% 
  rename("rfid" = "x16_digit_id",
         "jhou_cohort" = "shipment_cohort",
         "labanimalid" = "jhou_lab_id",
         "comments" = "notes_for_humans") %>% 
  mutate(cohort = paste0("C", str_pad(gsub("[.].*", "", jhou_cohort), 2, "left", "0")),
         jhou_cohort = as.character(as.numeric(jhou_cohort)),
         comments = replace(comments, grepl("Yellow", comments), NA)) %>% 
  mutate_at(vars(matches("locomotor")), as.numeric)

Jhou_Locomotor_xl_df <- Jhou_Locomotor_xl %>% mutate(labanimalid = toupper(labanimalid)) %>% 
  subset(grepl("U\\d+", labanimalid)&!is.na(rfid)) %>% 
  mutate(cohort = replace(cohort, grepl("NA", cohort), NA)) %>% 
  left_join(WFU_Jhou_test_df[, c("cohort", "rfid", "sex", "dob")], by = "rfid") %>% 
  mutate(cohort = coalesce(cohort.x, cohort.y)) %>% 
  select(-cohort.x, -cohort.y) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, everything(), comments, resolution)









## XX QC AFTER CONFERENCE 


Jhou_Locomotor <- Jhou_Excel[["Locomotor"]] %>% as.data.table
Jhou_Locomotor <- Jhou_Locomotor[,1:39] 
Jhou_Locomotor[, paste0("...", c(32,34,37)) := NULL][] # remove all na columns
setnames(Jhou_Locomotor, c("labanimalid", paste0("minute", 1:30),"Date", "first15avg_beforefooddep", "last15avg_beforefooddep", "first15avg_afterfooddep","last15avg_afterfooddep") )
Jhou_Locomotor_ID <- Jhou_Locomotor[grepl("^U", labanimalid), labanimalid] # hold vector of the labanimalids
Jhou_Locomotor <- Jhou_Locomotor[grepl("^u|binned", labanimalid, ignore.case = T) & !is.na(minute1) & !is.na(labanimalid),] # create the minute data first and then cbind to the total and averages data
# Jhou_Locomotor_df <- Jhou_Locomotor %>% as.data.frame() 

# get the minute count 
#add id based on session information
Jhou_Locomotor$labanimalidsession <- NA

i <- 1
j <- 1
repeat {
  Jhou_Locomotor$labanimalidsession[i] <- Jhou_Locomotor_ID[j]
  i = i + 1
  j = j 
  if (grepl("^U", Jhou_Locomotor$labanimalid[i])){
    j = j + 1
  }
} 
Jhou_Locomotor <- Jhou_Locomotor[grepl("^binned", labanimalid, ignore.case = T),] %>% 
  rename("session" = "labanimalid",
         "labanimalid" = "labanimalidsession")

Jhou_Locomotor %>% select(labanimalid, )

# get the total counts 
# get the average counts 
# assign the session information

################################
#### PROGRESSIVE PUNISHMENT ####
################################
setwd("~/Dropbox (Palmer Lab)/U01 folder")

Jhou_ProgPunishment_xl <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 39:40, 55:56)]
names(Jhou_ProgPunishment_xl) <- Jhou_ProgPunishment_xl[1,] %>% unlist() %>% as.character() %>% janitor::make_clean_names()

Jhou_ProgPunishment_xl <- Jhou_ProgPunishment_xl %>% 
  rename("rfid" = "x16_digit_id",
         "jhou_cohort" = "shipment_cohort",
         "mean_shock_breakpoint_ma" = "corrected_pp_breakpoint", 
         "labanimalid" = "jhou_lab_id",
         "comments" = "notes_for_humans") %>% 
  mutate(cohort = paste0("C", str_pad(gsub("[.].*", "", jhou_cohort), 2, "left", "0")),
         jhou_cohort = as.character(as.numeric(jhou_cohort)),
         comments = replace(comments, grepl("Yellow", comments), NA),
         mean_shock_breakpoint_ma = as.numeric(mean_shock_breakpoint_ma))

Jhou_ProgPunishment_xl_df <- Jhou_ProgPunishment_xl %>% mutate(labanimalid = toupper(labanimalid)) %>% 
  subset(grepl("U\\d+", labanimalid)&!is.na(rfid)) %>% 
  mutate(cohort = replace(cohort, grepl("NA", cohort), NA)) %>% 
  left_join(WFU_Jhou_test_df[, c("cohort", "rfid", "sex", "dob")], by = "rfid") %>% 
  mutate(cohort = coalesce(cohort.x, cohort.y)) %>% 
  select(-cohort.x, -cohort.y) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, everything(), comments, resolution)
  








## XX QC AFTER CONFERENCE 
# repeat sessions are in red to remove, use format to remove 

Jhou_ProgPun <- Jhou_Excel[["Progressive Punishment"]] %>% as.data.table
Jhou_ProgPun <- Jhou_ProgPun[, 1:13] 
setnames(Jhou_ProgPun, c("labanimalid", as.character(Jhou_ProgPun[2, 2:13])) )
# check if structure is consistent (it isn't)
idindex <- grep("^U", Jhou_ProgPun$labanimalid)
zeroindex <- grep("^0", Jhou_ProgPun$labanimalid)
zeroindex - idindex

Jhou_Excel_ProgressivePunishment_formats_cellbycell <- tidyxl::xlsx_cells("U01 Master sheet_readonly.xlsx") %>% 
  dplyr::filter(sheet == "Progressive Punishment")
Jhou_Excel_ProgressivePunishment_formats <- tidyxl::xlsx_formats("U01 Master sheet_readonly.xlsx")


### GENERALIZE TO ALL NON BLACK COLORS
blackexample_progpun <- Jhou_Excel_ProgressivePunishment_formats_cellbycell %>% dplyr::filter(row == 4, col == 2)
wantedhexa_progpun <- Jhou_Excel_ProgressivePunishment_formats$local$font$color$rgb[blackexample_progpun$local_format_id]
nonblackhexa_indices <- which(Jhou_Excel_ProgressivePunishment_formats$local$font$color$rgb != wantedhexa_progpun)
nonblackrows_Jhou_Excel_ProgPun <- Jhou_Excel_ProgressivePunishment_formats_cellbycell[Jhou_Excel_ProgressivePunishment_formats_cellbycell$local_format_id %in% nonblackhexa_indices, ] %>% 
  dplyr::filter(data_type != "blank") %>% 
  select(row, col, data_type, logical, numeric, date, character) 
nonblackrows_Jhou_Excel_ProgPun %>% dplyr::filter(!is.na(numeric))
# using this strategy reduces the originally 11 length vector to 9 (removing black and NA)

# get the information about the red point; get the hexademical string
# redexample <- Jhou_Excel_ProgressivePunishment_formats_cellbycell %>% dplyr::filter(row == 351, col == 1)
# wantedhexa <- Jhou_Excel_ProgressivePunishment_formats$local$font$color$rgb[redexample$local_format_id]
# wantedhexa_indices <- which(Jhou_Excel_ProgressivePunishment_formats$local$font$color$rgb == wantedhexa)
# redrows <- Jhou_Excel_ProgressivePunishment_formats_cellbycell[Jhou_Excel_ProgressivePunishment_formats_cellbycell$local_format_id %in% wantedhexa_indices, ] %>% 
#   dplyr::filter(!is.na(numeric)) %>% select(row) %>% unique() %>% mutate(row = row - 1)
# Jhou_Excel_ProgressivePunishment_red <- Jhou_Excel_ProgressivePunishment_formats_cellbycell[Jhou_Excel_ProgressivePunishment_formats_cellbycell$local_format_id %in% wantedhexa_indices, ] %>% select(row, col)
# 
# # remove the red rows and clean up data
# # create the compromised data
# labanimalid_indices <- grep("^U", Jhou_ProgPun_nored$labanimalid, ignore.case = F)
# Jhou_ProgPun_nored_split <- split(Jhou_ProgPun_nored, cumsum(1:nrow(Jhou_ProgPun_nored) %in% labanimalid_indices))
# 
# Jhou_ProgPun_nored <- Jhou_ProgPun[-redrows$row, ]
# 
# ## get indices of labanimalid
# Jhou_ProgPun_split <- split(Jhou_ProgPun, cumsum(1:nrow(Jhou_ProgPun) %in%  grep("^U", Jhou_ProgPun$labanimalid, ignore.case = F))) 
# 
# Jhou_ProgPun_split <- lapply(Jhou_ProgPun_split, function(x){ 
#   x <- x %<>% 
#     rename("date" = "Date",
#            "shockoflastcompletedblock" = "Last block completed (mA)",
#            "shockoflastattemptedblock" = "Last trial attempted (mA)",
#            "numtrialsatlastshock" = "# of trials at last Shock Intensity",
#            "activepresses" = "Active (left)",
#            "inactivepresses" = "Inactive (right)",
#            "boxorstationumber" = "Box #",
#            "session" = "labanimalid") %<>% 
#     dplyr::mutate(labanimalid = grep("U\\d+", session, value = T),
#                  date = as.POSIXct(as.numeric(date) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) %>% 
#     dplyr::select(-c(Avg, Time, FR, `Weight (%)`))
#   return(x)
#     }) %>% rbindlist()
# 
# Jhou_ProgPun_red <- Jhou_ProgPun_split[redrows$row, ]

## TO DO: CHANGE NUMLEFTPRESSESLAST HERE AND IN PROGPUN RAW DATA (done)
Jhou_ProgPun_Excel <- lapply(Jhou_ProgPun_nored_split, function(x){
 x <- x %>% 
    rename("date" = "Date",
           "shockoflastcompletedblock" = "Last block completed (mA)",
           "shockoflastattemptedblock" = "Last trial attempted (mA)",
           "numtrialsatlastshock" = "# of trials at last Shock Intensity",
           "activepresses" = "Active (left)",
           "inactivepresses" = "Inactive (right)",
           "boxorstationumber" = "Box #",
           "session" = "labanimalid") %>% 
   dplyr::select(-c(Avg, Time, FR, `Weight (%)`)) %>%
   dplyr::mutate(labanimalid = grep("U\\d+", session, value = T),
                 date = as.POSIXct(as.numeric(date) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) %>% 
   dplyr::filter(!is.na(as.numeric(shockoflastcompletedblock))) 
 progpunmeasures.num <- grep(pattern = "(?=shock|presses)", names(x), perl = T, value = T)
 x[progpunmeasures.num] <- sapply(x[progpunmeasures.num],as.numeric)
 
 return(x)
}) %>% rbindlist() #2942 observations

# remove Time bc the values are invalid and sparse (subset(Jhou_ProgPun, !is.na(as.numeric(Time))))

################################
#### PROGRESSIVE RATIO ####
################################
setwd("~/Dropbox (Palmer Lab)/U01 folder")

Jhou_ProgRatio_xl <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 40:41, 55:56)]
names(Jhou_ProgRatio_xl) <- Jhou_ProgRatio_xl[1,] %>% unlist() %>% as.character() %>% janitor::make_clean_names()

Jhou_ProgRatio_xl <- Jhou_ProgRatio_xl %>% 
  rename("rfid" = "x16_digit_id",
         "jhou_cohort" = "shipment_cohort",
         "mean_leverpresses_maxratio" = "pr_breakpoint_lever_presses", 
         "labanimalid" = "jhou_lab_id",
         "comments" = "notes_for_humans") %>% 
  mutate(cohort = paste0("C", str_pad(gsub("[.].*", "", jhou_cohort), 2, "left", "0")),
         jhou_cohort = as.character(as.numeric(jhou_cohort)),
         comments = replace(comments, grepl("Yellow", comments), NA),
         mean_leverpresses_maxratio = as.numeric(mean_leverpresses_maxratio))

Jhou_ProgRatio_xl_df <- Jhou_ProgRatio_xl %>% mutate(labanimalid = toupper(labanimalid)) %>% 
  subset(grepl("U\\d+", labanimalid)&!is.na(rfid)) %>% 
  mutate(cohort = replace(cohort, grepl("NA", cohort), NA)) %>% 
  left_join(WFU_Jhou_test_df[, c("cohort", "rfid", "sex", "dob")], by = "rfid") %>% 
  mutate(cohort = coalesce(cohort.x, cohort.y)) %>% 
  select(-cohort.x, -cohort.y) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, everything(), comments, resolution)









## XX QC AFTER CONFERENCE 


Jhou_ProgRatio <- Jhou_Excel[["Progressive ratio"]] %>% as.data.table
setnames(Jhou_ProgRatio, c("date", as.character(Jhou_ProgRatio[3, 2:17])) )
Jhou_ProgRatio[,c(7:8, 13:14,16) := NULL ]
# split the data by animals, rename the columns for mean, remove the colnames rows if session isn't numeric? 
setnames(Jhou_ProgRatio, 7:9, paste0(names(Jhou_ProgRatio)[7:9], '_mean'))
setnames(Jhou_ProgRatio, 11, 'covariance')
Jhou_ProgRatio_split <- split(Jhou_ProgRatio, cumsum(1:nrow(Jhou_ProgRatio) %in% grep("^U", Jhou_ProgRatio$date, ignore.case = F)))
Jhou_ProgRatio_Excel <- lapply(Jhou_ProgRatio_split, function(x){
  names(x) <- mgsub::mgsub(names(x), c(" |-|#", ",", "L", "R", "Press"), c("", "_", "active", "inactive", "presses")) 
  names(x) %<>% tolower()
  x$labanimalid = grep("^U", x$date, ignore.case = F, value = T) 
  # x$covariance = grep("\\d+", x$covariance, ignore.case = F, value = T)
  # x <- x %>% 
  #   dplyr::filter(!is.na(as.numeric(session))) 
  rownames(x) <- NULL
  x <- x %>% 
    dplyr::mutate(date = as.POSIXct(as.numeric(date) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d"),
                  covariance = dplyr::nth(x$covariance, 2), 
                  maxratio_stdev = x$maxratio_mean[2],
                  activepresses_stdev = x$activepresses_mean[2], 
                  inactivepresses_stdev = x$inactivepresses_mean[2], 
                  maxratio_sem = x$maxratio_mean[3],
                  activepresses_sem = x$activepresses_mean[3], 
                  inactivepresses_sem = x$inactivepresses_mean[3] )  %>%
    dplyr::mutate( maxratio_mean = x$maxratio_mean[1],
                  activepresses_mean = x$activepresses_mean[1], 
                  inactivepresses_mean = x$inactivepresses_mean[1]) 
  return(x)
}) %>% rbindlist()
# return id's for which sessions don't match the number of rows
conflictedcases_progratio <- Jhou_ProgRatio_Excel %>% group_by(labanimalid) %>% add_count(n = n()) %>% dplyr::filter(max(as.numeric(session)) != n) %>% select(labanimalid) %>% unique()

# use tidyxl to extract the red cases

Jhou_ProgRatio_formats_cellbycell <- tidyxl::xlsx_cells("U01 Master sheet_readonly.xlsx") %>% 
  dplyr::filter(sheet == "Progressive ratio")
Jhou_Excel_ProgRatio_formats <- tidyxl::xlsx_formats("U01 Master sheet_readonly.xlsx")
blackexample <- Jhou_ProgRatio_formats_cellbycell %>% dplyr::filter(row == 1, col == 1)
wantedhexa <- Jhou_Excel_ProgRatio_formats$local$font$color$rgb[blackexample$local_format_id]
wantedhexa_indices <- which(Jhou_Excel_ProgRatio_formats$local$font$color$rgb != wantedhexa) # not black
nonblackrows <- Jhou_ProgRatio_formats_cellbycell[Jhou_ProgRatio_formats_cellbycell$local_format_id %in% wantedhexa_indices, ] %>% 
  dplyr::filter(!is.na(numeric)) %>% select(row) %>% unique()
Jhou_Excel_ProgRatio_red <- Jhou_ProgRatio_formats_cellbycell[Jhou_ProgRatio_formats_cellbycell$local_format_id %in% wantedhexa_indices, ] %>% select(row, col) %>% mutate(row = row - 1)

# if above approach doesn't work, you can refer back to this value: 
"FFFF0000"

# remove the red rows and clean up data
# create the compromised data
labanimalid_indices <- grep("^U", Jhou_ProgPun_nored$labanimalid, ignore.case = F)
Jhou_ProgPun_nored_split <- split(Jhou_ProgPun_nored, cumsum(1:nrow(Jhou_ProgPun_nored) %in% labanimalid_indices))

Jhou_ProgPun_nored <- Jhou_ProgPun[-redrows$row, ]

################################
###### DELAYED PUNISHMENT ######
################################
# Jhou_DelayedPunishment_xl <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 48:51, 55:56, 153:173)]

Jhou_DelayedPunishment_xl <- Jhou_Excel[["Summary all"]][, c(1, 2, 6, 48:51, 55:56)]

names(Jhou_DelayedPunishment_xl) <- Jhou_DelayedPunishment_xl[1,] %>% unlist() %>% as.character() %>% janitor::make_clean_names()
Jhou_DelayedPunishment_xl <- Jhou_DelayedPunishment_xl %>% 
  select(-matches("^na(_\\d)?$")) # remove the all na space columns
names(Jhou_DelayedPunishment_xl) <- mgsub::mgsub(names(Jhou_DelayedPunishment_xl), 
                                                 c("x(\\d)$", "x(\\d)_2$", "x(\\d)_3$"),
                                                 c("dp0_trial\\1", "dp20_trial_\\1", "dp40_trial_\\1"))
Jhou_DelayedPunishment_xl <- Jhou_DelayedPunishment_xl %>% 
  rename("rfid" = "x16_digit_id",
         "jhou_cohort" = "shipment_cohort",
         "labanimalid" = "jhou_lab_id",
         "mean_dp0_1_lastshock" = "x0_sec",
         "mean_dp20_1_lastshock" = "x20s", 
         "mean_dp0_2_lastshock" = "x0s",
         "mean_dp4540_2_lastshock" = "x45_40s", 
         "comments" = "notes_for_humans") %>% 
  mutate(cohort = paste0("C", str_pad(gsub("[.].*", "", jhou_cohort), 2, "left", "0")),
         jhou_cohort = as.character(as.numeric(jhou_cohort)),
         comments = replace(comments, grepl("Yellow", comments), NA)) %>% 
  mutate_at(vars(starts_with("mean")), as.numeric)
Jhou_DelayedPunishment_xl_df <- Jhou_DelayedPunishment_xl %>% mutate(labanimalid = toupper(labanimalid)) %>% 
  subset(grepl("U\\d+", labanimalid)&!is.na(rfid)) %>% 
  mutate(cohort = replace(cohort, grepl("NA", cohort), NA)) %>% 
  left_join(WFU_Jhou_test_df[, c("cohort", "rfid", "sex", "dob")], by = "rfid") %>% 
  mutate(cohort = coalesce(cohort.x, cohort.y)) %>% 
  select(-cohort.x, -cohort.y) %>% 
  select(cohort, jhou_cohort, labanimalid, rfid, comments, resolution, everything())

# commented out on 09/11/2020
# Jhou_Delayedpun <- Jhou_Excel[["Delayed punishment (DP)"]] %>% as.data.table
# Jhou_Delayedpun <- Jhou_Delayedpun[, 1:12, with=T]
# setnames(Jhou_Delayedpun, c("delay", as.character(Jhou_Delayedpun[2, 2:12])) )
# 
# # (Commented out on 2/18 because I'm doing a new approach where I don't drop any ID's and keeping all records)
# # use row indices to remove the rows
# # remove non black rows by matching delay and shock intensity of last trial completed and # of active lever presses
# # get the character 
# # setwd("~/Dropbox (Palmer Lab)/U01 folder")
# # Jhou_Delayedpun_formats_cellbycell <- tidyxl::xlsx_cells("U01 Master sheet_readonly.xlsx") %>% 
# #   dplyr::filter(sheet == "Delayed punishment (DP)")
# # Jhou_Excel_Delayedpun_formats <- tidyxl::xlsx_formats("U01 Master sheet_readonly.xlsx")
# # wantedhexa_Delayedpun <- Jhou_Excel_Delayedpun_formats$local$font$color$rgb[Jhou_Delayedpun_formats_cellbycell[which(Jhou_Delayedpun_formats_cellbycell$row == 1 & Jhou_Delayedpun_formats_cellbycell$col == 15),]$local_format_id]
# # wantedhexa_indices_Delayedpun <- which(Jhou_Excel_Delayedpun_formats$local$font$color$rgb != wantedhexa_Delayedpun) # not black
# # nonblackrows_Delayedpun <- Jhou_Delayedpun_formats_cellbycell[Jhou_Delayedpun_formats_cellbycell$local_format_id %in% wantedhexa_indices_Delayedpun, ] %>% 
# #   dplyr::filter(!is.na(numeric)) %>% select(row) %>% unique() %>% mutate(row = row - 1)
# # # Jhou_Excel_ProgRatio_red <- Jhou_ProgRatio_formats_cellbycell[Jhou_ProgRatio_formats_cellbycell$local_format_id %in% wantedhexa_indices, ] %>% select(row, col) %>% mutate(row = row - 1)
# # Jhou_Delayedpun <- Jhou_Delayedpun[-nonblackrows_Delayedpun$row,] # with nonblack rows removed 
# 
# # split the data
# Jhou_Delayedpun_split <- split(Jhou_Delayedpun, cumsum(1:nrow(Jhou_Delayedpun) %in%  grep("^U", Jhou_Delayedpun$delay, ignore.case = F))) 
# Jhou_Delayedpun_Excel <- lapply(Jhou_Delayedpun_split, function(x){
#   x %<>% select(-c(Time, FR, `Weight (%)`)) %<>% dplyr::filter(!is.na(delay)) # remove na or unwanted columns
#   names(x) <- c("delay", "date", "shockoflastcompletedblock", "shockoflastattemptedblock", "numtrialsatlastshock", "activepresses", "inactivepresses", "boxorstationumber", "notes") #rename to equate raw names
#   x$labanimalid = grep("^U", x$delay, ignore.case = F, value = T) 
#   x %<>% dplyr::filter(grepl("^\\d", x$delay)) %<>% mutate(date = as.POSIXct(as.numeric(date) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) %>% group_by(labanimalid) %<>% mutate(session = dplyr::row_number())
#   return(x)
# }) %>% rbindlist(fill = T)
# 
# 
# ## XX PICK UP 5/19
# Jhou_Delayedpun_Excel %>% 
#   left_join(., Jhou_SummaryAll[, c("labanimalid", "shipmentcohort")], by = "labanimalid") %>% 
#   mutate(cohort = gsub("[.]\\d", "", shipmentcohort) %>% as.numeric) %>% 
#   select(cohort) %>%
#   table()
# 
# # Jhou_Delayedpun_Excel %>% 
# #   group_by(labanimalid) %>%
# #   dplyr::filter(session == max(session))



################################
###### GOOGLE SHEETS  ##########
################################
library(gsheet)

Jhou_Ratweights <- list()
Jhou_Ratweights[[1]] <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1f17kzWnu2nwZ5VfWvKOZibH5Cw_Y1tt6p-zLJWbrsDw/edit#gid=691824374')
Jhou_Ratweights[[2]] <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1f17kzWnu2nwZ5VfWvKOZibH5Cw_Y1tt6p-zLJWbrsDw/edit#gid=847270128')

Jhou_Ratweights_U1_392 <- Jhou_Ratweights[[1]] %>% 
  as.data.frame() %>% 
  t() %>%
  as.data.frame() %>% 
  mutate_all(as.character)
names(Jhou_Ratweights_U1_392) <- Jhou_Ratweights_U1_392[1,]
names(Jhou_Ratweights_U1_392)[1] <- "labanimalid"
Jhou_Ratweights_U1_392_df <- Jhou_Ratweights_U1_392 %>% 
  subset(grepl("U\\d+?", labanimalid))
Jhou_Ratweights_U1_392_df <- Jhou_Ratweights_U1_392_df[,colSums(is.na(Jhou_Ratweights_U1_392_df))<nrow(Jhou_Ratweights_U1_392_df)] # drop out columns that are all na

Jhou_Ratweights_U1_392_date_session <- names(Jhou_Ratweights_U1_392_df[4:length(Jhou_Ratweights_U1_392_df)]) %>% as.data.frame() %>%
  rename("date" = ".") %>%
  mutate_all(as.character) %>%
  mutate(date = sub("[.]\\d+", "", date),
         date = replace(date, grepl("Session", date), NA),
         date = replace(date, date == "NA", NA)) %>% 
  fill(date) %>% 
  group_by(date) %>% 
  mutate(session = row_number()) %>%
  ungroup() %>% 
  mutate(date = ifelse(grepl("^\\d{7,8}$", date), str_pad(date, 8, "left", "0"), as.character(date)),
         date = ifelse(grepl("\\d{8}", date), sub("(\\d{2})(\\d{2})", '\\1/\\2/', date) %>% lubridate::mdy() %>% as.character(), as.character(date)),
         # date = ifelse(grepl("\\d{7}", date), lubridate::mdy(date) %>% as.character(), as.character(date)),
         date = ifelse(grepl("\\d+?/\\d+?", date), lubridate::mdy(date) %>% as.character(), as.character(date)),
         date = paste0(date, "_", session),
         date = make.unique(date)) %>% 
  select(-session) 
  
names(Jhou_Ratweights_U1_392_df)[4:length(Jhou_Ratweights_U1_392_df)] <- Jhou_Ratweights_U1_392_date_session$date
Jhou_Ratweights_U1_392_df <- gather(Jhou_Ratweights_U1_392_df, "date", "weight","2018-07-07_1":"2019-08-30_1") %>%
  rename("init_weight" = "Initial Weight",
         "goal_weight" = "(g)") %>% 
  mutate_at(vars(matches("weight")), as.numeric)  %>% 
  subset(!is.na(weight)) %>% 
  separate(date, into = c("date", "session"), sep = "_") %>% 
  group_by(labanimalid) %>% 
  mutate(init_weight = replace(init_weight, is.na(init_weight), first(weight)),
         goal_weight = replace(goal_weight, is.na(goal_weight) | goal_weight == 0, 0.85 * init_weight),
         weight_pct = (weight/init_weight) * 100) %>% 
  ungroup() %>% 
  subset(weight_pct != 100)

## seems to be working! but are the dates at which the init weight taken important? do we need to consider how far the baseline is measured vs the dates of exp? (in that case, am worried about U100?? )


Jhou_Ratweights_U393_ <- Jhou_Ratweights[[2]] %>% 
  as.data.frame() %>% 
  t() %>%
  as.data.frame() %>% 
  mutate_all(as.character)
names(Jhou_Ratweights_U393_) <- Jhou_Ratweights_U393_[1,]
names(Jhou_Ratweights_U393_)[1] <- "labanimalid"
Jhou_Ratweights_U393_df <- Jhou_Ratweights_U393_ %>% 
  subset(grepl("U\\d+?", labanimalid))
Jhou_Ratweights_U393_df <- Jhou_Ratweights_U393_df[,colSums(is.na(Jhou_Ratweights_U393_df))<nrow(Jhou_Ratweights_U393_df)] # drop out columns that are all na

Jhou_Ratweights_U393_date_session <- names(Jhou_Ratweights_U393_df[4:length(Jhou_Ratweights_U393_df)]) %>% as.data.frame() %>%
  rename("date" = ".") %>%
  mutate_all(as.character) %>%
  mutate(date = sub("[.]\\d+", "", date),
         date = replace(date, grepl("Session", date), NA),
         date = replace(date, date == "NA", NA)) %>% 
  fill(date) %>% 
  group_by(date) %>% 
  mutate(session = row_number()) %>%
  ungroup() %>% 
  mutate(date = ifelse(grepl("^\\d{7,8}$", date), str_pad(date, 8, "left", "0"), as.character(date)),
         date = ifelse(grepl("\\d{8}", date), sub("(\\d{2})(\\d{2})", '\\1/\\2/', date) %>% lubridate::mdy() %>% as.character(), as.character(date)),
         # date = ifelse(grepl("\\d{7}", date), lubridate::mdy(date) %>% as.character(), as.character(date)),
         date = ifelse(grepl("\\d+?/\\d+?", date), lubridate::mdy(date) %>% as.character(), as.character(date)),
         date = paste0(date, "_", session),
         date = make.unique(date)) %>% 
  select(-session) 

names(Jhou_Ratweights_U393_df)[4:length(Jhou_Ratweights_U393_df)] <- Jhou_Ratweights_U393_date_session$date
Jhou_Ratweights_U393_df <- gather(Jhou_Ratweights_U393_df, "date", "weight","2019-08-23_1":"2020-01-16_1") %>%
  rename("init_weight" = "Initial Weight",
         "goal_weight" = "(g)") %>% 
  mutate_at(vars(matches("weight")), as.numeric)  %>% 
  subset(!is.na(weight)) %>% 
  separate(date, into = c("date", "session"), sep = "_") %>% 
  group_by(labanimalid) %>% 
  mutate(init_weight = replace(init_weight, is.na(init_weight), first(weight)),
         goal_weight = replace(goal_weight, is.na(goal_weight) | goal_weight == 0, 0.85 * init_weight),
         weight_pct = (weight/init_weight) * 100) %>% 
  ungroup() %>% 
  subset(weight_pct != 100)

Jhou_RatWeights_df <- rbind(Jhou_Ratweights_U1_392_df, Jhou_Ratweights_U393_df) %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  arrange(labanimalid, date, session) %>% 
  left_join(., Jhou_SummaryAll[, c("labanimalid", "shipmentcohort", "wfucohort", "sex", "dob")], by = c("labanimalid")) %>% # get sex and cohort information
  mutate(experimentage = as.numeric(date - dob)) 
  

Jhou_RatWeights_df %>% 
  ggplot() + 
  geom_line(aes(x = date, y = weight, group = labanimalid))
Jhou_RatWeights_df %>% 
  arrange(as.numeric(wfucohort)) %>% 
  subset(weight != 0 ) %>% 
  ggplot() + 
  geom_boxplot(aes(x = factor(wfucohort, levels = unique(wfucohort)), y = weight_pct, color = sex)) + 
  labs(x = "cohort", y = "weight percentage") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) 

Jhou_RatWeights_df %>% 
  subset(weight_pct < 70 & weight_pct > 50 | weight_pct > 100)

Jhou_RatWeights_df %>%   
  subset(weight != 0& experimentage > 0&experimentage < 400) %>%
  ggplot() +
  geom_point(aes(x = experimentage, y = weight_pct)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) 

  # arrange df by mixed column
df[mixedorder(df$Day),]



