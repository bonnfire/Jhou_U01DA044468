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

Jhou_Excel <- u01.importxlsx("U01 Master sheet_readonly.xlsx")

################################
########### Summary All ########
################################
# see summaryall from CREATE_RAW.R

################################
########### Runway #############
################################

Jhou_Runway <- Jhou_Excel[["Runway"]] %>% as.data.table
Jhou_Runway_test <- Jhou_Runway[1:16, ]
Jhou_Runway_test <- rbindlist(list(Jhou_Runway_test, as.list(names(Jhou_Runway))), fill=FALSE) # retain the column names before transposing
Jhou_Runway[, names(Jhou_Runway) := lapply(.SD, as.character)] # preserve values by turning into characters before transposing 


tJhou_Runway <- data.table::transpose(Jhou_Runway) # transpose data
colnames(tJhou_Runway) <- as.character(tJhou_Runway[1,])
tJhou_Runway <- tJhou_Runway[-1,] # data.table cannot delete rows by reference with tJhou_Runway[1 := NULL,]  

# append reversals to column names
reversalstartindex <- which(grepl("^Hab", colnames(tJhou_Runway)))[3] # find the third occurence of habituation 1,2,1,2
reversalendindex <- which(grepl("^Coc.*12$", colnames(tJhou_Runway)))[2] # find the second occurence of cocaine12 (1, 2)

colnames(tJhou_Runway)[reversalstartindex:reversalendindex] <- paste0(colnames(tJhou_Runway)[reversalstartindex:reversalendindex], "Reversals") # append reversals


# separate reversals from elapsed time information

tJhou_Runway_vars_nonrevers <- grep("^(Gender|Habituation \\d|Coc|Animal)", colnames(tJhou_Runway), value = T) %>% 
  grep('\\d+(?!Reversals)$', . , value = T, perl=T) 
tJhou_Runway_nonreverswide <- tJhou_Runway[, tJhou_Runway_vars_nonrevers, with=FALSE]
names(tJhou_Runway_nonreverswide) <- ifelse(str_count(tJhou_Runway_vars_nonrevers, "\\d")==1, gsub(" (\\d)", "0\\1", tJhou_Runway_vars_nonrevers), gsub(" ", "", tJhou_Runway_vars_nonrevers))
tJhou_Runway_nonreverswide[, animalid := names(Jhou_Runway)[-1]] # remove animal id element but retain the animal ids for the data


tJhou_Runway_vars_reversals <- grep("^(Gender|Habituation \\d|Coc|Animal)", colnames(tJhou_Runway), value = T) %>% 
  grep('\\d+(?=Reversals)', . , value = T, perl=T)
tJhou_Runway_reverswide <- tJhou_Runway[, tJhou_Runway_vars_reversals, with=FALSE]
names(tJhou_Runway_reverswide) <- ifelse(str_count(tJhou_Runway_vars_reversals, "\\d")==1, gsub(" (\\d)", "0\\1", tJhou_Runway_vars_reversals), gsub(" ", "", tJhou_Runway_vars_reversals))
tJhou_Runway_reverswide[, animalid := names(Jhou_Runway)[-1]] # remove animal id element but retain the animal ids for the data


# convert wide to long formats for both reversals and elapsed time datasets 

tJhou_Runway_nonreverslong <- gather(tJhou_Runway_nonreverswide, session, elapsedtime, `Habituation01`:`Cocaine12`, factor_key=F) 
tJhou_Runway_reverslong <- gather(tJhou_Runway_reverswide, reversalsession, numreversals, `Habituation01Reversals`:`Cocaine12Reversals`, factor_key=F) 

tJhou_Runway_data <- cbind(tJhou_Runway_nonreverslong, tJhou_Runway_reverslong %>% select(-animalid)) %>% 
  mutate_all() ## XX 
  arrange(animalid, session) # all ids are represented 14 times

# extract the notes (create specific comments table)
tJhou_Runway_notes <- tJhou_Runway[, "notes", with = FALSE]
tJhou_Runway_notes[, animalid := names(Jhou_Runway)[-1]]

################################
########### LOCOMOTOR ##########
################################

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



# get the total counts 
# get the average counts 
# assign the session information

################################
#### PROGRESSIVE PUNISHMENT ####
################################

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
Jhou_Excel_ProgressivePunishment_formats$local$font$bold[Jhou_Excel_ProgressivePunishment_formats_cellbycell$local_format_id]

# get the information about the red point; get the hexademical string
redexample <- Jhou_Excel_ProgressivePunishment_formats_cellbycell %>% dplyr::filter(row == 351, col == 1)
wantedhexa <- Jhou_Excel_ProgressivePunishment_formats$local$font$color$rgb[redexample$local_format_id]
wantedhexa_indices <- which(Jhou_Excel_ProgressivePunishment_formats$local$font$color$rgb == wantedhexa)
redrows <- Jhou_Excel_ProgressivePunishment_formats_cellbycell[Jhou_Excel_ProgressivePunishment_formats_cellbycell$local_format_id %in% wantedhexa_indices, ] %>% 
  dplyr::filter(!is.na(numeric), grepl("^A",address))
# redrows <- # 
# To look up the local formatting of a given cell
# my_cells$Sheet1[1, "local_format_id"]`
# my_formats$local$font$size[local_format_id]

