setwd("~/Dropbox (Palmer Lab)/U01 folder")

# Load libraries
library(tidyverse) #loads dplyr, tidyr, ggplot2, purrr, etc
require(mgsub)
library(readxl)
library(lubridate)
library(openxlsx)
library(stringr)
library(data.table)

u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 

Jhou_Excel <- u01.importxlsx("U01 Master sheet_readonly.xlsx")

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

tJhou_Runway <- cbind(tJhou_Runway_nonreverslong, tJhou_Runway_reverslong %>% select(-animalid)) %>% 
  arrange(animalid, session) # all ids are represented 14 times


