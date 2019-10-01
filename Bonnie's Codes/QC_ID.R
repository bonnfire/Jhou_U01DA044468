rm(list=ls())

setwd("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01")

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(data.table)

## helpful ## 
list.files()
length(IDs[!is.na(IDs)])
####  


### obtain a table of all text files

### test one file 
filetest <- read.table("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/tom_jhou_u01/Locomotor/U1/2018-0727-1337_1_LOCOMOTOR_BASIC.txt", header = F, fill = T)
filetesttab <- filetest[7:36, 1:2] %>% 
  mutate("labanimalid" = ifelse(filetest[5,6] %>% as.character() %>% as.numeric() < 10, paste0("U0", filetest[5,6] %>% as.character() %>% as.numeric()), paste0("U", filetest[5,6] %>% as.numeric(as.character()))))
names(filetesttab) <- c("minute", "counts", "labanimalid")
filetesttab <- filetest[7:36, 1:2]

### 

setwd(paste0("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/", experiment_name,"/"))

runway <- NA 
idqc <- function(experiment_name){
  setwd(paste0("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/", experiment_name,"/"))
  files <- list.files(path=".", pattern=".txt", full.names=TRUE, recursive=TRUE) 
  read_ids<-function(x){
    data = fread(paste0("sed -n 8p ","'",x,"'"), header=F, fill=T, sep=":", showProgress = F)  
    data$id<-x
    return(data)
  }
  all<-lapply(files, read_ids)
  all_test <- rbindlist(all, use.names = T, fill = T)
  all_test2 <- all_test %>% select(-V1)
  colnames(all_test2) <- c("generatedid", "filename")
  all_test2$idinfile <- gsub("(.*_)(\\d+)_.+", "\\2", all_test2$filename) 
  all_test2$idindir <- gsub("(.*U)(\\d+)/.+", "\\2", all_test2$filename) 
  subset(all_test2, generatedid!= idindir | generatedid!=idinfile | idindir!=idinfile)
}

idqc("Locomotor") #done
idqc("Progressive punishment") #done
idqc("Progressive ratio") #done
runway <- idqc("Runway") # XX 2 col name to 5 col
idqc("Delayed punishment") # done
idqc("Lever training") # XX line 8 is a bigger problem




  
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder")  
mastersheet_path = "U01 Master sheet_readonly.xlsx"
master_sheetnames <- excel_sheets(mastersheet_path)
master_listdf <- lapply(excel_sheets(mastersheet_path), read_excel, path = mastersheet_path)

locomotorqc <-f 

####### Locomotor ##############

## Master file preparation
Locomotor <- master_listdf["Locomotor"] %>% as.data.frame()
# Locomotor_sans_NA_cols <- Locomotor[!map_lgl(Locomotor, ~ all(is.na(.)))] # remove all "completely na" columns
# Locomotor_remake <- data.frame("animallabid" = grep("U\\d", Locomotor_sans_NA_cols$Locomotor.Program..Locomotor.Basic., value = T))
# namevector <- c(paste0("bincounts", as.character(1:30)), "notes", "bincountsearly", "bincountslate")
# Locomotor_remake[ , namevector] <- NA

counts <- Locomotor_sans_NA_cols[which(grepl("^Binned", Locomotor_sans_NA_cols$Locomotor.Program..Locomotor.Basic.)),1:34]
counts_test <- counts
rownames(counts_test) <- seq(length=nrow(counts_test)) #reset rownumbering
names(counts_test) <- c("animallabid", paste0("bincounts", as.character(1:30)), "notes", "bincountsearly", "bincountslate")

# make into while and for loop

## COME BACK TO THIS LATER [PICK UP]
names(counts_test) <- c("value", paste0("bincounts", as.character(1:30)), "notes", "bincountsearly", "bincountslate")
counts_test$animallabid <- NA 
i <- 1
j <- 1
repeat {
  counts_test$animallabid[i] <- unames[j]
  i = i + 1
  if (grepl("Counts$", counts_test$value[i]) | grepl("Counts[1][ab]?$", counts_test$value[i])){
    j = j + 1
  }
}


counts_test$animallabid[1:296] <- paste0(unames[1:296], "_", counts_test$animallabid[1:296])
counts_test$animallabid[297:300] <- paste0(rep(c(unames[297:298]), each = 2), "_", counts_test$animallabid[297:300])
counts_test$animallabid[301:302] <- paste0(unames[299:300], "_", counts_test$animallabid[301:302])
counts_test$animallabid[303:338] <- paste0(rep(c(unames[301:318]), each = 2), "_", counts_test$animallabid[303:338])
counts_test$animallabid[339:343] <- paste0(rep(c(unames[319]), times = 5), "_", counts_test$animallabid[339:343])
counts_test$animallabid[344:353] <- paste0(rep(c(unames[320:324]), each = 2), "_", counts_test$animallabid[344:353])
counts_test$animallabid[354:361] <- paste0(rep(c(unames[325:326]), each = 4), "_", counts_test$animallabid[354:361])
counts_test$animallabid[362:365] <- paste0(rep(c(unames[327:328]), each = 2), "_", counts_test$animallabid[362:365])
counts_test$animallabid[366:373] <- paste0(rep(c(unames[329:330]), each = 4), "_", counts_test$animallabid[366:373])
counts_test$animallabid[374:377] <- paste0(rep(c(unames[331:332]), each = 2), "_", counts_test$animallabid[374:377])
counts_test$animallabid[378:385] <- paste0(rep(c(unames[333:334]), each = 4), "_", counts_test$animallabid[378:385])
counts_test$animallabid[386:387] <- paste0(rep(c(unames[335]), times = 2), "_", counts_test$animallabid[386:387])
counts_test$animallabid[388:415] <- paste0(rep(c(unames[336:342]), each = 4), "_", counts_test$animallabid[388:415])
counts_test$animallabid[416:417] <- paste0(rep(c(unames[343]), times = 2), "_", counts_test$animallabid[416:417])
counts_test$animallabid[418:422] <- paste0(rep(c(unames[344]), times = 5), "_", counts_test$animallabid[418:422])
counts_test$animallabid[423:630] <- paste0(rep(c(unames[345:396]), each = 4), "_", counts_test$animallabid[423:630])

counts_test2 <- counts_test %>% 
  separate(animallabid, c("animallabid","value"), sep = "_")

# it really worked 

## for above code: used grepl("Counts$",counts$animallabid) 
# Locomotor_sans_NA_cols$Locomotor.Program..Locomotor.Basic.[which(grepl("1a$", Locomotor_sans_NA_cols$Locomotor.Program..Locomotor.Basic.))-1] + U339 and 368 (STDEV bc they have Counts1, 1a, 1b, etc.)


## Text file preparation



####### Progressive Punishment ##############


## Master file preparation

ProgressivePunishment <- master_listdf["Progressive Punishment"] %>% as.data.frame()
shocks <- ProgressivePunishment[which(grepl("^\\d", as.numeric(ProgressivePunishment[,1]))), c(1, 3, 7:11)]
names(shocks) <- c("session", "date", "lastcompletedintensity", "lastattempedintensity","numberoftrialsatlastshockintensity","attemptedactivepresses","attemptedinactivepresses")
# shocks$session <- factor(shocks$session, ordered = T)
shocks_test <- shocks
shocks_test$session <- as.numeric(shocks_test$session)
shocks_test$date <- openxlsx::convertToDateTime(shocks$date)
rownames(shocks_test) <- seq(length=nrow(shocks_test)) #reset rownumbering
shocksunames <- grep("^U", ProgressivePunishment$Progressive.Punishment....1, value = T)
shocks_test$animallabid <- NA

i <- 1
j <- 1
repeat {
  shocks_test$animallabid[i] <- shocksunames[j]
  i = i + 1
  if (shocks_test$session[i] < shocks_test$session[i-1] & shocks_test$session[i] == 0){
    j = j + 1
  }
}


## LOOK BACK TO HT PROTOCOL FOR DEFINITION OF NUMBER OF TRIALS AT LAST SHOCK INTENSITY


txt1 <- read.table("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01DA044468_Dropbox_copy/U01 folder/Progressive punishment/U1/2018-0719-1450_1_FOOD CONFLICT.txt", blank.lines.skip = F)

sub('.*?(\\w+)\\W+\\w+\\W*?$', '\\1', string)