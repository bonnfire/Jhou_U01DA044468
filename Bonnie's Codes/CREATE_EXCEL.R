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

### RUNWAY
Jhou_Runway <- Jhou_Excel[["Runway"]] %>% as.data.table
Jhou_Runway_test <- Jhou_Runway[1:16, ]
Jhou_Runway_test <- rbindlist(list(Jhou_Runway_test, as.list(names(Jhou_Runway))), fill=FALSE) # retain the column names before transposing
