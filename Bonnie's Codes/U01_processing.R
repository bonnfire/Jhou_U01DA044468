# LOAD AND CLEAN TOM JHOU'S DATA

setwd("~/Public/U01")

library(readxl)
library(dplyr)
library(tibble)
library(RPostgreSQL)
library(purrr)

#################################################################
#### Second version of data (8/26 Noon)
path2 <- "/home/bonnie/Public/U01/U01 Behavioral data_readonly_2.xlsx"

U01_data_original2 <- lapply(excel_sheets(path2), read_excel, path = path2)
names(U01_data_original2) <- excel_sheets(path2)
Runway_df <- (U01_data_original2[["Runway"]])[1:16,]
head(Runway_df)

head(U01_data_original2[["Summary all"]])[1:5, 1:10]

# Variable name designation 
U01_data_processed2 <- lapply(seq_along(U01_data_original2), function(i) {
  names(U01_data_original2[[i]]) <- gsub(" ", "_", names(U01_data_original2[[i]]))
  U01_data_original2[[i]]})
names(U01_data_processed2) <- paste0(gsub(" ", "_", excel_sheets(path2)), "_df")

# Extract and clean Summary All
Summary_all_df <- U01_data_processed2[["Summary_all_df"]]
Summary_all_df_processed <- Summary_all_df
colnames(Summary_all_df_processed) <- as.character(unlist(Summary_all_df_processed[1,]))
Summary_all_df_processed <- Summary_all_df_processed[-1, ]

# Extract and clean Runway
Runway_df <- U01_data_processed2[["Runway_df"]][1:16,]
Runway_df_processed <- t(Runway_df) %>% as.data.frame()
colnames(Runway_df_processed) <- as.character(unlist(Runway_df_processed[1,]))
Runway_df_processed <- Runway_df_processed[-1, ]
Runway_df_processed <- rownames_to_column(Runway_df_processed, var = "Jhou lab ID") # Create ID column
## QC: Runway Sex vs Summary All Sex
U01_Sex_QC <- left_join(Runway_df_processed, Summary_all_df_processed, by = "Jhou lab ID")
which(Summary_all_df_processed

# #################################################################
# #### First version of data (8/26 Morning)
# path <- "/home/bonnie/Public/U01/U01 Behavioral data_readonly.xlsx"
# 
# # Skip first row and read in XL worksheets 
# U01_data_original <- lapply(path, function(x) {
#   sheets <- excel_sheets(x)
#   dfs <- lapply(sheets, function(y) {
#     read_excel(x, sheet = y, skip = 1)
#   })
#   names(dfs) <- sheets
#   dfs 
# })[[1]]
# 
# # To see a list at a time
# str(U01_data_original[[1]], list.len = 15)
# 
# # Variable name designation 
# U01_data_processed <- lapply(seq_along(U01_data_original), function(i) {
#   names(U01_data_original[[i]]) <- gsub(" ", "_", names(U01_data_original[[i]]))
#   U01_data_original[[i]]})
# 
# names(U01_data_processed) <- paste0(gsub(" ", "_", names(U01_data_original)), "_df")
# 
# # Summary of "U01 all"
# U01_data_processed[["Summary_all_df"]] %>% summary()
# U01_data_processed[["Summary_all_df"]] $"16_digit_ID" %>% unique() %>% length() #425 unique 16 digit ID and 1626 rows 
# ##########  QUESTIONS ############
# ### Why are all of those NA's there? 
# ### Are these the important variables
# 
# # Summary of Runway 
# U01_data_processed[["Runway_df"]] %>% summary() 
# ## Very messy?? Gender column is Cocaine + # 
# U01_data_processed[["runway_males_vs_females_df" ]] %>% summary() 
# 
# # Summary of Confidental Place Preference
# 
# # Summary of Progressive Ratio/Punishment Ratio