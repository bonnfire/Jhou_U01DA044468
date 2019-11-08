## Create subset of a handful of rats from each cohort to plot their dates 

## Check for consistency in time in files vs filename
## Calculate the experiment age 
## Check for the protocol changes? Why younger rats in the later cohorts? 


# Create the dataframe with the assigned cohorts 

# Select the cohorts and the rats 
allexperimentfiles <- lapply(list(runwayfiles_clean, 
locomotorfiles_clean,
progpunfiles_clean,
progratiofiles_clean,
delayed_punishmentfiles_clean), function(x){
  x <- as.data.frame(matrix(x))
  x$V1 <- as.character(x$V1)
  return(x)})

names(allexperimentfiles) =  gsub("_clean", "", c("runwayfiles_clean", 
                                    "locomotorfiles_clean",
                                    "progpunfiles_clean",
                                    "progratiofiles_clean",
                                    "delayed_punishmentfiles_clean"))

allexperimentfiles %<>% rbindlist(idcol = "experiment")

allexperimentfiles %<>%
  rename("filename" = "V1") 
allexperimentfiles[!grepl("_\\d+", allexperimentfiles$filename),]
allexperimentfiles[which(filename == "./U214/2019-0328-1327__DELAYED PUNISHMENT.txt"),]$filename <- "./U214/2019-0328-1327_214_DELAYED PUNISHMENT.txt"
allexperimentfiles$subdirectoryid = str_extract(allexperimentfiles$filename, regex("U\\d+", ignore_case = T))
allexperimentfiles$labanimalid = str_extract(allexperimentfiles$filename, "_\\d+") %>% gsub("_", "U", .)

allexperimentfiles %>%
  dplyr::group_by(experiment) %>%
  select(labanimalid) %>% 
  unique() %>%
  summarize(n = n()) # not consistent throughout

allexperimentfiles %>% 
  dplyr::filter(subdirectoryid != labanimalid) 

# allexperimentfiles %>% 
#  dplyr::filter(subdirectoryid != labanimalid) 

allexperimentfiles %>% 
  left_join(., rfidandid[,c("shipmentcohort", "labanimalid")], by = "labanimalid") %>%
  dplyr::filter(subdirectoryid == labanimalid) %>%
  group_by(experiment, shipmentcohort) %>% 
  top_n(n = 3) %>% 
  select(labanimalid)  
  

# Extract data for these rats across all experiments and merge
lapply(runwayfiles_clean[2500:2550], readrunway) %>% rbindlist(fill = T) 

# QC experiment date 

# Calculate and evaluate the age at experiment 

# Plot the dates, color by experiment