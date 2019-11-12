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
# allexperimentfiles[!grepl("_\\d+", allexperimentfiles$filename),]
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




allexperimentdatedobandbroadcohorts <- allexperimentfiles %>% 
  dplyr::mutate(subdirectoryid_edit = gsub("u", "U", subdirectoryid)) %>% 
  left_join(., rfidandid[,c("shipmentcohort", "labanimalid")], by = c("subdirectoryid_edit" = "labanimalid") ) %>%
  # dplyr::filter(subdirectoryid == labanimalid) %>%
  extractfromfilename() %>%
  left_join(., rfidandid[,c("dob", "labanimalid")], by = "labanimalid") %>% 
  dplyr::mutate(experimentage = as.numeric(difftime(date, dob, units = "days")),
                broadcohorts = round(shipmentcohort, 0)) %>% 
  dplyr::group_by(subdirectoryid_edit, experiment) %>%
  plyr::arrange(date) %>%
  do(head(., n=1)) %>%
  select(subdirectoryid_edit, shipmentcohort, broadcohorts, experimentage) %>% # add to prevent all NA column
  spread(., experiment, experimentage) 

allexperimentdayofexperiments <- allexperimentdatedobandbroadcohorts %>%
  rowwise() %>% 
  mutate_at(vars(delayed_punishmentfiles:progratiofiles), .funs = list(days = ~ . - runwayfiles + 1)) %>%
  mutate(runwayfiles_days = runwayfiles - runwayfiles + 1)

# why are there so many na's in runway
norunwayids <- allexperimentdayofexperiments[which(is.na(allexperimentdayofexperiments$runwayfiles_days)),]$subdirectoryid_edit

allexperimentfiles %>% 
  dplyr::mutate(subdirectoryid_edit = gsub("u", "U", subdirectoryid)) %>% 
  dplyr::filter(subdirectoryid_edit %in% norunwayids , experiment == "runwayfiles" ) %>% # cannot be found -- no runway data?!  
  extractfromfilename() %>%
  dplyr::group_by(subdirectoryid_edit) %>%
  plyr::arrange(date) %>%
  do(head(., n=1)) %>%
  dplyr::select(filename, experiment, subdirectoryid_edit)


experimentfilesbyid <- split(allexperimentfiles, f = allexperimentfiles$subdirectoryid)
experimentfilesbyexperiment <- split(allexperimentfiles, f = allexperimentfiles$experiment)
experimentfilesbyexperiment <- lapply(experimentfilesbyexperiment, function(x) {
  unique(x$subdirectoryid)
})
lapply(experimentfilesbyexperiment, length)
setdiff(experimentfilesbyexperiment$runwayfiles, experimentfilesbyexperiment$delayed_punishmentfiles)

allexperimentwithdateanddob %>% 
  dplyr::filter(subdirectoryid_edit %in% animalswithwrongorder$subdirectoryid_edit) %>% 
  dplyr::group_by(subdirectoryid_edit, experiment) %>%
  plyr::arrange(date) %>%
  do(head(., n=1)) %>%
  select(subdirectoryid_edit, date, shipmentcohort, experiment) %>%
  spread(., experiment, date) %>%
  rename("labanimalid" = "subdirectoryid_edit") %>% 
  select(labanimalid, shipmentcohort, runwayfiles, progpunfiles,delayed_punishmentfiles, progratiofiles, locomotorfiles) %>%
  dplyr::arrange(labanimalid)



ggplot(allexperimentwithdateanddob, aes(experiment,date)) + 
  geom_point(aes(color = experiment)) + 
  facet_grid(. ~ shipmentcohort) + 
  labs(title = "experiment dates by cohort, extracted from raw file filename dates") + 
  theme(axis.text.x=element_blank()) + 
  scale_y_datetime(date_breaks = "25 day")


# create another variable that categorizes the shipment cohort more broadly
ggplot(allexperimentwithdateanddob, aes(experiment,date,group=broadcohorts)) + 
  geom_point(aes(color = shipmentcohort)) + 
  facet_grid(. ~ broadcohorts) + 
  labs(title = "experiment dates by cohort, extracted from raw file filename dates") + 
  theme(axis.text.x=element_blank()) + 
  scale_y_datetime(date_breaks = "25 day")


ggplot(allexperimentwithdateanddob %>% dplyr::filter(shipmentcohort < 5), aes(experiment, experimentage, group = subdirectoryid_edit)) + 
  geom_path(aes(color = experiment)) + geom_point(aes(color = experiment)) +
  facet_grid(. ~ shipmentcohort) + 
  labs(title = "experiment dates by cohort, extracted from raw file filename dates") + 
  theme(axis.text.x=element_blank())
# + 
#   scale_y_datetime(date_breaks = "25 day")


allexperimentwithdateanddob %>% 
  group_by(subdirectoryid_edit,experiment) %>%
  top_n(1) %>% 
  select(experiment, date) %>%
  unique() %>% 
  group_by(subdirectoryid_edit) %>% 
  dplyr::mutate(exporder = row_number()) %>% 
  group_by(subdirectoryid_edit) %>% 
  dplyr::filter(max(exporder) == 5) %>% 
  dplyr::filter(#experiment == "runwayfiles" & exporder != 1,
                experiment == "progpunfiles" & exporder != 2,
                #experiment == "delayed_punishmentfiles" & exporder != 3,
                #experiment == "progratiofiles" & exporder != 4, 
                experiment == "locomotorfiles" & exporder != 5) %>%
  select(subdirectoryid_edit, experiment)
  View()
  
animalswithwrongorder <-  allexperimentwithdateanddob %>%
    group_by(subdirectoryid_edit,experiment) %>%
    top_n(1) %>%
    select(experiment, date, shipmentcohort) %>%
    unique() %>%
    group_by(subdirectoryid_edit) %>%
    dplyr::mutate(exporder = row_number()) %>%
    group_by(subdirectoryid_edit) %>%
    arrange(date) %>%
    dplyr::filter(max(exporder) == 5) %>%
    dplyr::filter(experiment == "locomotorfiles" & exporder != 5 |
                  experiment == "progratiofiles" & exporder != 4 |
                  experiment == "delayed_punishmentfiles" & exporder != 3 |
                  experiment == "progpunfiles" & exporder != 2 |
                  experiment == "runwayfiles" & exporder != 1  ) %>%
    select(subdirectoryid_edit) %>% 
  unique()

# send to palmer team
labanimaliddatesandexps_wrongorder <- allexperimentwithdateanddob %>% 
  dplyr::filter(subdirectoryid_edit %in% animalswithwrongorder$subdirectoryid_edit) %>% 
  dplyr::group_by(subdirectoryid_edit, experiment) %>%
  plyr::arrange(date) %>%
  do(head(., n=1)) %>%
  select(subdirectoryid_edit, date, shipmentcohort, experiment) %>%
  spread(., experiment, date) %>%
  rename("labanimalid" = "subdirectoryid_edit") %>% 
  select(labanimalid, shipmentcohort, runwayfiles, progpunfiles,delayed_punishmentfiles, progratiofiles, locomotorfiles) %>%
  dplyr::arrange(labanimalid)
# %>%
    # select(subdirectoryid_edit, experiment, exporder)
  #View()
  


allexperimentwithdateanddob %>% 
  group_by(subdirectoryid_edit,experiment) %>%
  top_n(1) %>% 
  select(experiment, date) %>%
  unique() %>% 
  group_by(subdirectoryid_edit) %>% 
  dplyr::mutate(exporder = row_number()) %>% 
  group_by(subdirectoryid_edit) %>% 
  dplyr::filter(max(exporder) == 5) %>% 
  group_by(experiment, exporder) %>% 
  count()
  View()


experiment == "runwayfiles" && exporder != 1 |
  experiment == "progpunfiles" && exporder != 2 |
  
allexperimentwithdateanddob %>% 
  dplyr::filter(shipmentcohort == 11.3)

  # group_by(experiment, shipmentcohort) %>% 
  # top_n(n = 3) %>% 
  # select(labanimalid)  
  

# Extract data for these rats across all experiments and merge
lapply(runwayfiles_clean[2500:2550], readrunway) %>% rbindlist(fill = T) 

# QC experiment date 

# Calculate and evaluate the age at experiment 

# Plot the dates, color by experiment



##############

lapply(experimentfilesbyexperiment, length)

# because the id's represented in each experiment differs so much, i want to check them against the master table
# create a master sheet of id's from wfu, join to the excel sheet to get the u numbers, 
WFU_Jhou_findidindropbox <- WFU_Jhou_test_df %>% 
  left_join(., rfidandid[,c("labanimalid", "wakeforestid") ], by = c("labanimalnumber"= "wakeforestid")) %>%
  mutate(runway = ifelse(labanimalid %in% experimentfilesbyexperiment$runwayfiles, 1,0), 
         progratio = ifelse(labanimalid %in% experimentfilesbyexperiment$progratiofiles, 1, 0), 
         progpun = ifelse(labanimalid %in% experimentfilesbyexperiment$progpunfiles, 1, 0), 
         locomotor = ifelse(labanimalid %in% experimentfilesbyexperiment$locomotorfiles,1, 0), 
         delayedpun = ifelse(labanimalid %in% experimentfilesbyexperiment$delayed_punishmentfiles, 1, 0)) 
sapply(subset(WFU_Jhou_findidindropbox, select = runway:delayedpun), sum)
