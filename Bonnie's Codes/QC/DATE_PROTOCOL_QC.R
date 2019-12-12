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
                                  delayed_punishmentfiles_clean, 
                                  lever_trainingfiles_clean), function(x){
  x <- as.data.frame(matrix(x))
  x$V1 <- as.character(x$V1)
  names(x) <- "filename"
  return(x)})

names(allexperimentfiles) =  gsub("_clean", "", c("runwayfiles_clean", 
                                    "locomotorfiles_clean",
                                    "progpunfiles_clean",
                                    "progratiofiles_clean",
                                    "delayed_punishmentfiles_clean",
                                    "lever_trainingfiles_clean"))

allexperimentfiles %<>% rbindlist(idcol = "experiment")

# allexperimentfiles[!grepl("_\\d+", allexperimentfiles$filename),]
#allexperimentfiles[which(filename == "./U214/2019-0328-1327__DELAYED PUNISHMENT.txt"),]$filename <- "./U214/2019-0328-1327_214_DELAYED PUNISHMENT.txt"
# qc if subdirectory is the same as labanimalid
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


allexperiment_meta <- allexperimentfiles %>% 
  dplyr::mutate(subdirectoryid = toupper(subdirectoryid)) %>% # checked that all end with numbers 
  left_join(., Jhou_SummaryAll[, c("labanimalid", "wakeforestid", "shipmentcohort")], by = c("subdirectoryid" = "labanimalid")) %>% 
  left_join(., WFU_Jhou_test_df[,c("cohort", "dob","labanimalid")], by = c("wakeforestid" = "labanimalid") ) %>%
  mutate(date = gsub("[-]([[:digit:]]{2})([[:digit:]]{2})", "-\\1-\\2", stringr::str_extract(filename, "[[:digit:]]{4}[-][[:digit:]]{4}")),
         date =  as.POSIXct(date, tz = "UTC"),
         time = stringr::str_extract(filename, "[[:digit:]]{4}(?=_)"),
         time = gsub('([[:digit:]]{2})([[:digit:]]{2})', '\\1:\\2', time),
         shipmentcohort = as.numeric(shipmentcohort) %>% round(3),
         experimentage = as.numeric(difftime(date, dob, units = "days"))) %>% 
  rename("protocolcohort" = "shipmentcohort", 
         "shipmentcohort" = "cohort")

allexperiment_meta %>% dplyr::filter(is.na(dob))

  # dplyr::group_by(subdirectoryid_edit, experiment) %>%
  # plyr::arrange(date) %>%
  # do(head(., n=1)) %>%
  # select(subdirectoryid_edit, shipmentcohort, broadcohorts, experimentage) %>% 
  # ungroup() 
  # %>%
  # mutate(experiment = factor(experiment, levels=c("runwayfiles", "progpunfiles", "delayed_punishmentfiles", "progratiofiles", "locomotorfiles"))) %>% 
  # dplyr::arrange(experiment)


## CREATE GRAPH DATASET WITH CORRECT ORDER OF X AXES

#### From Alen (12/5)

# For the order of behaviors, there are some variations especially in the order of locomotor . 
# For cohorts 1.1 - 3.4  = Runway --> Food deprivation --> Lever Training --> Progressive Punishment --> Progressive Ratio --> Locomotor --> Delayed Punishment.
# For cohort 3.5 = Runway --> Food deprivation --> Lever Training --> Progressive Punishment --> Progressive Ratio --> Delayed Punishment --> Locomotor.
# For cohorts 4.1 - 8.2 = Runway --> Food deprivation --> Lever Training --> Progressive Punishment --> Progressive Ratio --> Locomotor --> Delayed Punishment.
# For cohort 8.3 = Runway --> Food deprivation -->  Locomotor --> Lever Training --> Progressive Punishment --> Progressive Ratio --> Delayed Punishment --> Locomotor.
# For cohort 9.1 =  Runway --> Food deprivation -->  Locomotor --> Lever Training --> Progressive Punishment --> Progressive Ratio --> Locomotor --> Delayed Punishment. 
# For cohorts 9.2 - current = Runway --> Locomotor x2 --> Food deprivation --> Lever Training --> Progressive Punishment --> Progressive Ratio --> Locomotor x2 --> Delayed Punishment. 


allexperiment_meta_graph <- allexperiment_meta
allexperiment_meta_graph$experiment <- with(allexperiment_meta_graph,paste(experiment,protocolcohort,sep="_"))

protocolcohortsinorder <- allexperiment_meta_graph$protocolcohort %>% unique() %>% sort %>% as.character()

cohort1_3.4_order <- paste0(c("runwayfiles", "lever_trainingfiles", "progpunfiles", "progratiofiles", "locomotorfiles","delayed_punishmentfiles"), "_",  rep(protocolcohortsinorder[1:8], each = 6))
cohort3.5_order <- paste0(c("runwayfiles", "lever_trainingfiles", "progpunfiles", "progratiofiles", "delayed_punishmentfiles", "locomotorfiles"), "_",  rep(protocolcohortsinorder[9], each = 6))
cohort4.1_8.2_order <- paste0(c("runwayfiles", "lever_trainingfiles", "progpunfiles", "progratiofiles","locomotorfiles", "delayed_punishmentfiles"), "_",  rep(protocolcohortsinorder[10:23], each = 6))
cohort8.3_order <- paste0(c("runwayfiles", "locomotorfiles", "lever_trainingfiles", "progpunfiles", "progratiofiles",  "delayed_punishmentfiles", "locomotorfiles2"), "_", rep(protocolcohortsinorder[24], each = 6))
cohort9.1_order <- paste0(c("runwayfiles",  "locomotorfiles", "lever_trainingfiles", "progpunfiles", "progratiofiles", "locomotorfiles2", "delayed_punishmentfiles"), "_", rep(protocolcohortsinorder[25], each = 6))
cohort9.2_12.2order <- paste0(c("runwayfiles", "locomotorfiles1a", "locomotorfiles1b", "lever_trainingfiles", "progpunfiles", "progratiofiles", "locomotorfiles2a", "locomotorfiles2b", "delayed_punishmentfiles"), "_", rep(protocolcohortsinorder[26:33], each = 6)) #temporarily adding :33 to create 155 levels

allexperiment_meta_graph$experiment <- factor(allexperiment_meta_graph$experiment,
                                                         levels=c(cohort1_3.4_order,
                                                                  cohort3.5_order,
                                                                  cohort4.1_8.2_order,
                                                                  cohort8.3_order,
                                                                  cohort9.1_order,
                                                                  cohort9.2_12.2order))
#### test ggplot code 
# filter some cohorts (1 through 8.2 to test if the code will change for 8.2)
test_graph <- allexperiment_meta_graph %>% 
  dplyr::filter(shipmentcohort %in% protocolcohortsinorder[1:2])
cohort1_2.1_order <- paste0(c("runwayfiles",  "progpunfiles","delayed_punishmentfiles", "progratiofiles", "locomotorfiles"), "_", rep(shipmentcohortsinorder[1:2], each = 5))
test_graph$experiment <- factor(test_graph$experiment, levels=cohort1_2.1_order)

test_graph$experiment <- factor(test_graph$experiment, levels=c(cohort1_8.1_order,cohort8.2_order))

allexperiment_meta_graph %>% 
  dplyr::filter(protocolcohort %in% protocolcohortsinorder[1:2]) %>% 
ggplot(aes(experiment,experimentage, group = subdirectoryid)) + 
  geom_line() +
  geom_point() +
  facet_grid(. ~ protocolcohort, scales = "free_x") + 
  labs(title = "experiment age by cohort") + 
  # scale_x_discrete(labels = gsub(".\\d+", "", test_graph$experiment)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test_graph2 <- allexperimentdatedobandbroadcohorts_graph %>% 
  dplyr::filter(shipmentcohort %in% shipmentcohortsinorder[24:33])
test_graph$experiment <- factor(test_graph$experiment, levels=c(cohort8.3_9.2_order,cohort10.1_order))

# where are the delayed punishment files 10.2; runway 11.1; runway 11.2; delayed, progratio, and runway 11.3; everything but locomotor for 12.1

# test_graph2 %>% group_by(experiment, shipmentcohort) %>% count()


ggplot(test_graph, aes(experiment,experimentage, group = subdirectoryid_edit)) + 
  geom_line() +
  geom_point() +
  facet_grid(. ~ shipmentcohort, scales = "free") + 
  labs(title = "experiment age by cohort") + 
  scale_x_discrete(labels = gsub(".\\d+", "", test_graph$experiment)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
#   new_x <- paste(x, within, sep = sep)
#   stats::reorder(new_x, by, FUN = fun)
# }


#%>% # add to prevent all NA column
  #spread(., experiment, experimentage) 

# allexperimentdayofexperiments <- allexperimentdatedobandbroadcohorts %>%
#   rowwise() %>% 
#   mutate_at(vars(delayed_punishmentfiles:progratiofiles), .funs = list(days = ~ . - runwayfiles)) %>%
#   mutate(runwayfiles_days = runwayfiles - runwayfiles + 1)


## CREATE GRAPH DATASET FACETED BY EXPERIMENT, FILL BY COHORT COLOR, HISTOGRAM OF AGES, EXPECTING NORMAL DISTRIBUTION
allexperimentdatedobandbroadcohorts %>% 
  mutate(shipmentcohort = factor(shipmentcohort, levels = as.numeric(allexperimentdatedobandbroadcohorts $shipmentcohort) %>% sort() %>% unique())) %>%
ggplot(aes(shipmentcohort, experimentage)) + 
  geom_boxplot() +
  facet_grid(. ~ experiment) + 
  labs(title = "experiment age by cohort by experiment") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

## CREATE GRAPH DATASET FACETED BY SHIPMENTCOHORT, EXPERIMENT AGE BOXPLOTS, EXPECTING SMALL SPREADS SINCE EACH SHIPMENTCOHORT SHOULD BE GOING INTO EXPERIMENTS AT AROUND THE SAME TIME
allexperimentdatedobandbroadcohorts %>% 
  mutate(shipmentcohort = factor(shipmentcohort, levels = as.numeric(allexperimentdatedobandbroadcohorts $shipmentcohort) %>% sort() %>% unique())) %>%
  ggplot(aes(experiment, experimentage)) + 
  geom_boxplot() +
  facet_grid(. ~ shipmentcohort) + 
  labs(title = "experiment age by cohort by experiment") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


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

# plotting the order of the exps based on age at exp

ggplot(allexperimentdatedobandbroadcohorts, aes(experiment,experimentage, group = subdirectoryid_edit)) + 
  geom_path() +
  geom_point() +
  facet_grid(. ~ broadcohorts, scales = "free") + 
  labs(title = "experiment age by cohort") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## add color for subcohorts
ggplot(allexperimentdatedobandbroadcohorts, aes(experiment,experimentage, group = subdirectoryid_edit)) + 
  geom_path(aes(color = shipmentcohort)) +
  geom_point(aes(color = shipmentcohort)) +
  facet_grid(. ~ broadcohorts) + 
  labs(title = "experiment age by cohort") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








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

# because the id's represented in each experiment differs so much, i want to check them against the 
master table
# create a master sheet of id's from wfu, join to the excel sheet to get the u numbers, 
WFU_Jhou_findidindropbox <- WFU_Jhou_test_df %>% 
  left_join(., rfidandid[,c("labanimalid", "wakeforestid") ], by = c("labanimalnumber"= "wakeforestid")) %>%
  mutate(runway = ifelse(labanimalid %in% experimentfilesbyexperiment$runwayfiles, 1,0), 
         progratio = ifelse(labanimalid %in% experimentfilesbyexperiment$progratiofiles, 1, 0), 
         progpun = ifelse(labanimalid %in% experimentfilesbyexperiment$progpunfiles, 1, 0), 
         locomotor = ifelse(labanimalid %in% experimentfilesbyexperiment$locomotorfiles,1, 0), 
         delayedpun = ifelse(labanimalid %in% experimentfilesbyexperiment$delayed_punishmentfiles, 1, 0)) 
# using WFU_Jhou_findidindropbox$rfid %>% unique() %>% length(), we are expecting 472, but the files represent only 388-439 animals

sapply(subset(WFU_Jhou_findidindropbox, select = runway:delayedpun), sum)






