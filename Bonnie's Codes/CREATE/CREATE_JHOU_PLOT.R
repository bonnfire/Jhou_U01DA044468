### CREATE_JHOU_PLOT.R

# Runway

#####################################################
# Locomotor 
#####################################################

# TO DO: GET THE NUMBER OF LAB ANIMAL ID'S IN THE WFU DATA, ID'S IN THE RAW, AND ID'S IN THE EXCEL 

# rawfiles_locomotor_wide_graph <- rawfiles_locomotor_wide %>% 
#   mutate(shipmentcohort = trunc(as.numeric(rawfiles_locomotor_wide$shipmentcohort)) %>% as.character(),
#          shipmentcohort = factor(rawfiles_locomotor_wide_graph$shipmentcohort, levels=sort(as.numeric(unique(rawfiles_locomotor_wide_graph$shipmentcohort))), ordered=TRUE))

# trying with split data
Jhou_Locomotor_Raw_graph <- Jhou_Raw_Locomotor %>% 
  mutate(shipmentcohort = trunc(as.numeric(Jhou_Raw_Locomotor$shipmentcohort)) %>% as.character(),
         shipmentcohort = factor(Jhou_Raw_Locomotor$shipmentcohort, levels=sort(as.numeric(unique(Jhou_Raw_Locomotor$shipmentcohort))), ordered=TRUE))


Jhou_Locomotor_Excel_graph <- Jhou_Locomotor %>%  
  merge(Jhou_SummaryAll[,c("labanimalid", "rfid", "shipmentcohort", "dob")], ., by = "labanimalid") 
# %>%  # extract file information for preparation for appending to rfid
  # mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid))

locomotormeasures <- grep(pattern = "^(?=min|bin)", names(Jhou_Locomotor_Raw_graph), perl = T, value = T)
onlymins <-  grep(pattern = "^(min)", names(Jhou_Locomotor_Raw_graph), perl = T, value = T)[-31]
onlymins_excel <- paste0(onlymins, "_excel")
onlymins_raw <- paste0(onlymins, "_raw")

joinrawtoexcel <- left_join(Jhou_Locomotor_Excel_graph,subset(Jhou_Locomotor_Raw_graph, select = c("labanimalid", "session", onlymins)),  by = c("labanimalid", "session"))
names(joinrawtoexcel) <- gsub(".x", "_excel", names(joinrawtoexcel))
names(joinrawtoexcel) <- gsub(".y", "_raw", names(joinrawtoexcel))
xlim <- lapply(joinrawtoexcel[onlymins_excel], range, na.rm=T)
ylim <- lapply(joinrawtoexcel[onlymins_raw], range, na.rm=T)


pdf("jhou_locomotor_compare.pdf", onefile = T)
plot_list = list()
plot_compare_list = list()



for (i in seq_along(onlymins)){
  # 
  # plot_list[[i]] <- ggplot(rawfiles_locomotor_wide_graph, aes(x=shipmentcohort))+ 
  #   geom_boxplot(aes_string(y = locomotormeasures[i])) + 
  #   labs(title = paste0(locomotor_dd$var_graphtext[i], "_locomotor_U01_Jhou"),
  #        y = locomotor_dd$var_graphtext[i], x = "Cohort") +
  #   theme(axis.text.x = element_text(angle = 45))

  plot_compare_list[[i]] <- ggplot(joinrawtoexcel, aes_string( onlymins_excel[i], onlymins_raw[i])) + 
                                     geom_point(aes(color = shipmentcohort)) +
    # geom_text(aes_string(label=ifelse(onlymins_excel[i] == onlymins_raw[i], '', "labanimalid")),hjust=0,vjust=0) + 
    # geom_text(aes(label = labanimalid), data = joinrawtoexcel[joinrawtoexcel$labanimalid %in% excelhasbutnotraw$labanimalid,]) + # get excelhasbutnotraw from raw and excel qc
    labs(title = paste0("Comparison of ", onlymins[i], "_locomotor_U01_Jhou"),
         y = onlymins_raw[i], x = onlymins_excel[i]) + 
    scale_x_continuous(limits = xlim[[i]]) + 
    scale_y_continuous(limits = xlim[[i]]) + 
    geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)
  
  
  # geom_text(aes(CPI, HDI, label = Country), data = dat[dat$Country %in% pointsToLabel,])
  
  
  #plot_compare_list[[i]] <- ggplot(data = test, aes(x=minute1.x, y = minute1.y)) + geom_point()
  
  #  print(plot_list[[i]])
  print(plot_compare_list[[i]])
  
  
}
dev.off()


inexcelnotraw_locomotor <- anti_join(subset(Jhou_Locomotor, select = c("labanimalid", "session", onlymins)), 
          subset(Jhou_Raw_Locomotor, select = c("labanimalid", "session", onlymins)),
          by = c("labanimalid", onlymins)) ## 44 entries

rawandexcel_locomotor <- full_join(subset(Jhou_Locomotor, select = c("labanimalid", "session", onlymins)), 
                                     subset(Jhou_Raw_Locomotor, select = c("labanimalid", "session", onlymins)),
                                     by = c("labanimalid", onlymins)) %>% #ignoring the sessions for now 
  merge(Jhou_SummaryAll[,c("labanimalid", "rfid", "shipmentcohort")], ., by = "labanimalid") 


pdf("jhou_locomotorfulljoin.pdf", onefile = T)
plot_list = list()
plot_compare_list = list()



for (i in seq_along(onlymins)){
  # 
  # plot_list[[i]] <- ggplot(rawfiles_locomotor_wide_graph, aes(x=shipmentcohort))+ 
  #   geom_boxplot(aes_string(y = locomotormeasures[i])) + 
  #   labs(title = paste0(locomotor_dd$var_graphtext[i], "_locomotor_U01_Jhou"),
  #        y = locomotor_dd$var_graphtext[i], x = "Cohort") +
  #   theme(axis.text.x = element_text(angle = 45))
  
  plot_compare_list[[i]] <- ggplot(rawandexcel_locomotor, aes_string(onlymins)) + 
    geom_point(aes(color = shipmentcohort)) +
    # geom_text(aes_string(label=ifelse(onlymins_excel[i] == onlymins_raw[i], '', "labanimalid")),hjust=0,vjust=0) + 
    # geom_text(aes(label = labanimalid), data = joinrawtoexcel[joinrawtoexcel$labanimalid %in% excelhasbutnotraw$labanimalid,]) + # get excelhasbutnotraw from raw and excel qc
    labs(title = paste0("Cohort breakdown of ", onlymins[i], "_locomotor_U01_Jhou")) + 
    scale_x_continuous(limits = xlim[[i]]) + 
    scale_y_continuous(limits = xlim[[i]]) + 
    geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)
  
  
  # geom_text(aes(CPI, HDI, label = Country), data = dat[dat$Country %in% pointsToLabel,])
  
  
  #plot_compare_list[[i]] <- ggplot(data = test, aes(x=minute1.x, y = minute1.y)) + geom_point()
  
  #  print(plot_list[[i]])
  print(plot_compare_list[[i]])
  
  
}
dev.off()

## make this work for one and then put it into the fxn'

temp = reshape(rawandexcel_locomotor, direction="long", varying=paste0("minute", 1:30), sep="") %>% 
  rename("minute" = "time", "counts" = "minute") %>% 
  subset(as.numeric(shipmentcohort) < 2)
lattice::bwplot(counts ~ as.factor(minute) | shipmentcohort, data = temp) + latticeExtra::as.layer(lattice::xyplot(counts ~ as.factor(minute) | shipmentcohort, data = temp))

for(i in 1:length(onlymins)){
  ggplot(rawandexcel_locomotor, aes(x = shipmentcohort, group = shipmentcohort)) + geom_boxplot(aes_string(onlymins))
}

ggplot(subset(rawandexcel_locomotor, shipmentcohort == 1), minute1, group = shipmentcohort)) + geom_boxplot()

ggplot(rawandexcel_locomotor, aes(x = shipmentcohort, y = minute1, group = shipmentcohort)) + geom_boxplot()



#########################################################
# Progressive punishment
#########################################################

progressivepunishment_graph <- progressivepunishment %>%
  dplyr::mutate(shipmentcohort = trunc(as.numeric(progressivepunishment$shipmentcohort)) %>% as.character()) %>%
  dplyr::mutate(shipmentcohort = factor(shipmentcohort, levels=sort(as.numeric(unique(shipmentcohort))), ordered=TRUE),
                labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid))

Jhou_ProgPun_Excel_graph <- Jhou_ProgPun_Excel %>%  
  left_join(., rfidandid, by = "labanimalid") %>%  # extract file information for preparation for appending to rfid
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid))

progpunmeasures <- grep(pattern = "(?=shock|presses)", names(Jhou_ProgPun_Excel_graph), perl = T, value = T)
progpunmeasures_excel <- paste0(progpunmeasures, "_excel")
progpunmeasures_raw <- paste0(progpunmeasures, "_raw")

joinrawtoexcel_progpunishment <- left_join(Jhou_ProgPun_Excel_graph,progressivepunishment_graph,  by = c("labanimalid", "session"))
names(joinrawtoexcel_progpunishment) <- gsub("[.]x", "_excel", names(joinrawtoexcel_progpunishment))
names(joinrawtoexcel_progpunishment) <- gsub("[.]y", "_raw", names(joinrawtoexcel_progpunishment))
xlim <- lapply(joinrawtoexcel_progpunishment[progpunmeasures_excel], range, na.rm=T)
ylim <- lapply(joinrawtoexcel_progpunishment[progpunmeasures_raw], range, na.rm=T)

# check raw_and_excel_qc.R file for changes to joinrawtoexcel values 

pdf("jhou_progpunishment_compare.pdf", onefile = T)
plot_list = list()
plot_compare_list = list()



for (i in seq_along(progpunmeasures)){
  # 
  # plot_list[[i]] <- ggplot(rawfiles_locomotor_wide_graph, aes(x=shipmentcohort))+ 
  #   geom_boxplot(aes_string(y = locomotormeasures[i])) + 
  #   labs(title = paste0(locomotor_dd$var_graphtext[i], "_locomotor_U01_Jhou"),
  #        y = locomotor_dd$var_graphtext[i], x = "Cohort") +
  #   theme(axis.text.x = element_text(angle = 45))
  
  plot_compare_list[[i]] <- ggplot(joinrawtoexcel_progpunishment, aes_string( progpunmeasures_excel[i], progpunmeasures_raw[i])) +
    geom_point(aes(color = shipmentcohort_raw )) +
    theme(axis.text.x = element_text(angle = 45))
    # geom_text(aes_string(label=ifelse(onlymins_excel[i] == onlymins_raw[i], '', "labanimalid")),hjust=0,vjust=0) +
    # geom_text(aes(label = labanimalid), data = joinrawtoexcel_progpunishment[joinrawtoexcel_progpunishment$labanimalid %in% excelhasbutnotraw$labanimalid,]) + # get excelhasbutnotraw from raw and excel qc
    labs(title = paste0("Comparison of ", progpunmeasures[i], "_ProgressivePunishment_U01_Jhou"),
         y = progpunmeasures_raw[i], x = progpunmeasures_excel[i]) +
    geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)


  # g <- ggplot(joinrawtoexcel_progpunishment, aes( activepresses_excel, activepresses_raw)) + 
  #   geom_point(aes(color = shipmentcohort_raw )) + 
  #   theme(axis.text.x = element_text(angle = 45)) + # scale_x_discrete(breaks = scales::pretty_breaks(n = 2)) 
  #   #labs(title = paste0("Comparison of ", progpunmeasures[i], "_ProgressivePunishment_U01_Jhou"),
  #        #y = progpunmeasures_raw[i], x = progpunmeasures_excel[i]) + 
  #   geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)
  
  # geom_text(aes(CPI, HDI, label = Country), data = dat[dat$Country %in% pointsToLabel,])
  
  
  #plot_compare_list[[i]] <- ggplot(data = test, aes(x=minute1.x, y = minute1.y)) + geom_point()
  
  #  print(plot_list[[i]])
  print(plot_compare_list[[i]])
 # print(g)
  
  
}

dev.off()

# change to prog pun text
inexcelnotraw_progpun <- anti_join(subset(Jhou_Locomotor, select = c("labanimalid", "session", onlymins)), 
                                     subset(progpun_raw, select = c("labanimalid", "session", onlymins)),
                                     by = c("labanimalid", onlymins))


###################################################################
# Progressive ratio
##################################################################
# create dataframe without the conflicted id's

progressive_ratio_joined_graph <- Jhou_ProgRatio_Excel %>%
    left_join(., rfidandid, by = "labanimalid") %>%
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)) %>% 
  left_join(., progratio, by = c("labanimalid", "session")) %>% 
  dplyr::filter(!labanimalid %in% conflictedcases_progratio) 
names(progressive_ratio_joined_graph) <- gsub("[.]x", "_excel", names(progressive_ratio_joined_graph))
names(progressive_ratio_joined_graph) <- gsub("[.]y", "_raw", names(progressive_ratio_joined_graph))

progratiomeasures <- c("maxratio", "activepresses", "inactivepresses", "maxratio")
progratiomeasures_excel <- paste0(progratiomeasures, "_excel")
progratiomeasures_raw <- paste0(progratiomeasures, "_raw")

pdf("jhou_progratio_compare.pdf", onefile = T)
plot_compare_list = list()
for (i in seq_along(progratiomeasures)){
 plot_compare_list[[i]] <- ggplot(progressive_ratio_joined_graph, aes_string( progratiomeasures_excel[i], progratiomeasures_raw[i])) +
    geom_point(aes(color = shipmentcohort )) +
    theme(axis.text.x = element_text(angle = 45))
   print(plot_compare_list[[i]])
  }

dev.off()



###################################################################
# Delayed punishment
##################################################################
delayed_pun_joined_graph <- Jhou_Delayedpun_Excel %>%
  left_join(., rfidandid, by = "labanimalid") %>%
  left_join(., delayedpunishment, by = c("labanimalid", "session")) %>%
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid)) # Excel has 5740 values and delayedpunishment raw has 5541 values
  # %>% dplyr::filter(!labanimalid %in% conflictedcases_progratio)
names(delayed_pun_joined_graph) <- gsub("[.]x", "_excel", names(delayed_pun_joined_graph))
names(delayed_pun_joined_graph) <- gsub("[.]y", "_raw", names(delayed_pun_joined_graph))
naniar::vis_miss(delayed_pun_joined_graph)

ggplot(delayed_pun_joined_graph %>% dplyr::filter(date_excel != date_raw), aes(x = date_excel, y = date_raw, label = labanimalid)) + 
  geom_point() + 
  ggrepel::geom_label_repel(aes(label=labanimalid))
