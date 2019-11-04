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
rawfiles_locomotor_wide_graph <- test__split_session %>% 
  mutate(shipmentcohort = trunc(as.numeric(test__split_session$shipmentcohort)) %>% as.character(),
         shipmentcohort = factor(test__split_session$shipmentcohort, levels=sort(as.numeric(unique(test__split_session$shipmentcohort))), ordered=TRUE))


Jhou_Locomotor_Excel_graph <- Jhou_Locomotor %>%  
  left_join(., rfidandid, by = "labanimalid") %>%  # extract file information for preparation for appending to rfid
  mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid))

locomotormeasures <- grep(pattern = "^(?=min|bin)", names(rawfiles_locomotor_wide_graph), perl = T, value = T)
onlymins <-  grep(pattern = "^(min)", names(rawfiles_locomotor_wide_graph), perl = T, value = T)[-31]
onlymins_excel <- paste0(onlymins, "_excel")
onlymins_raw <- paste0(onlymins, "_raw")

joinrawtoexcel <- left_join(Jhou_Locomotor_Excel_graph,rawfiles_locomotor_wide_graph,  by = c("labanimalid", "session"))
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
                                     geom_point(aes(color = shipmentcohort_excel)) +
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
    # geom_text(aes_string(label=ifelse(onlymins_excel[i] == onlymins_raw[i], '', "labanimalid")),hjust=0,vjust=0) + 
    # geom_text(aes(label = labanimalid), data = joinrawtoexcel_progpunishment[joinrawtoexcel_progpunishment$labanimalid %in% excelhasbutnotraw$labanimalid,]) + # get excelhasbutnotraw from raw and excel qc
    labs(title = paste0("Comparison of ", progpunmeasures[i], "_ProgressivePunishment_U01_Jhou"),
         y = progpunmeasures_raw[i], x = progpunmeasures_excel[i]) + 
    geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)
  
  
  ggplot(joinrawtoexcel_progpunishment, aes( activepresses_excel, activepresses_raw)) + 
    geom_point(aes(color = shipmentcohort_raw )) +
    # geom_text(aes_string(label=ifelse(onlymins_excel[i] == onlymins_raw[i], '', "labanimalid")),hjust=0,vjust=0) + 
    # geom_text(aes(label = labanimalid), data = joinrawtoexcel_progpunishment[joinrawtoexcel_progpunishment$labanimalid %in% excelhasbutnotraw$labanimalid,]) + # get excelhasbutnotraw from raw and excel qc
    #labs(title = paste0("Comparison of ", progpunmeasures[i], "_ProgressivePunishment_U01_Jhou"),
         #y = progpunmeasures_raw[i], x = progpunmeasures_excel[i]) + 
    geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)
  
  # geom_text(aes(CPI, HDI, label = Country), data = dat[dat$Country %in% pointsToLabel,])
  
  
  #plot_compare_list[[i]] <- ggplot(data = test, aes(x=minute1.x, y = minute1.y)) + geom_point()
  
  #  print(plot_list[[i]])
  print(plot_compare_list[[i]])
  
  
}

dev.off()

# Progressive ratio
# Delayed punishment