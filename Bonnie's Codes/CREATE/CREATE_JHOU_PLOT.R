### CREATE_JHOU_PLOT.R

# Runway
# Locomotor 

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

test <- left_join(Jhou_Locomotor_Excel_graph,rawfiles_locomotor_wide_graph,  by = c("labanimalid", "session"))
names(test) <- gsub(".x", "_excel", names(test))
names(test) <- gsub(".y", "_raw", names(test))


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

  plot_compare_list[[i]] <- ggplot(test, aes_string( onlymins_excel[i], onlymins_raw[i])) + 
                                     geom_point(aes(color = shipmentcohort_excel)) +
    labs(title = paste0("Comparison of ", onlymins_excel[i], "_locomotor_U01_Jhou"),
         y = onlymins_raw[i], x = onlymins_excel[i])
  
  #plot_compare_list[[i]] <- ggplot(data = test, aes(x=minute1.x, y = minute1.y)) + geom_point()
  
  #  print(plot_list[[i]])
  print(plot_compare_list[[i]])
  
  
}

dev.off()

# for min19 which has weird axes for some reason
# ggplot(test, aes(minute19_excel, minute19_raw)) + geom_point() + coord_cartesian(xlim = c(0, 80), ylim = c(0, 80))

# Progressive punishment
# Progressive ratio
# Delayed punishment