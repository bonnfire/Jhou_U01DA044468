### CREATE PLOTS FOR JHOU DATA

## TO DO: 
# CREATE THE DATADICTIONARY AND REPLACE IN LABS TITLE = 

library(ggplot2)
# list <- list("runway" = runway)
# , 
#              "progpunishment" = cohort2,
#              "cohort3" = cohort3,
#              "cohort4" = cohort4,
#              "cohort5" = cohort5,
#              "cohort7" = cohort7,
#              "cohort8" = cohort8)



# process the runway data to make shipment cohorts (reorder and WFU)

runway_graph <- runway %>% 
  mutate(shipmentcohort = shipmentcohort %>% as.numeric() %>% sort(decreasing = F) %>% factor) # create 29 levels in order

runway_wholenumbercohorts <- runway %>% 
  mutate(shipmentcohort = as.numeric(as.character(shipmentcohort)) %>% trunc() %>% sort(decreasing = F) %>% as.factor())  # create 10 levels in order

# process the runway_excel data to make shipment cohorts (reorder and WFU); append shipment cohort information from summaryall

tJhou_Runway_graph <- tJhou_Runway_data %>% 
  rename(labanimalid = animalid) %>%
  merge(x = ., y = rfidandid[, c("labanimalid", "shipmentcohort")], by = "labanimalid", all.x=T) %>%
  mutate(shipmentcohort = shipmentcohort %>% as.numeric() %>% sort(decreasing = F) %>% factor) # NA shippment cohort

 # check if all missing shipping information correlates to dead animals (check in runway table as well)


tJhou_Runway_wholenumbercohorts <- tJhou_Runway_data %>% 
  mutate(shipmentcohort = as.numeric(as.character(shipmentcohort)) %>% trunc() %>% sort(decreasing = F) %>% as.factor())


# extract variables of interest to graph 

jhourunwaymeasures <- grep(pattern = "^(elapsedtime|reversals)", names(runway_graph), perl = T, value = T)

# create plots 

pdf("jhou_runway.pdf", onefile = T)
for (i in seq_along(jhourunwaymeasures)){
  
  # Raw files *with decimal point cohorts
  
  plot_list <- ggplot(runway_graph, aes(x=shipmentcohort, color = shipmentcohort)) + 
    geom_boxplot(aes_string(y = jhourunwaymeasures[i]), outlier.size = 0.75) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou", "\n"), x = "Cohort", fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
    # with the outlier removed from Cohort 9.1
  plot_list_outlier <- ggplot(data = runway_graph %>% filter(elapsedtime < 2000), aes(x=shipmentcohort, color = shipmentcohort)) + 
        geom_boxplot(aes_string(y = jhourunwaymeasures[i]), outlier.size = 0.75) + 
        labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_removedoutlier", "\n"), x = "Cohort", fill = "Cohort") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list2 <- ggplot(runway_graph, aes(color = shipmentcohort)) + 
    geom_density(aes_string(jhourunwaymeasures[i])) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou", "\n"), x = jhourunwaymeasures[i], fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
    # with the outlier removed from Cohort 9.1
  plot_list2_outlier <-  ggplot(data = runway_graph %>% filter(elapsedtime < 2000), aes(x=shipmentcohort, color = shipmentcohort)) +
        geom_density(aes_string(jhourunwaymeasures[i])) + 
        labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_removedoutlier", "\n"), x = jhourunwaymeasures[i], fill = "Cohort") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
  # Raw files *with whole number cohorts

      # with removed outliers
      
  plot_list3 <- ggplot(runway_wholenumbercohorts %>% filter(elapsedtime < 2000), aes(x = shipmentcohort, color = shipmentcohort)) + 
    geom_boxplot(aes_string(y = jhourunwaymeasures[i]), outlier.size = 0.75) +
    geom_jitter(aes_string(y = jhourunwaymeasures[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_wWFUcohort_wremovedoutlier", "\n"), x = "Cohort", fill = "Cohort") +  # scale_fill_discrete(name = "New Legend Title")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list4 <- ggplot(runway_wholenumbercohorts %>% filter(elapsedtime < 2000), aes(color = shipmentcohort)) + 
    geom_density(aes_string(jhourunwaymeasures[i]), alpha = 0.5) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_wWFUcohort_wremovedoutlier", "\n"), x = jhourunwaymeasures[i], fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Excel files *with decimal point cohorts
  plot_list5 <- ggplot(tJhou_Runway_graph %>% filter(elapsedtime < 2000), aes(x = shipmentcohort, color = shipmentcohort)) + 
    geom_boxplot(aes_string(y = jhourunwaymeasures[i]), outlier.size = 0.75) +
    geom_jitter(aes_string(y = jhourunwaymeasures[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_wWFUcohort_wremovedoutlier", "\n"), x = "Cohort", fill = "Cohort") +  # scale_fill_discrete(name = "New Legend Title")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Excel files *with whole number cohorts
  plot_list6 <- ggplot(tJhou_Runway_wholenumbercohorts %>% filter(elapsedtime < 2000), aes(x = shipmentcohort, color = shipmentcohort)) + 
    geom_boxplot(aes_string(y = jhourunwaymeasures[i]), outlier.size = 0.75) +
    geom_jitter(aes_string(y = jhourunwaymeasures[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_wWFUcohort_wremovedoutlier", "\n"), x = "Cohort", fill = "Cohort") +  # scale_fill_discrete(name = "New Legend Title")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  print(plot_list)
  print(plot_list_outlier)
  print(plot_list2)
  print(plot_list2_outlier)
  print(plot_list3)
  print(plot_list4)
  print(plot_list5)
  print(plot_list6)
}


dev.off()


# rawfiles_locomotor_melt <- melt(rawfiles_locomotor_wide,  id.vars = 'filename', variable.name = 'animals')
# ggplot(rawfiles_locomotor_melt, aes(filename,value)) + geom_line(aes(colour = animals))
# ggplot(data = rawfiles_locomotor_long, aes(x = minute, y = bincounts, color = filename, group = filename))+
#   geom_path(show.legend = FALSE) + geom_point(show.legend = F)
