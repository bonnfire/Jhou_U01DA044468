### CREATE PLOTS FOR JHOU DATA

library(ggplot2)
# list <- list("runway" = runway)
# , 
#              "progpunishment" = cohort2,
#              "cohort3" = cohort3,
#              "cohort4" = cohort4,
#              "cohort5" = cohort5,
#              "cohort7" = cohort7,
#              "cohort8" = cohort8)

pdf("jhou_runway.pdf", onefile = T)

# plot_list = list()
# plot_list2 = list()

jhourunwaymeasures <- grep(pattern = "^(elapsedtime|reversals)", names(runway), perl = T, value = T)

# process the runway data to make shipment cohorts uniform to WFU (plot_list3)
runway_wholenumbercohorts <- runway %>% 
  mutate(shipmentcohort = as.numeric(shipmentcohort) %>% trunc() %>% as.character())

for (i in seq_along(jhourunwaymeasures)){
  
  plot_list <- ggplot(runway, aes(x=as.factor(shipmentcohort), color = as.factor(shipmentcohort))) + 
    geom_boxplot(aes_string(y = jhourunwaymeasures[i])) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou", "\n"), x = "Cohort", fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  plot_list2 <- ggplot(runway, aes(color = as.factor(shipmentcohort))) + 
    geom_density(aes_string(jhourunwaymeasures[i]), alpha = 0.5) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou", "\n"), x = jhourunwaymeasures[i], fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

  plot_list3 <- ggplot(runway_wholenumbercohorts, aes(x=as.factor(shipmentcohort), color = as.factor(shipmentcohort))) + 
    geom_boxplot(aes_string(y = jhourunwaymeasures[i])) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_wWFUcohort", "\n"), x = "Cohort", fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot_list4 <- ggplot(runway_wholenumbercohorts, aes(color = as.factor(shipmentcohort))) + 
    geom_density(aes_string(jhourunwaymeasures[i]), alpha = 0.5) + 
    labs(title = paste0(jhourunwaymeasures[i], "_Runway_U01_Jhou_wWFUcohort", "\n"), x = jhourunwaymeasures[i], fill = "Cohort") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot_list)
  print(plot_list2)
  print(plot_list3)
  print(plot_list4)
}


dev.off()


# rawfiles_locomotor_melt <- melt(rawfiles_locomotor_wide,  id.vars = 'filename', variable.name = 'animals')
# ggplot(rawfiles_locomotor_melt, aes(filename,value)) + geom_line(aes(colour = animals))
# ggplot(data = rawfiles_locomotor_long, aes(x = minute, y = bincounts, color = filename, group = filename))+
#   geom_path(show.legend = FALSE) + geom_point(show.legend = F)
