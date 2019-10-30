### CREATE_JHOU_PLOT.R

# Runway
# Locomotor 
pdf("jhou_locomotor.pdf", onefile = T)
plot_list = list()

rawfiles_locomotor_wide_graph <- rawfiles_locomotor_wide %>% 
  mutate(shipmentcohort = trunc(as.numeric(rawfiles_locomotor_wide$shipmentcohort)) %>% as.character())

locomotormeasures <- grep(pattern = "^(?=min|bin)", names(rawfiles_locomotor_wide_graph), perl = T, value = T)

for (i in seq_along(locomotormeasures)){
  
  plot_list[[i]] <- ggplot(rawfiles_locomotor_wide_graph, aes(x=shipmentcohort, color = shipmentcohort))+ 
    geom_boxplot(aes_string(y = locomotormeasures[i])) + 
    labs(title = paste0(locomotor_dd$var_graphtext[i], "_locomotor_U01_Jhou"),
         y = locomotor_dd$var_graphtext[i], x = "Cohort") +
    theme(axis.text.x = element_text(angle = 45))
  print(plot_list[[i]])

}

dev.off()

# Progressive punishment
# Progressive ratio
# Delayed punishment