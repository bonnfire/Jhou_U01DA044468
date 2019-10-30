### CREATE_JHOU_PLOT.R

# Runway
# Locomotor 
pdf("jhou_locomotor.pdf", onefile = T)
plot_list = list()
locomotormeasures <- grep(pattern = "^(?=min|bin)", names(rawfiles_locomotor_wide), perl = T, value = T)
for (i in seq_along(locomotormeasures)){
  
  plot_list[[i]] <- ggplot(rawfiles_locomotor_wide, aes(x=cohort, color = cohort))+ geom_boxplot(aes_string(y = locomotormeasures[i])) + labs(title = paste0(locomotormeasures[i], "_SA_U01_Olivier", "\n", width = 80))
  print(plot_list[[i]])

}

dev.off()

# Progressive punishment
# Progressive ratio
# Delayed punishment