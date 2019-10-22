### CREATE PLOTS

pdf("cohort1_olivier_selfadmin.pdf",onefile = T)

plot_list = list()

values <- grep("^(def|agg)", names(cohort1$tirritability), value = T)
df <- cohort1$tirritability
for (i in seq_along(values)){
  
  plot_list[[i]] <- ggplot(df, aes(x=rat))+ geom_point(aes_string(y = values[i])) + labs(title = values[i])
  print(plot_list[[i]])
  
}

df2 <- cohort1$tselfadmin
values2 <- grep(pattern = "^(?!rf|date|comment|lab|cohort)", names(cohort1$tselfadmin), perl = T, value = T)
for (i in seq_along(values2)){
  
  plot_list[[i]] <- ggplot(df2, aes(x=labanimalid))+ geom_point(aes_string(y = values2[i])) + labs(title = values2[i]) 
  print(plot_list[[i]])
  
}
dev.off()

# rawfiles_locomotor_melt <- melt(rawfiles_locomotor_wide,  id.vars = 'filename', variable.name = 'animals')
# ggplot(rawfiles_locomotor_melt, aes(filename,value)) + geom_line(aes(colour = animals))
ggplot(data = rawfiles_locomotor_long, aes(x = minute, y = bincounts, color = filename, group = filename))+
  geom_path(show.legend = FALSE) + geom_point(show.legend = F)
