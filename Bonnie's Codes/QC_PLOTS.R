### CREATE PLOTS

# load libraries

# rawfiles_locomotor_melt <- melt(rawfiles_locomotor_wide,  id.vars = 'filename', variable.name = 'animals')
# ggplot(rawfiles_locomotor_melt, aes(filename,value)) + geom_line(aes(colour = animals))
ggplot(data = rawfiles_locomotor_long, aes(x = minute, y = bincounts, color = filename, group = filename))+
  geom_path(show.legend = FALSE) + geom_point(show.legend = F)
