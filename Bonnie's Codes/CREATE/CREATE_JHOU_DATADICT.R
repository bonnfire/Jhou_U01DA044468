# CREATE DATA DICTIONARY

jhou_datadictionary <- list()

#############################
# Runway
#############################

#############################
# Locomotor 
#############################

locomotormeasures

locomotor_dd <- data.frame(var_abbv = locomotormeasures, 
                           var_graphtext = c(paste("Locomotor counts for minute", 1:31),
                                             "Total locomotor counts in session", "Average locomotor counts in session"), 
                           var_description = NA)


#############################
# Progressive punishment
#############################

#############################
# Progressive ratio
#############################

#############################
# Delayed punishment
#############################

jhou_datadictionary <- list("locomotor" = locomotor_dd)