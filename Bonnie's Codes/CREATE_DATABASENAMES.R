experiments <- list("runway" = names(runway),
                    "locomotor" = names(rawfiles_locomotor_wide),
                    ""
                    "progressivepunishment_box" = names(rawfiles_box),
                    "progressivepunishment_raw" = names(rawfiles_pp),
                    )
rbind() 
# in runway, add 
# in locomotor, add cohort, date, and time 
# in propun, add box number, number of trials at last shock intensity

# in all, add the notes column 

save(list = (), file = "jhouexpnames.RData")
