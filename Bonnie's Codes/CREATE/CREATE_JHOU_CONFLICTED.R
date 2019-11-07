# CREATE THE DATA FRAME FOR CONFLICTED CASES

# make this the same index as the names vector, ignore
# use Jhou_Excel_ProgRatio_red$col %>% table() to find the distribution of the red cases
Jhou_Excel_ProgRatio_red <- Jhou_Excel_ProgRatio_red %>% 
  mutate(col = ifelse(col == 12, 10, 
                      ifelse(col == 17, NA, col))) %>% 
  dplyr::filter(!is.na(col))

Jhou_conflicted <- data.frame(rfid = 1, labanimalid = 1, cohort = 1, experiment = 1, file1me = 1, affectedvar = 1, comment = 1, resolution = 1)

Jhou_conflicted <- data.frame(labanimalid = character(), cohort = character(), experiment = character(), affectedvar = character(), comment = character(), resolution = character())
rbind(Jhou_conflicted, list(labanimalid = Jhou_ProgRatio_Excel[Jhou_Excel_ProgRatio_red$row,]$labanimalid,
                            experiment = rep("ProgRatio", 57), 
                            affectedvar = names(Jhou_ProgRatio_Excel)[Jhou_Excel_ProgRatio_red$col], 
                            comment = Jhou_ProgRatio_Excel[Jhou_Excel_ProgRatio_red$row,]$notes, 
                            resolution = rep("Ignore", 57)))

      