################################
########### WFU DATA(BASIC) ####
################################
# check if values are moved correctly from shipment files to the summary all page




################################
########### WFU DATA(BASIC) ####
################################

# extract cohort 
ship_path <- "/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_WFU_U01_ShippingMaster/Jhou Master Shipping Sheet.xlsx"
sheetnames <- excel_sheets(ship_path)
WFU_Jhou <- lapply(excel_sheets(ship_path), read_excel, path = ship_path)

# name the dataframes
names(WFU_Jhou) <- sheetnames

# Skip first row and read in XL worksheets
# WFU_Jhou <- lapply(ship_path, function(x) {
#   sheets <- excel_sheets(x)
#   dfs <- lapply(sheets, function(y) {
#     read_excel(x, sheet = y)
#   })
#   names(dfs) <- sheets
#   dfs
# })

# cut down the first table and rebind
WFU_Jhou[[1]] <- WFU_Jhou[[1]][1:17, 1:15]

WFU_Jhou[1:6] <- lapply(WFU_Jhou[1:6], function(x){
  colnames(x)[1:2] <- x[1,1:2]
  x <- x[-1, ]
}) 
# row.names(x) <- 1:nrow(x) (to reorder the row numbers)

# add date of shipment column for second table 
WFU_Jhou[[2]] <- WFU_Jhou[[2]] %>% 
  mutate("Date of Ship" = as.POSIXct(ymd("2018-07-05")))

#rename column name for consistency 
# WFU_Jhou <- lapply(WFU_Jhou, function(x){
#   x <- x %>%
#   rename("Litter Number" = "Litter #",
#          "Date of Ship" = "Ship Date",
#          "Shipping Box" = "Ship Box",
#          "Animal ID" = "Animal #",
#          "Date of Wean" = "Wean Date") 
# }) 

# create dataframe from WFU list of dataframes
WFU_Jhou_df <- bind_rows(WFU_Jhou, .id = "id")
# merge two similarly named columns 
WFU_Jhou_df <- WFU_Jhou_df %>%
  mutate("Date of Ship" = ifelse(is.na(WFU_Jhou_df$"Date of Ship"), WFU_Jhou_df$"Ship Date", WFU_Jhou_df$"Date of Ship"),
         "Shipping Box" = ifelse(is.na(WFU_Jhou_df$"Shipping Box"), WFU_Jhou_df$"Ship Box", WFU_Jhou_df$"Shipping Box"),
         "Animal ID" = ifelse(is.na(WFU_Jhou_df$"Animal ID"), WFU_Jhou_df$"Animal #", WFU_Jhou_df$"Animal ID"),
         "Date of Wean" = ifelse(is.na(WFU_Jhou_df$"Date of Wean"), WFU_Jhou_df$"Wean Date", WFU_Jhou_df$"Date of Wean"),
         "Litter Number" = ifelse(is.na(WFU_Jhou_df$"Litter Number"), WFU_Jhou_df$"Litter #", WFU_Jhou_df$"Litter Number")) %>%
  select(-c("Litter #", "Ship Date","Ship Box","Animal #","Wean Date"))

# WFU_Jhou_df <- map_df(WFU_Jhou, ~as.data.frame(.x), .id="Cohort")
# WFU_Jhou_df_test2 <- WFU_Jhou_df %>%
#   rename("Litter Number" = "Litter #",
#          "Date of Ship" = "Ship Date",
#          "Shipping Box" = "Ship Box",
#          "Animal ID" = "An imal #",
#          "Date of Wean" = "Wean Date") 

# check id from file vs folder 
folder_id <- stringr::str_extract_all(lt_files, "_[[:digit:]]{3}_") %>% unlist()
folder_id <- paste0("U", gsub("_", "", folder_id))
animal_ID_lever <- strsplit(Lever_training_IDs, "\\D$") %>% unlist()

### 
files<-list.files(path="/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Tom_Jhou_U01/Lever training",recursive = T,pattern=".txt",full.names = T)

basename(files)


