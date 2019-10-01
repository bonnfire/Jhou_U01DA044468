setwd("/home/bonnie/Dropbox (Palmer Lab)/U01 folder")

U01_Behavioral_data_readonly2 <- read_excel("U01 Behavioral data_readonly2.xlsx")
names(U01_Behavioral_data_readonly2) <- as.character(unlist(U01_Behavioral_data_readonly2[1, ]))
names(U01_Behavioral_data_readonly2) <- gsub(" ", "_", names(U01_Behavioral_data_readonly2))
U01_Behavioral_data_readonly2 <- U01_Behavioral_data_readonly2[-1, ]

Jhou_Master_Shipping_Sheet_original <- read_excel("~/Public/20190829_AllShipments_Jhou_WFU_Master.xls")
Jhou_Master_Shipping_Sheet <- Jhou_Master_Shipping_Sheet_original
names(Jhou_Master_Shipping_Sheet)[c(1,2)] <- paste0("Parents_", as.character(unlist(Jhou_Master_Shipping_Sheet[1, 1:2])))
names(Jhou_Master_Shipping_Sheet) <- gsub(" ", "_", names(Jhou_Master_Shipping_Sheet))
Jhou_Master_Shipping_Sheet <- Jhou_Master_Shipping_Sheet[-1, ]

Jhou_Sex <- U01_Behavioral_data_readonly2[,c("Sex", "16_digit_ID")] %>% distinct() # Remove duplicates, otherwise there are 1200+ observations 
Jhou_Sex <- rename(Jhou_Sex, Jhou_Sex = Sex, Transponder_ID = "16_digit_ID")
Shipping_Jhou_Sex <- Jhou_Master_Shipping_Sheet[,c("Sex", "Transponder_ID")]
Shipping_Jhou_Sex <- rename(Shipping_Jhou_Sex, Shipping_Sex = Sex)

Sex_QC <- left_join(Jhou_Sex, Shipping_Jhou_Sex, by = "Transponder_ID")
length(which(Sex_QC$Shipping_Sex==Sex_QC$Jhou_Sex))
Sex_QC[17,] # NA from original dataset 
