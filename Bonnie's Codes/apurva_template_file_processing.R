rm(list=ls())

setwd("/home/apurva/Desktop/Dropbox (Palmer Lab)/Palmer Lab/Apurva Chitre/Tom_Jhou_U01/")



list.files()

IDs<-list.dirs(path="Delayed punishment/",full.names=F)


length(IDs[!is.na(IDs)])


files<-list.files(path="./Delayed punishment",recursive = T,pattern=".txt",full.names = T)

basename(files)


#column1: full filename along with directory
#column2: basename()
#column3 extract animal ID
#column4: e or i
#column5: y/M/d
#column6: time
#column7: cohort

grepl()




#count number of unique files for each animals
