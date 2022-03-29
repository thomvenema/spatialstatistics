library(dplyr)

setwd("~/Downloads/GR2022_osv4-3_2022-03-23T21.36/")

plaatsen <- list.files(pattern="")
folderfile <- ""
folderfile_l <- ""
count <- 0
for (i in plaatsen){
  myFiles <- list.files(path = paste(i,"/", sep=""),pattern="*.csv")
  folderfile <- paste(i,"/",myFiles, sep="")
  for (j in folderfile){
    count <- count + 1
    folderfile_l[[count]] <- j
  }
}

aantal_gebruikt <- 0
df_total = data.frame()
for (k in 1:length(folderfile_l)){
  data_verkiezing <- read.csv(folderfile_l[k], header=FALSE, sep=";")
  if (data_verkiezing[8,1] == "Postcode") {
    aantal_gebruikt <- aantal_gebruikt + 1
    data_verkiezing_test <- data_verkiezing %>% slice(c(6,8,9,14))
    data_verkiezing_test <- as.data.frame(t(data_verkiezing_test)) %>% slice(-c(1:5))
    data_verkiezing_test$V5 <- as.numeric(data_verkiezing_test$V4)/as.numeric(data_verkiezing_test$V3)
    df_total <- rbind(df_total,data_verkiezing_test)
  }
}
colnames(df_total) <- c("Street/Name", "Zipcode", "Invited", "Turnout", "Turnout percentage")

#data_verkiezing <- osv4.3_telling_gr2022_utrecht
#data_verkiezing_test <- data_verkiezing %>% slice(c(6,8,9,14))
#data_verkiezing_test <- as.data.frame(t(data_verkiezing_test)) %>% slice(-c(1:5))
#data_verkiezing_test$V5 <- as.numeric(data_verkiezing_test$V4)/as.numeric(data_verkiezing_test$V3)

#colnames(data_verkiezing_test) <- c("Street", "Zipcode", "Invited", "Turnout", "Turnout percentage")
                          
