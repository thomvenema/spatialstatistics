library(dplyr)
library(rstudioapi)
library(tidyr)
#install.packages("zoo")              
library(zoo)  
setwd(dirname(getActiveDocumentContext()$path))
getwd()      

#Importing dataset with polling station locations
pollstations <- read.csv("data/stembureaus.csv")
#Keeping only relevant columns
pollstations <- pollstations[c("Gemeente","CBS.gemeentecode","Nummer.stembureau","Naam.stembureau","Postcode", "Plaats", "X", "Y", "Longitude", "Latitude")]
pollstations$CBS.gemeentecode <- extract_numeric(pollstations$CBS.gemeentecode)

### Manual imputation of a municipality without numbering of poll stations
pollstations[5815:5853,3] <- c(19,22,42,43,34,39,37,11,9,8,2,3,12,32,28,33,29,31,25,41,27,30,18,40,1,15,38,24,17,21,6,35,36,13,23,20,5,7,10)


# Make a merger for easier merging
pollstations$merger <- paste(pollstations$CBS.gemeentecode,pollstations$Nummer.stembureau)


plaatsen <- list.files(path="data/",pattern="")
folderfile <- ""
folderfile_l <- ""
count <- 0
for (i in plaatsen){
  myFiles <- list.files(path = paste("data/",i,"/", sep=""),pattern="*.csv")
  folderfile <- paste("data/",i,"/",myFiles, sep="")
  for (j in folderfile){
    count <- count + 1
    folderfile_l[[count]] <- j
  }
}

df_total = data.frame()
for (k in 1:length(folderfile_l)){
  data_verkiezing <- read.csv(folderfile_l[k], header=FALSE, sep=";")
  if (data_verkiezing[8,1] == "Postcode") {
    data_verkiezing_test <- data_verkiezing %>% slice(c(6,7,8,9,14))
    data_verkiezing_test <- as.data.frame(t(data_verkiezing_test)) %>% slice(-c(1:5))
    data_verkiezing_test$V6 <- as.numeric(data_verkiezing_test$V5)/as.numeric(data_verkiezing_test$V4)
    df_total <- rbind(df_total,data_verkiezing_test)
  } else {
    #for (k in 1:length(folderfile_l)){
     # data_postcode_1 <- read.csv(folderfile_l[k], header=FALSE, sep=";")
    data_postcode_1 <- as.data.frame(t(data_verkiezing))
    data_postcode_1[,5] <- as.numeric(data_postcode_1[3,4])
    data_postcode_1 <- data_postcode_1[c(5:8,12)] 
    data_postcode_1 <- data_postcode_1 %>% slice(-c(1:5))
    data_postcode_1$merger <- paste(data_postcode_1$V5, data_postcode_1$V7)
    data_postcode_1 <- merge(x = data_postcode_1, y = pollstations[c("merger","Postcode")], by = "merger", all.x = TRUE)
    
    # Some voting stations had different days of polling. Those were given new pollingstation numbers which are not in the locations dataset
    # As a fix, we sort the dataset at names, and give the polling station with NA's the postal code from the station the row above, which should be the same
    # all cases
    data_postcode <- data_postcode_1[order(data_postcode_1$V6),]
    #data_postcode$Postcode <- na.locf(data_postcode$Postcode)
    #change order to match other for loop
    data_postcode <- data_postcode[, c(3, 2, 7, 5, 6)]
    data_postcode$v6 <- "test"#as.numeric(data_postcode[,"V12"])/as.numeric(data_postcode[,"V8"])
    colnames(data_postcode) <- c("V1", "V2", "V3", "V4","V5", "V6")
    
    df_total <- rbind(df_total,data_postcode)
    #Printing a list of all municipalities that have a different file structure
    print(folderfile_l[k])
    #Municipalities lack the postal code with we will add matching the municipality and booth name with the different dataset
    }
}

colnames(df_total) <- c("Street/Name", "stationcode", "Zipcode", "Invited", "Turnout", "Turnout percentage")

write.csv(df_total,"pollingstations.csv", row.names = FALSE)

# Toe te voegen:
#   -Missende stembureaus
# x -Gebiednummer toevoegen aan eerste deel
#   -Samenvoegen duplicates
#   -Coordinaten
#   -Buurtcode 
#   -Population die naar dat stembureau zou gaan gebasseerd op thyssen polgon en aantal stemgerechtigden
#   -Betere turnout rate








test_postcode <- read.csv("data/01_Groningen/osv4-3_telling_gr2022_hethogeland.csv", header=FALSE, sep=";")


data_postcode_1 <- as.data.frame(t(test_postcode))
data_postcode_1[,5] <- as.numeric(data_postcode_1[3,4])
data_postcode_1 <- data_postcode_1[c(5:8,12)] 
data_postcode_1 <- data_postcode_1 %>% slice(-c(1:5))
data_postcode_1$merger <- paste(data_postcode_1$V5, data_postcode_1$V7)
data_postcode_1 <- merge(x = data_postcode_1, y = pollstations[c("merger","Postcode")], by = "merger", all.x = TRUE)

# Some voting stations had different days of polling. Those were given new pollingstation numbers which are not in the locations dataset
# As a fix, we sort the dataset at names, and give the polling station with NA's the postal code from the station the row above, which should be the same
# all cases
data_postcode <- data_postcode_1[order(data_postcode_1$V6),]
data_postcode$Postcode <- na.locf(data_postcode$Postcode)
#change order to match other for loop
data_postcode <- data_postcode[, c(3, 2, 7, 5, 6)]
data_postcode$v6 <- as.numeric(data_postcode[,"V12"])/as.numeric(data_postcode[,"V8"])
colnames(data_postcode) <- c("V1", "V2", "V3", "V4","V5", "V6")

#colnames(data_postcode) <- c("Street/Name", "Zipcode", "Invited", "Turnout", "Turnout percentage")






