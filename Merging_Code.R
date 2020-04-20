install.packages('digest')
library(digest)

#----Import- Raw Data and Geocoded Data
Raw_PPR_Sample <- read.csv("/Users/mickdineen97/Desktop/FYP_Data/PPR_Semester_2A.csv")
Geocoded_Sample <-  read.csv("/Users/mickdineen97/Desktop/FYP_Data/Geo_PPR_April_Fin.csv")
#Match Dataset Length
Raw_PPR_Sample <- Raw_PPR_Sample[1:62312, ]
colnames(Geocoded_Sample)[9] <- 'Address'

#Remove Ireland From the Geocoded Input Address,
#This was Added to Improve Accuarcy in Geocoding.
Geocoded_Sample$Address <- as.character(Geocoded_Sample$Address)
Geocoded_Sample$Address <- substring(Geocoded_Sample$Address,1,nchar(Geocoded_Sample$Address) -9)

#Merge the Two Data using the Common Variable, Address
Merged_Data <- merge(Geocoded_Sample, Raw_PPR_Sample, by = "Address")

#Remove Duplicates 
Merged_Data <- Merged_Data[!duplicated(Merged_Data$Address), ]
Merged_Data$Address[ !duplicated(lapply(Merged_Data$Address, summary))]

#Set empty cells as NA's
Merged_Data[Merged_Data == ''] <- NA
sapply(Merged_Data, function(x) sum(is.na(x)))
#Merged_Data <- Merged_Data[complete.cases(Merged_Data[, ]), ]

#Remove Datasets 
rm(Geocoded_Sample)
rm(PPR_Semester_2A)
rm(Raw_PPR_Sample)

#Write File
write.csv(file="Merged_Data",Merged_Data, row.names = F)





