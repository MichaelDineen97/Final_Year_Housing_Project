#Michael Dineen,
#X16350523,
#BSHTM4,
#Final Year Project. 

#-------------------Start---------------
#Libraries
install.packages("randomForest")
library(randomForest)
library(caret)
library(ggplot2)

#Import and Review Data
Geocoded_PPR
Geocoded_PPR[Geocoded_PPR == ''] <- NA #Turn all blanks to NA
sapply(Geocoded_PPR, function(x) sum(is.na(x))) #Post Code has NA's

#Remove All NA's
Geocoded_PPR$postcode <- NULL
sapply(Geocoded_PPR, function(x) sum(is.na(x))) #Post Code has NA's
#NA's Removed

#------------------Get a sample of The data, Train and Test -----------------

#50% of the data was selected as a sample size
sample_index <- sample(1:nrow(Geocoded_PPR), 1*nrow(Geocoded_PPR), replace = F) 
sampleData <- Geocoded_PPR[sample_index, ]

#Class Imbalance means we have to minimise the varaible length, 
#sampleData <- subset(sampleData, Number_of_Vehicles < 7) #Accidnets with less than 7 cars

# The 0.8*nrow(data) is getting a sample of 80% of the data
rf_sample_index <- sample(1:nrow(sampleData), 0.8*nrow(sampleData), replace = F)

#Seperate into train and test, 
PPR_train <- sampleData[rf_sample_index, ] # Storing the 80% of data in train
PPR_test <- sampleData[-rf_sample_index, ] # Storing the remaining 20% to test

#Ensure the Length of the Two variables are uniform. 
str(PPR_train$Description.of.Property) #6
str(PPR_test$Description.of.Property) #6 

#Ensure the Length of the Two variables are uniform. 

#-----------------Random Forrest Model-----------------------

#Dependant Variable - Vehicle
rf_Desc_Property <- randomForest(Description.of.Property ~ Price + County + year + Not.Full.Market.Price, PPR_train)

rf_All <- randomForest(Description.of.Property ~ Price + County + year + Not.Full.Market.Price + latitude + longitude + accuracy + month + day + VAT.Exclusive + number_of_results,PPR_train)

#Visualize Model
varImpPlot(rf_Desc_Property , col="Blue", main = "Random Forrest: Property Description", bg="Purple")
varImpPlot(rf_All , col="Blue", main = "Random Forrest: Property Description", bg="Purple")

#---------------------Predictions and Confusion Model-------------------

PredictionRF_Desc <- predict(rf_Desc_Property, PPR_test)
confusionMatrix(PredictionRF_Desc, PPR_test$Description.of.Property) 

PredictionRF_ALL <- predict(rf_All, PPR_test)
confusionMatrix(PredictionRF_ALL, PPR_test$Description.of.Property) 

#---------------Show Distribution and Class Imbalance-----------------

ggplot(PPR_train, aes(Description.of.Property, head= "Distribution of Vehicle Numbers")) + geom_bar()
table(PPR_test$VAT.Exclusive)

table(PPR_test$type)
table(Geocoded_PPR$accuracy)
