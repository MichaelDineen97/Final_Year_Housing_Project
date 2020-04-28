#Michael Dineen,
#X16350523,
#BSHTM4,
#Final Year Project. 
setwd("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project")
#-------------------Start---------------
#Libraries
install.packages("randomForest")
install.packages("caretEnsemble")
install.packages("corrplot")
install.packages("ROSE")
library(randomForest)
library(caret)
library(ggplot2)
library(corrplot)
library(e1071) 
library(caretEnsemble)
library(mlbench)
library(ROSE)

#Import and Review Data before Running Machine Learning models
Geocoded_PPR <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Geocoded_23420.csv")

Geocoded_PPR[Geocoded_PPR == ''] <- NA #Turn all blanks to NA
sapply(Geocoded_PPR, function(x) sum(is.na(x))) #Post Code has NA's

#Remove All NA's
Geocoded_PPR$postcode <- NULL
sapply(Geocoded_PPR, function(x) sum(is.na(x))) #Post Code has NA's
#NA's Removed


#---------------------***Random Forrest Model***-----------------------

#Uses Full Dataset as Sample
set.seed(2468)
sample_index <- sample(1:nrow(Geocoded_PPR), 1*nrow(Geocoded_PPR), replace = F) 
sampleData <- Geocoded_PPR[sample_index, ]

# The 0.8*nrow(data) is getting a sample of 80% of the data
set.seed(1357)
rf_sample_index <- sample(1:nrow(sampleData), 0.8*nrow(sampleData), replace = F)

#Seperate into train and test, 
PPR_train <- sampleData[rf_sample_index, ] # Storing the 80% of data in train
PPR_test <- sampleData[-rf_sample_index, ] # Storing the remaining 20% to test

#Ensure the Length of the Two variables are uniform.
str(PPR_train$Description.of.Property) #2
str(PPR_test$Description.of.Property)#2
str(PPR_train$DescriptionProp) #2
str(PPR_test$DescriptionProp)#2

#Dependant Variable - Vehicle
#rf_Desc_Property <- randomForest(Description.of.Property ~ Price + County + year + Not.Full.Market.Price, PPR_train)

rf_All <- randomForest(Description.of.Property ~ Price + County + year +
                         Not.Full.Market.Price + latitude + longitude + 
                         accuracy + month + VAT.Exclusive + number_of_results,PPR_train)

#Visualize Model - Variable Importance plot
varImpPlot(rf_All , col="Blue", main = "Random Forrest: Property Description", bg="Purple")

#Predictions and Confusion Model----------

#PredictionRF_Desc <- predict(rf_Desc_Property, PPR_test)
#confusionMatrix(PredictionRF_Desc, PPR_test$Description.of.Property) 

PredictionRF_ALL <- predict(rf_All, PPR_test)
confusionMatrix(PredictionRF_ALL, PPR_test$Description.of.Property) 

#----------------------***Naive Bayes***-----------------------------

# Create Sample. Test and Train Data.
set.seed(20) 
sample_index <- sample(1:nrow(Geocoded_PPR), 1*nrow(Geocoded_PPR), replace = F) 

set.seed(20)
rf_sample_index <- sample(1:nrow(sampleData), 0.8*nrow(sampleData), replace = F)

#Seperate into train and test, 
PPR_train <- sampleData[rf_sample_index, ] # Storing the 80% of data in train
PPR_test <- sampleData[-rf_sample_index, ] # Storing the remaining 20% to test

#Ensure the Length of the Two variables are uniform.
str(PPR_train$Description.of.Property) #2
str(PPR_test$Description.of.Property)#2

#Naive Bayes Model
nb_desc <- naiveBayes(Description.of.Property ~ Price + County + year + Not.Full.Market.Price + latitude + longitude +accuracy + month + number_of_results + type, PPR_train)

nb_desc_VAT <- naiveBayes(Description.of.Property ~ Price + VAT.Exclusive +County + year + Not.Full.Market.Price + latitude + longitude + accuracy + month + number_of_results + type, PPR_train)

#Build Prediction Model
nb_pred <- predict(nb_desc, PPR_test)
nb_pred_VAT <- predict(nb_desc_VAT, PPR_test)

#Confusion Matrix
confusionMatrix(nb_pred, PPR_test$Description.of.Property)
confusionMatrix(nb_pred_VAT, PPR_test$Description.of.Property)




#--------------***Support Vector Machine***------------------
library(mlbench)

#Remove uneeded Variables 
Geocoded_PPR$Address <- NULL
Geocoded_PPR$X <- NULL
Geocoded_PPR$formatted_address <- NULL
Geocoded_PPR$google_place_id <- NULL

#New Coloumn for Description of Property to INT for SVM
Geocoded_PPR$DescriptionProp <- Geocoded_PPR$Description.of.Property
Geocoded_PPR$DescriptionProp <- gsub('New Dwelling house /Apartment','1',Geocoded_PPR$DescriptionProp)
Geocoded_PPR$DescriptionProp <- gsub('Second-Hand Dwelling house /Apartment','2',Geocoded_PPR$DescriptionProp)
Geocoded_PPR$Description.of.Property <- NULL


#Convert County to Numeric Values 1-26
Geocoded_PPR$County_Number <- Geocoded_PPR$County
Geocoded_PPR$County_Number <- gsub('Dublin','1',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Cork','2',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Galway','3',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Kildare','4',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Meath','5',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Limerick','6',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Tipperary','7',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Donegal','8',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Wexford','9',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Kerry','10',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Wicklow','11',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Mayo','12',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Louth','13',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Clare','14',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Waterford','15',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Kilkenny','16',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Westmeath','17',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Laois','18',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Offaly','19',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Cavan','20',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Sligo','21',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Roscommon','22',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Monaghan','23',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Carlow','24',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Longford','25',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- gsub('Leitrim','26',Geocoded_PPR$County_Number)
Geocoded_PPR$County_Number <- as.integer(Geocoded_PPR$County_Number)


#Get Raw Sample, SVM uses a smaller sample size due to the high computational Requirements. 
set.seed(1234)
sample_index <- sample(1:nrow(Geocoded_PPR), 1000, replace = F) 
sampleData <- Geocoded_PPR[sample_index, ]

# The 0.8*nrow(data) is getting a sample of 80% of the data
set.seed(321)
rf_sample_index <- sample(1:nrow(sampleData), 0.8*nrow(sampleData), replace = F)

#Seperate into train and test, 
PPR_train <- sampleData[rf_sample_index, ] # Storing the 80% of data in train
PPR_test <- sampleData[-rf_sample_index, ] # Storing the remaining 20% to test

PPR_train$DescriptionProp <- as.factor(PPR_train$DescriptionProp)
PPR_test$DescriptionProp <- as.factor(PPR_test$DescriptionProp)
#Class Imbalance- Fix 
table(PPR_train$DescriptionProp) #Signifianct Class Imbalance

#Use Under and over sampling

PPR_train <- ROSE(DescriptionProp ~ Price + number_of_results + month + longitude + latitude + accuracy + Price + County + year, data= PPR_train)$data

table(PPR_train$DescriptionProp)


#---SVM1 - SVM Linear Grid Tuned

Cost = 2^c(1:8)
print(Cost)


svm.control = trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)
svm.linear.grid <- expand.grid(C = Cost)

svm.fit1 <- train(DescriptionProp ~., data = PPR_train, method = "svmLinear",
                  trControl = svm.control,preProc = c("center", "scale","nzv"), 
                  verbose = FALSE, tuneGrid = svm.linear.grid)

svm.fit1

#----

Cost = 2^seq(0,2,0.1)

svm.linear.grid2 <- expand.grid(C = Cost)

svm.fit2 <- train( DescriptionProp ~., data =PPR_train ,
                    method = "svmLinear", trControl = svm.control, 
                   preProc = c("center", "scale","nzv"),
                   verbose = FALSE, tuneGrid = svm.linear.grid2)

svm.fit2

#----

svm.linear.predict <- predict(svm.fit1, PPR_test[,-15])
svm.linear.confusionmatrix <- confusionMatrix( data = svm.linear.predict, 
                               reference =PPR_test[,15], positive = "1")

svm.linear.confusionmatrix

#-------------SVM RBF Grid Tuned--------------

svm.rbf.grid <- expand.grid(
  C = 2^seq(3,5,0.1),
  sigma = 2^c(-25, -20, -15,-10, -5, 0)
)
svm.rbf.fit <- train( 
  DescriptionProp ~ ., 
  data =PPR_train ,
  method = "svmRadial", 
  trControl = svm.control, 
  verbose = FALSE,
  preProc = c("center", "scale", "nzv"), 
  tuneGrid = svm.rbf.grid
)
svm.rbf.fit$bestTune 

#----

svm.rbf.predict <- predict(svm.rbf.fit, PPR_test[,-15])
svm.rbf.confusionmatrix <- confusionMatrix(data = svm.rbf.predict, reference = PPR_test[,15], positive = "1")

svm.rbf.confusionmatrix


#--------SVM RBF Random Tuned -----------

svm.random.control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  search = "random"
)
svm.rbf.random.fit <- train( # Train a model
  DescriptionProp ~., 
data = PPR_train, 
  method = "svmRadial",
  trControl = svm.control, 
  verbose = FALSE,
  preProc = c("center", "scale", "nzv"),
  tuneLength = 60 
)

svm.rbf.random.fit$bestTune

#----

svm.rbf.random.predict <- predict(svm.rbf.random.fit,PPR_test[,-15])

svm.rbf.random.confusionmatrix <- confusionMatrix(data = svm.rbf.random.predict,reference = PPR_test[,15], positive = "1")

svm.rbf.random.confusionmatrix

#-------------Class Imbalance-------

plot(PPR_train$Description.of.Property, main= "ClassImbalance", col=rainbow(2))

#---------------Multiple Linear Regression------------

cor(Geocoded_PPR[c("Price", "latitude",
                   "longitude", "month", "number_of_results", "day", "year"  )])
#Now Run The Models

MLR <- lm(Price ~ County + year +
    Not.Full.Market.Price + latitude + longitude + 
    accuracy + month + number_of_results +VAT.Exclusive, data=Geocoded_PPR)
summary(MLR)






