#Analysis and Investigation of The Geocoded, Irish Property Price Register
#Michael Dineen
#X16350523
#BSHTM4 -Data Analytics
#---------------------------------------------------------------------------

# Load Libraries 
library(scales)
library(dplyr)
library(plotly)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(leaflet)
library(haven)
library(safejoin)
library(randomForest)
library(caret)
library(ggplot2)
library(corrplot)
library(e1071) 
library(caretEnsemble)
library(mlbench)
library(ROSE)
library(digest)

#Ensure The working Directory is set to the local GitHub folder
setwd("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project")

#--------------------Original, Full Property Price Register  --------------------

#Original, Full Property Price Register import after downloading from the PRSA website
PPR_Full_DataSet <- read_excel("PPR_Full_DataSet.xlsm")

#Seperate Date into Year, Month, and Day 
PPR_Full_DataSet$Date <- PPR_Full_DataSet$`Date of Sale (dd/mm/yyyy)`
PPR_Full_DataSet <- PPR_Full_DataSet %>%
  separate(Date, sep="-", into = c("year", "month", "day"))

#year Analysis
plot(PPR_Full_DataSet$Price ~ PPR_Full_DataSet$year, col= rainbow(10), xlab="Year", ylab="Price", main="House Price From 2010 to 2019", pch=16)
options(scipen=5)

summary(PPR_Full_DataSet$Price)

#Distribution
scatter.smooth(PPR_Full_DataSet$Price, pch=2, main="Price Distribution", ylab="Price", col="green")
table(PPR_Full_DataSet$year)

# I Decided to take a Sample of 100,000 records
# as there is too much dqta for my machine's capability

set.seed(24897)
index <- sample(1:nrow(PPR_Full_DataSet), 100000, replace=FALSE)
PPR_Data <- PPR_Full_DataSet[index, ]

# It was noted that the Postal Code Section is not neccesary, Remove it,
PPR_Data <-PPR_Data[-3]
sapply(PPR_Data, function(x) sum(is.na(x)))

#Write File
write.csv(file="PPR_Raw_Sample",PPR_Data)
rm(PPR_Data)

# This sample was used in the Geocoding Process 
# The New Sample  was called PPR_Raw_Sample.csv
#**Sample sent to Spyder for Geocoding**


#-----------------Merging Input and output geocoding file -------------

#The Input and output file used in Geocoding has to be merged, as follows:

Raw_PPR_Sample <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/PPR_Raw_Sample.csv") #62,312 Output from Geocoding 
Geocoded_Sample <-  read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Geocoding_Fin.csv") #100000 sample

#Match Dataset Length
Raw_PPR_Sample <- Raw_PPR_Sample[1:62312, ] #Output size
colnames(Geocoded_Sample)[9] <- 'Address' #Match Coloumn name

#Remove Ireland From the Geocoded Input Address,
#This was Added to Improve Accuarcy in Geocoding and Prevents us from merging.
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
rm(Raw_PPR_Sample)

#Write File
write.csv(file="Merged_Data.csv",Merged_Data, row.names = F)
rm(Merged_Data)

# *****It was found that there were some outliers in geocoding, the data was brought to
#Tableau where the outliers were removed using a cut and exclude function
#All failed executions  were removed in Excel through Filtering - #189 failures in Total

#Reimported as Geocoded_23420.csv - Size 60,425 rows
Geocoded_PPR <- read.csv("Geocoded_23420.csv") 



#--------Census Data: Initial Import,Merge and Seperate of Data----------------

#****Using Excel, All values over €1 Million was Removed from Property Price Register
#Import CensusData
 SA1 <-  read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Census_Data1.csv")
 SA2 <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Census_Data2.csv")
 Avg_County_Price <- read.csv("/Users/mickdineen97/Desktop/FYP_Data/Average_Price.csv")
 
 #Merge
Census_Data <- merge(SA1, SA2, by ="GUID")
write.csv(file="Census_Data.csv", Census_Data)
rm(SA1,SA2)

#Mortgage and Loan Data creation
Census_Data$WithMortage <- Census_Data$T6_3_OMLH #Amount of People with Mortages 
Census_Data$FullyOwn <- Census_Data$T6_3_OOH #Amount of People with Mortages

Prop_1<- data_frame(Census_Data$WithMortage,Census_Data$COUNTYNAME,) 
Prop_2<- data.frame(Census_Data$FullyOwn,Census_Data$COUNTYNAME) 

Prop_1$County <- Prop_1$`Census_Data$COUNTYNAME`
Prop_2$County <- Prop_2$Census_Data.COUNTYNAME
Prop_1$Mortgage <- Prop_1$`Census_Data$WithMortage`
Prop_2$Own <- Prop_2$Census_Data.FullyOwn

# combine two dataset
Overview_Property <-cbind(Prop_1, Prop_2)
#Removal of Duplicates
Overview_Property$`Census_Data$COUNTYNAME`<-  NULL
Overview_Property$Census_Data.COUNTYNAME <- NULL
Overview_Property$`Census_Data$WithMortage` <- NULL 
Overview_Property$Census_Data.FullyOwn <-NULL
Overview_Property$County <- NULL

#Output File For Tableau
write.csv(file="Mortgage_Data.csv",Overview_Property, row.names = F)

#Population 
Pop_Data <- read_excel("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Last10.xlsx")
Population_Change <- read_excel("Population_Change.xlsx")

#-----------------------------------------

#Visualize using Leaflet a Map - Too Much Data, Completed in Tablaeu
#m <- leaflet()
#m <- addTiles(m)
#m <- addMarkers(m, lng=Geocoded_PPR$longitude, lat=Geocoded_PPR$latitude)
#m

#County Price Analysis
boxplot(Geocoded_PPR$Price ~ Geocoded_PPR$County, col=rainbow(26) , xlab="Year", ylab="Price", main="Average House Price by county") 
rm(sampleData)

#Distribution of Price
qqnorm(Geocoded_PPR$Price)
qqline(Geocoded_PPR$Price)

#population data
par(bg="light yellow")
plot(Pop_Data$State ~ Pop_Data$Year ,main="Ireland's Population Growth", xlab="Year", ylab="Population", col="dark green", pch=16)
lines.default(Pop_Data$State ~ Pop_Data$Year, lwd="5", col="Green")

#Historical Pop Change 
barplot(PopChange$State ~ PopChange$Year, col ="Hot Pink", xlab="Year", ylab="Population", main= "Population of Ireland: 1841-2016")

#Dublin
par(bg="light yellow")
barplot(Pop_Data$Dublin ~ Pop_Data$Year, col="blue", main="Dublin's increasing population",xlab="Year", ylab="Population")

#-------------------------Correlation- Average Price ~ Population Density-------------------------------

Corr_Den_Price <- read_sav("~/Desktop/DensityvsPrice.sav")

#Correlation Model - 
cor(Corr_Den_Price$Avg_House_Price, Corr_Den_Price$Population_Density) #0.72

#Plot Correlation
x <-  Corr_Den_Price$Avg_House_Price
y <- Corr_Den_Price$Population_Density

par(bg="light grey")
plot(x, y, main = "Correlation - All Ireland",
     xlab = "Average House Price", ylab = "Population Density",
     pch = 16, cex=1.4, col = "hotpink", frame = F)
abline(lm(y ~ x, data = Corr_Den_Price), lwd=2, col = "blue") 
#Dublin is Skewing the Results, Should be removed and retested 

#--------Removal of Dublin in SPSS----------

#Re-import Data
Corr_NoDublin<- read_sav("~/Desktop/Desnity_Sans_Dublin.sav")

#Correlation Model - Without Dublin
cor(Corr_NoDublin$Average_Price, Corr_NoDublin$Population_Density) #0.60

#Plot Correlation - Without Dublin
x2 <- Corr_NoDublin$Average_Price 
y2 <- Corr_NoDublin$Population_Density

par(bg="light yellow")
plot(x1, y1, main = "Correlation - Without Dublin",
     xlab = "Average House Price", ylab = "Population Density",
     pch = 16, cex=1.4, col = "blue", frame = F)
abline(lm(y1 ~ x1, data = Corr_NoDublin), lwd=2, col = "green") 

#---------------Time Series Analysis------------
ggplot(Geocoded_PPR, aes(year, colour = Description.of.Property)) +
  geom_freqpoly(binwidth = 1) + labs(title="Time Series Analysis - Sales over the last 10 years")

ggplot(Geocoded_PPR, aes(month, colour = Description.of.Property)) +
  geom_freqpoly(binwidth = 1) + labs(title="Time Series Analysis: Sales Across the Months")

table(PPR_train$Description.of.Property)

#------Statistical Tests------

#Conducted In SPSS and import here 
NewOld_Price <- read.csv("/Users/mickdineen97/Desktop/FYP_Data/StatTest1.csv")
boxplot(NewOld_Price$Price ~ NewOld_Price$Description.of.Property, col=rainbow(2), main="Price of New vs Second hand Prices", xlab = "Type", ylab = "Price", ylim=c(0,1000000))
rm(NewOld_Price)


#ultiple Linear Regression

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


#Check For Multicolinearity
m <- cor(Geocoded_PPR[c("Price", "month", "number_of_results", "year", "County_Number", "longitude","latitude")])

#Now Run The Models
MLR <- lm(Price ~ County + year + Not.Full.Market.Price + VAT.Exclusive + accuracy + number_of_results, data=Geocoded_PPR)
summary(MLR)

m <- round(cor(MLR), 2)

#Hotspot Diagram
corrplot(m, diag = FALSE, method = "color", order = "FPC", tl.srt = 90, col = rainbow(50))


#---- Machine Learning 


#---------------------***Random Forrest Model***-----------------------

#Uses Full Dataset as Sample
set.seed(2468)
sample_index <- sample(1:nrow(Geocoded_PPR), 1*nrow(Geocoded_PPR), replace = F) #Use full Sample
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
#str(PPR_train$DescriptionProp) #2
#str(PPR_test$DescriptionProp)#2


rf_All <- randomForest(Description.of.Property ~ Price + County + year +
                         Not.Full.Market.Price + latitude + longitude + 
                         accuracy + month + number_of_results,PPR_train) #VAT Excluded Removed

#Visualize Model - Variable Importance plot
varImpPlot(rf_All , col="Blue", main = "Random Forrest: Property Description", bg="Purple")

#Predictions and Confusion Model----------

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

#Naive Bayes Model - Withhout VAT
nb_desc <- naiveBayes(Description.of.Property ~ Price + County + year + Not.Full.Market.Price + latitude + longitude +accuracy + month + number_of_results + type, PPR_train)

#Naive Bayes Model -With VAT
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
PPR_train$DescriptionProp <- as.factor(PPR_train$DescriptionProp)
plot(PPR_train$DescriptionProp, col = rainbow(2), main="Removal of Class Imbalance")
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

svm.linear.predict <- predict(svm.fit1, PPR_test[,-16])
svm.linear.confusionmatrix <- confusionMatrix( data = svm.linear.predict, 
                                               reference =PPR_test[,16], positive = "1")

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

svm.rbf.predict <- predict(svm.rbf.fit, PPR_test[,-16])
svm.rbf.confusionmatrix <- confusionMatrix(data = svm.rbf.predict, reference = PPR_test[,16], positive = "1")

svm.rbf.confusionmatrix


#--------SVM RBF Random Tuned -----------

svm.random.control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  search = "random"
)
svm.rbf.random.fit <- train(
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

svm.rbf.random.predict <- predict(svm.rbf.random.fit,PPR_test[,-16])

svm.rbf.random.confusionmatrix <- confusionMatrix(data = svm.rbf.random.predict,reference = PPR_test[,16], positive = "1")

svm.rbf.random.confusionmatrix

#-------------Class Imbalance-------

plot(PPR_train$Description.of.Property, main= "ClassImbalance", col=rainbow(2))


