#Analysis of Datasets
#Michael Dineen
#FYP
install.packages('ggplot2')
install.packages('dplyr')
install.packages('plotly')
install.packages('scales')
install.packages("safejoin")
library(scales)
library(dplyr)
library(plotly)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(leaflet)
library(haven)
library(safejoin)

setwd("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project")
#---------Initial Import,Merge and Seperate of Data----------------

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
#Population Census
Pop_Data <- read_excel("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Last10.xlsx")
Population_Change <- read_excel("Population_Change.xlsx")

#Original PPR
PPR_Full_DataSet <- read_excel("PPR_Full_DataSet.xlsm")
View(PPR_Full_DataSet)

Dublin <- PPR_Full_DataSet$County=="Dublin"
rm(Dublin)
#Seperate Date into Year, Month, and Day 
PPR_Full_DataSet$Date <- PPR_Full_DataSet$`Date of Sale (dd/mm/yyyy)`
PPR_Full_DataSet <- PPR_Full_DataSet %>%
  separate(Date, sep="-", into = c("year", "month", "day"))

#-----Post Gecoding Import----

#Import New PPR
Geocoded_PPR <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Geocoded_PPR.csv")

#Seperate Date into Year, Month, and Day 
Geocoded_PPR <- Geocoded_PPR %>%
separate(Date.of.Sale..dd.mm.yyyy., sep="/", into = c("day", "month", "year"))
#-----------------------------------------

#Visualize using Leaflet a Map - Too Much Data, Completed in Tablaeu
#m <- leaflet()
#m <- addTiles(m)
#m <- addMarkers(m, lng=Geocoded_PPR$longitude, lat=Geocoded_PPR$latitude)
#m

# *****It was found that there were some outliers in geocoding, the data was brough to Tableau

#The outliers were removed 
#Reimport
Geocoded_PPR <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Geocoded_23420.csv")

#---------------------All Ireland Analysis- Pre-Geocoded Data --------------------
summary(Geocoded_PPR$Price)

#year Analysis
B <- plot(PPR_Full_DataSet$Price ~ PPR_Full_DataSet$year, col= rainbow(10), xlab="Year", ylab="Price", main="House Price From 2010 to 2019", pch=16)
options(scipen=5)
summary(PPR_Full_DataSet$Price)

#Distribution
scatter.smooth(PPR_Full_DataSet$Price, pch=2, main="Price Distribution", ylab="Price", col="green")
table(PPR_Full_DataSet$year)

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

par(bg="light yellow")
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
x1 <- Corr_NoDublin$Average_Price 
y1 <- Corr_NoDublin$Population_Density

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

#------Statistical Tests------

#Conducted In SPSS and import here 
NewOld_Price <- read.csv("/Users/mickdineen97/Desktop/FYP_Data/StatTest1.csv")
boxplot(NewOld_Price$Price ~ NewOld_Price$Description.of.Property, col=rainbow(2), main="Price of New vs Second hand Prices", xlab = "Type", ylab = "Price", ylim=c(0,1000000))
rm(NewOld_Price)



ggplot(PPR_train, aes(year, colour = Description.of.Property)) +
  geom_freqpoly(binwidth = 1) + labs(title="Time Series Analysis: Difference in Count of Property type")
