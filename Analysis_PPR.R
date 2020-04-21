#Analysis of Datasets
#Michael Dineen
#FYP
install.packages('ggplot2')
install.packages('dplyr')
install.packages('plotly')
install.packages('scales')
library(scales)
library(dplyr)
library(plotly)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(leaflet)

#---------Import,Merge and Seperate Data----------------
#****Using Excel, All values over €1 Million was Removed from Property Price Register
#Import CensusData
 SA1 <-  read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Census_Data1.csv")
 SA2 <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Census_Data2.csv")
 Avg_County_Price <- read.csv("/Users/mickdineen97/Desktop/FYP_Data/Average_Price.csv")
 #Merge
Census_Data <- merge(SA1, SA2, by ="GUID")
write.csv(file="Census_Data.csv", Census_Data)

#----

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
rm(SA1,SA2)
#---------------------All Ireland Analysis ---------------------
data.frame(Geocoded_PPR)
summary(Geocoded_PPR$Price)
#year Analysis
B <- boxplot(Geocoded_PPR$Price ~ Geocoded_PPR$year, col= rainbow(9), xlab="Year", ylab="Price", main="Average House Price From 2010 to 2019")

#County Price Analysis
C <- boxplot(Geocoded_PPR$Price ~ Geocoded_PPR$County, col= rainbow(9), xlab="Year", ylab="Price", main="Average House Price by county")

#Average House Price
barplot(Avg_County_Price$Average_Price ~ Avg_County_Price$County)








       

