# Michael Dineen
# BSHTM4, Data Analysis
# Final Year Project
# Tutor- Dr. Eugene O'loughlin

install.packages("leaflet")

library(readxl)
library(ggplot2)
library(leaflet)
library(randomForest)
library(caret)
library(iplot)
#------------------Import All Data-------------------------

setwd("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project")

#Original Dataset
PPR_Full_DataSet <- read_excel("PPR_Full_DataSet.xlsm")
View(PPR_Full_DataSet)

#Population Census Data
Population_Change <- read_excel("Population_Change.xlsx")
View(Population_Change)

PPR_Cor <-read_excel("PPR_Corr.xlsx")

#-----------Intial Test of 45,000 records-----------------------

outputAgain <- read_csv("Desktop/Thesis Related/outputAgain.csv")
 
#-----------Get Sample of 100000 Records using Sample function------------

set.seed(24897)
index <- sample(1:nrow(PPR_Full_DataSet), 100000, replace=FALSE)
PPR_Data <- PPR_Full_DataSet[index, ]
View(PPR_Data)

 #---------Write file for geocoding in Python using Google Maps API------

 write.csv(file="PPR_Semester_2.csv", PPR_Data, row.names = F)
 PopChange <- Population_Change
 rm(Population_Change)
 
 #--------------Claean sample Data Before Geocoding----------
 
 
 sem2_ppr <- read.csv("~/Desktop/Thesis Related/Geocoding/PPR_Semester_2.csv")
 sapply(sem2_ppr, function(x) sum(is.na(x)))
 
    # It was noted that the Postal Code Section is not neccesary, Remove it,
    sem2_ppr <- sem2_ppr[-4]
    sapply(sem2_ppr, function(x) sum(is.na(x)))
    #Some ID's were NA's, Export to excel to fix,
    write.csv(file="Sem2.csv",sem2_ppr, row.names = F)
    #Update Data
    sem2_ppr <- read.csv("~/Desktop/Thesis Related/Final_Year_Housing_Project/sem2.csv")
 
       #No NA'S,
#-----------------------------Merge Test------------------------
    
    Geo_samp <-  read.csv("~/Desktop/Thesis Related/Geocoding/Geo_PPR_OverNighttest.csv")
    Geo_ppr<- merge(Geo_samp, sem2_ppr,by.x = "input_string", by.y = "Address")
    write.csv(file="LOOK.csv",Geo_ppr, row.names = F)

    #Visualize using Leaflet a Map
    m <- leaflet()
    m <- addTiles(m)
    m <- addMarkers(m, lng=Geo_ppr$longitude, lat=Geo_ppr$latitude, popup="The birthplace of R")
    m
    sapply(Geo_ppr, function(x) sum(is.na(x)))
#-----------------------------------------------------------------
    
    rf_sample_index <- sample(1:nrow(Road_Train_Test), 0.8*nrow(Road_Train_Test), replace = F)
 
#--------------------Midpoint-Visuals--------------------------------------
 
        #--Change in pop--
 barplot(PopChange$State ~ PopChange$Year, col ="Hot Pink", xlab="Year", ylab="Population")
 
        #--recent years--
 lines.default(Last10$State ~ Last10$Year, col ="blue", xlab="Year", ylab="Population", 
               lwd=10, lines.default(bg="Black"))

 #--------Correlation----------- 
 
Corel <- PPR_Correlation 
 rm( PPR_Correlation )
 
 cor(Corel$County, Corel$Price)   # r = 0.4
 cor(Corel$Price, Corel$County) 

 x <- Corel$County
 y <- Corel$Price
 
 # Change point shape (pch = 19) and remove frame.
 plot(x, y, main = "Correlation",
      xlab = "Price", ylab = "County Number",
      pch = 1, col = "Green", frame = FALSE)
 # Add regression line
 plot(x, y, main = "Correlation",
      xlab = "County Number", ylab = "Price",
      pch = 16, col = "Green", frame = FALSE)
 abline(lm(y ~ x, data = Corel), lwd=10, col = "Red")
 
 

head(Corel)
 cor(Corel)
 plot(Corel)
 
  linearModel <- lm(Corel$Price ~ Corel$County, data =Corel)
        plot(Corel)
 
 
 
 
 
   