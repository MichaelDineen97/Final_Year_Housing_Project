



#----Import All Data---------


PPR_Full_DataSet <- read_excel("~/Desktop/Thesis Related/PPR_Full_DataSet.xlsm")
        View(PPR_Full_DataSet)

Population_Change <- read_excel("~/Desktop/Thesis Related/Population_Change.xlsx")
        View(Population_Change)  
 
# outputAgain <- read_csv("Desktop/Thesis Related/outputAgain.csv")
 
#-----------Get Sample of 100000 Records using Sample function------------
 
index <- sample(1:nrow(PPR_Full_DataSet), 100000, replace=FALSE)
 PPR_Data <- PPR_Full_DataSet[index, ]
 View(PPR_Data)
 
 #---------Write file for geocoding in Python using Google Maps API

 write.csv(file="PPR_Address_Sample.csv", PPR_Data, row.names = F)
 
 PopChange <- Population_Change
 rm(Population_Change)
 

 
   