library(ggplot2)
install.packages('dplyr')
library(dplyr)
library(plotly)
install.packages('plotly')
library(hrbrthemes)
install.packages('scales')
library(scales)

# ----- Analysis of Merged_Data - Before Removal of Outliers

Geocoded_PPR <- read.csv("/Users/mickdineen97/Desktop/Thesis Related/Final_Year_Housing_Project/Merged_Data.csv")

boxplot(Geocoded_PPR$Price~ Geocoded_PPR$County, col= rainbow(6))

#Time Series Analyis
p <- Geocoded_PPR %>%
  ggplot( aes(x=Date.of.Sale..dd.mm.yyyy., y=Price)) +
  geom_area(fill="red", alpha=0.5) +
  geom_line(color="Blue") +
  ylab("bitcoin price ($)") + theme_bw()

p <- ggplotly(p)
