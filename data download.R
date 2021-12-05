#Main Question 3 : North America makes the most revenue from games on average. 
  #Subquestion 1: XBOx is the most used console in North America 
  #Subqeustion 2: Sports games are the most popular in north america 
  #Subquestion 2.5: which variable affects sales in north america 
#nova test (same as t test but for multiple samples, null hypothesis has to be that means are roughly equivalent)

library(rvest)
library(dplyr)
library(ggplot2)


#____________________________Data Collection____________________________________

#Scrape data from website using rvest package
gaming.data <- read_html("https://vgsales.fandom.com/wiki/Video_game_industry")
tables <- gaming.data %>% html_table(fill = TRUE)
#Create dataframe from scraped data
data <- tables[[1]]
world.revenue <- tables[[2]]

#____________________________Data Cleanup_______________________________________

#Remove "Notes" column as it provides no data
data <- select(data, -c("Note(s)"))
#Rename first column for ease of use, verify data is dataframe
colnames(data)[1] <- "country"
is.data.frame(data)

#Filter for continents only 
#There is no Africa or antartica, and north america is split into north and latin 
continents <- c("Asia-Pacific", "Europe", "Australia", "North America", "Latin America", "Middle East")
revenue.data <- filter(data, country %in% continents) 

#Fix variables within dataframe which contain imperfections
#Renamed continents, removed "billions", removed "brackets
revenue.data$country <- c("Asia", "Australia", "Europe", "North America", "Latin America", "Middle East")
revenue.data$`2013` <- c("49.623", "2", "20", "22.8", "3.9", "2.6")
revenue.data$`2012` <- c("44.063", "1.16", "21.3", "20.7", "5.4", "2.6")
revenue.data$`2011` <- c("42.358", "1.5", "21.3", "20.7", "5.4", "1.983")
revenue.data$`2010` <- c("38.77", "1.67", "20.66", "20.49", "4.74", "1.2")

#Column "country" into rownames for dataframe
#Removed country column entirely
revenue.data<- select(revenue.data, -c("country"))
#Renamed rownames for continents
rownames <- rownames(revenue.data)
rownames(revenue.data) <- c("Asia", "Australia", "Europe", "North America", "Latin America", "Middle East")

#Data is cleaned and ready to use

#_____________________________Data Manipulation_________________________________

#Using Analysis of Variance (ANOVA) hypothesis testing
#ANOVA analyzes a dataset with two or more groups to check for relationships between groups.
#We wanted to check if there is relationships between continents
#Null Hypothesis: There is no real difference between groups, F-statistic will be closer to 1

#Data is in character format, need to convert to numeric
revenue.data[] <- lapply(revenue.data, as.numeric)
str(revenue.data)

#Calculating Mean value to Answer Main Question 1, adding mean column to data
revenue.data$Mean <- rowMeans(revenue.data)
rownames <- rownames(revenue.data)
rownames(revenue.data) <- c("Asia", "Australia", "Europe", "North America", "Latin America", "Middle East")
summary(revenue.data)

#Creating plot to look at mean distribution 
qplot(x = rownames, y = revenue.data$Mean)

#Creating proper conditions to run ANOVA testing
#Change format of dataset, transpose column to row so we test for continents not year
#stack data to be able to run ANOVA test
df_transpose = t(revenue.data)
transpose <- data.frame(df_transpose)
stacked.data <- stack(transpose)
Anova.results <- aov(values ~ ind, data = stacked.data)

#ANOVA Test Results:
print(summary(Anova.results))
# Df Sum Sq Mean Sq F value Pr(>F)    
# ind          5   6711  1342.2   465.2 <2e-16 ***
#   Residuals   24     69     2.9                   
# ---

#F-statistic value = 465.2 
#This means that the different continents have no relationship between each other
#The revenue each continent makes is unrelated to other continents
#The Null Hypothesis is TRUE

#Answer to Question 3: North America does not make the most gaming revenue, Asia makes the most. 
