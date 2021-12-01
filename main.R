# Copyright ...

#' Q1 - Sony Interactive Entertainment produces games with the highest reviews.
#' Subquestion - Sony's action games are the best rated among all of their games.
#' Q2 - The top rated games are released in winter.
#' Q3 - North America sells the most videogames.
#' Subquestion - XBox is the most popular platform in north america all-time.
#' Subquestion - Sports games are the most played games on XBox all-time.

# Part 1
# Fetch data
# Q1/Q2 website: page_url = 'https://www.metacritic.com/browse/games/score/metascore/all/ps4/filtered?view=detailed'
# Q3 website: https://www.kaggle.com/vinodsunny1/insight-s-of-gaming-world/data
# Download data everytime we run and don't save it. 

# FINISH ALL CODE FOR DECEMBER 8
function fetchTop100Games() {
  # TODO
  # Create dataframe for the top 100 games and their respective reviews
}

function fetchNorthAmericanGameSales() {
  # TODO
  # Create a dataframe that contains the file shown on the website
}

# Part 2
# Do data analysis

#' Q1 - Relative comparison of reviews based on count
#' Need to normalize data
#'
#' Q2 - Also normalize
#'
#' Q3 - Normalize for year 
#' #Main Question: North America makes the most revenue from games
#Subquestion 1: XBOx is the most used console in North America 
#Subqeustion 2: Sports games are the most popular in north america 

install.packages("dplyr")
install.packages("tidyverse")
install.packages("XML")
install.packages("rvest")
install.packages("tidyverse")
install.packages("tidyr")


library(tidyverse)
library(XML)
library(rvest)
library(dplyr)
library(tidyr)

# get and set working directory 
working_directory <- getwd() 
setwd(working_directory)
# load vgsales.csv
vgsales <- read.csv(unz("archive.zip", "vgsales.csv"),
                    stringsAsFactors = FALSE)

# subset data by grouping Platforms with the same name and suming the Global_Sales 
final.df <- data.frame(vgsales %>% 
                         group_by(Platform) %>% 
                         summarise(GlobalSales = sum(Global_Sales)))

# create new data frame to hold data for similar Platforms (Example: PS and PS2)
group <- data.frame(matrix(ncol = 2, nrow = 6))
colnames(group) <- c("Platform", "GlobalSales")
# create a vector to input for each row in the Platform column
platforms <- c("Xbox", "PlayStation", "DS", "Wii", "Nintendo", "GameBoy")

# create a vector to input for each row in the Global Sales
# use the with and sum function to group and sum the Global_Sales of similar Platforms 
sales <- c(with(final.df, sum(GlobalSales[Platform == 'XB' | Platform == 'X360' | Platform == 'XOne'])),
           with(final.df, sum(GlobalSales[Platform == 'PS' | Platform == 'PS2' | Platform == 'PS3' | Platform == 'PS4'| Platform == 'PSP' | Platform == 'PSV'])),
           with(final.df, sum(GlobalSales[Platform == '3DO' | Platform == '3DS' | Platform == 'DS'])),
           with(final.df, sum(GlobalSales[Platform == 'Wii' | Platform == 'WiiU'])),
           with(final.df, sum(GlobalSales[Platform == 'GB' | Platform == 'GBA'])),
           with(final.df, sum(GlobalSales[Platform == 'N64' | Platform == 'NG' | Platform == 'NES'])))
# input the values into the data frame 
group$Platform <- platforms
group$GlobalSales <- sales

# combine the data frames 
final.df <- rbind(group, final.df)

# remove irrelevant Platforms 
final.df<-final.df[!(final.df$Platform =="XB" | final.df$Platform=="X360" | final.df$Platform=="XOne"
                     | final.df$Platform=="PS" | final.df$Platform=="PS2" | final.df$Platform=="PS3" | final.df$Platform=="PS4" | final.df$Platform=="PSP" | final.df$Platform=="PSV"
                     | final.df$Platform=="3DO" | final.df$Platform=="3DS" | final.df$Platform=="DS" 
                     | final.df$Platform=="Wii"| final.df$Platform=="WiiU"
                     | final.df$Platform=="GB"| final.df$Platform=="GBA"
                     | final.df$Platform=="N64" | final.df$Platform=="NG" | final.df$Platform=="NES"),]

p <- ggplot(data=final.df, aes(x=Platform,y=GlobalSales, fill=Platform)) + geom_bar(stat="identity",position="dodge")
p  + labs(x="Platform", y="Global Sales", main = "Platform vs Global Sales") 


# T TEST between the two gameboy consoles -  
# The two-sample t-test (also known as the independent samples t-test) is a method used to test whether the unknown population means of two groups are equal or not.
Wiiu.df <- filter(gameboy.df, Platform == "WiiU")
PS.df <- filter(vgsales, Platform == "PS")
var(PS.df$Global_Sales)
var(WiiU.df$Global_Sales)
#found the variance to ensure that they are the same and we can use the two sample t test 

target_gameboy <- c("PS", "WiiU")
gameboy.df <- filter(vgsales, Platform %in% target_gameboy)


N <- nrow(gameboy.df)
myTest <- t.test(gameboy.df$Global_Sales ~ gameboy.df$Platform)
round(myTest$p.value, digits = 4) #basically 0 so we 

tcrit=qt(0.025, df=(N-1))
dum=seq(-3.5, 3.5, length=10^4)#For the plot

plot(dum, dt(dum, df=(N-1)), type='l', xlab='t', ylab='f(t)')
abline(v=myTest$statistic, lty=2)
abline(v=tcrit, col='red', lty=2)
abline(v=-tcrit, col='red', lty=2)

# Our idea is that the mean test scores for the two platforms are not the same We want to know if the mean score for PS is different from WiiU.
#?? = .05
# the p value is greater than 0.05 and the t test is within the ranges of the critical value so we fail to reject the hyphtesis. 
# https://stackoverflow.com/questions/36508020/can-r-visualize-the-t-test-or-other-hypothesis-test-results?rq=1

# If the absolute value of the t-value is greater than the critical value, you reject the null hypothesis. If the absolute value of the t-value is less than the critical value, you fail to reject the null hypothesis. 

########################################## SECOND PART ##########################################
# Does type of genre significantly affect NA Sales 
fit <- lm(NA_Sales ~ Genre, data = vgsales) # regress final raw score on all variables in the data set

summary(fit) # Gives regression summary output

Genre = c(as.character(unlist(fit$xlevels)))
Variable=names(coefficients(fit))
Estimated_Coefficients = as.numeric(coefficients(fit))
PValue = round(as.numeric(summary(fit)$coefficients[,  4]), digits = 5)

regression = data.frame(Variable, Genre, Estimated_Coefficients, PValue)

# expected difference in the response variable between one level of the variable 
# and the base level (the one selected for the intercept term), all else equal

# action is the base level 
# any values where there is no statistically significant difference in Na_Sales for Action and Genre
# the p-value will be larger than 5% 

# otherwise it would be smaller than 5% 
# Each line here is a type of two sample t-test comparing each genre to the base 

# if nothing else was being used to predict NA Sales from the P value of 
# F-stat of our model, we would see that the genre is a significant predictor 

# Graph 
ggplot(vgsales, aes(x=Genre,y=NA_Sales, color=Genre)) + # asking it to set the color by the variable "group" is what makes it draw three different lines
  geom_line(lwd=2) + 
  labs(x="Genre", y="NA Sales", title="Genre vs NA Sales") 

# Part 3
# Do data vis

#' Q1: Make bar chart comparing average score for each publisher
#' Subquestion: Chart of average ratings for each of sony's game genres
#' 
#' Q2: Make a stacked vertical bar chart
#' 
#' Q3: Sales of games all-time per game - World interactive chart
#' Subquestion 1 - Sales for each platform in north america
#' Subquestion 2 - Sales for each genre on XBox
