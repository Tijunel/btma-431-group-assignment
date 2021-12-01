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
#Subquestion 1: Xbox is the most used console in North America 
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

# set the working directory to where you downloaded archive.zip 
setwd("C:/Users/jessi/Downloads")
# unzip and load vgsales.csv into the environment. 
vgsales <- read.csv(unz("archive.zip", "vgsales.csv"),
                    stringsAsFactors = FALSE)

# Subquestion 2: WiiU and PS has the same mean. 
# create a new dataframe including both PS and WiiU 
target <- c("PS", "WiiU")
vgsales.filtered <- filter(vgsales, Platform %in% target)

# filter the data to only show the necessary fields - Name, Platform, Global Sales
drops <- c("Rank","Year", "Genre", "Publisher", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
vgsales.filtered <- vgsales.filtered[ , !(names(vgsales.filtered) %in% drops)]

# Rename Columns 
vgsales.filtered <- vgsales.filtered %>% 
                      rename(
                        'Game Name' = Name,
                        'Global Sales' = Global_Sales
                      )

# TWO SAMPLE T-TEST BETWEEN PS AND WIIU 

# Also known as the indepedent samples t-test 
# We chose the two-sample t-test to test whether the means of the two platforms (WiiU and PS) are equal or not.
# Additionally, as shown below the variance of the two are almost identical making this test appropriate to use 

# create a dataframe to filter for each platform 
WiiU.df <- filter(vgsales, Platform == "WiiU")
PS.df <- filter(vgsales, Platform == "PS")
# show that the variance are the same to ensure we meet the assumptions of the t-test 
var(PS.df$Global_Sales)
var(WiiU.df$Global_Sales)

# get number of samples 
N <- nrow(vgsales.filtered)

# do the hypothesis test comparing the mean global sales of PS and WiiU
hypothesis_test <- t.test(vgsales.filtered$'Global Sales' ~ vgsales.filtered$Platform)

# get and store the p-value of the test to use in our analysis
ttest.pvalue <- round(hypothesis_test$p.value, digits = 4) 

# find the critical value given a 95% confidence interval 
tcrit=qt(0.025, df=(N-1))

# create the range for the plot graph
dum=seq(-3.5, 3.5, length=10^4)

# Plot the critical values, t-test value, and the curve. 
plot(dum, dt(dum, df=(N-1)), type='l', main = 'Probability Distribution Curve', xlab='t', ylab='f(t)', cex.main = 0.9,   font.main= 4,)
abline(v=hypothesis_test$statistic, lty=2) # t test value 
abline(v=tcrit, col='red', lty=2) # critical value one 
abline(v=-tcrit, col='red', lty=2) # critical value two 

# code retrieved from "https://stackoverflow.com/questions/36508020/can-r-visualize-the-t-test-or-other-hypothesis-test-results?rq=1"


# Subquestion 3: Sports Genre has the most statistically significant different in NA_Sales for Action Genre.

# Filter vgsales to include only the relevant columns (Name, Genre, NA_Sales)
drops.3 <- c("Rank","Year", "Platform", "Publisher", "Global_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
vgsales.filtered.3 <- vgsales[ , !(names(vgsales) %in% drops.3)]

# Rename Columns 
vgsales.filtered.3 <- vgsales.filtered.3 %>% 
  rename(
    'Game Name' = Name,
  )

# CATEGORICAL REGRESSION MODEL FOR GENRE AND NORTH AMERICA SALES 

# fit the linear regression between different types of Genre and the vgsales dataframe we just filteres 
# R has chosen the "Action" Genre to be the base. 
fit <- lm(NA_Sales ~ Genre, data = vgsales.filtered.3)

summary(fit) # Gives regression summary output

# scrap the Genre from the regression summary 
Genre = c(as.character(unlist(fit$xlevels)))
# scrap the variable from the regression summary 
Variable=names(coefficients(fit))
# scrap the Estimated Coefficient from the regression summary 
Estimated_Coefficients = as.numeric(coefficients(fit))
# scrap the P-Value from the regression summary 
PValue = round(as.numeric(summary(fit)$coefficients[,  4]), digits = 5)

# create a data frame to hold the important factors in the regression summary
regression = data.frame(Variable, Genre, Estimated_Coefficients, PValue)

# Reference: "https://www.r-bloggers.com/2013/01/regression-on-categorical-variables/"

# Graphs the NA Sales value for each Genre
ggplot(vgsales, aes(x=Genre,y=NA_Sales, color=Genre)) + 
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
