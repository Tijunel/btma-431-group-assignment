# Copyright Justin Tijunelis, Luke Fouad, Terrin Mathews, Jessica Huong, Faith Nayko
# Completed December 5, 2021


#' ### Question 4 #################################################################
#' # Part 2
#' # Do data analysis
#' 
#' #' Q1 - Relative comparison of reviews based on count
#' #' Need to normalize data
#' #'
#' #' Q2 - Also normalize
#' #'
#' #' Q3 - Normalize for year
#' #' #Main Question: North America makes the most revenue from games
#' #Subquestion 1: Xbox is the most used console in North America
#' #Subqeustion 2: Sports games are the most popular in north america
#' 
#' # set the working directory to where you downloaded archive.zip
#' # unzip and load vgsales.csv into the environment.
#' vgsales <- read.csv(unz("archive.zip", "vgsales.csv"), stringsAsFactors = FALSE)
#' 
#' # Subquestion 2: WiiU and PS has the same mean.
#' # create a new dataframe including both PS and WiiU
#' target <- c("PS", "WiiU")
#' vgsales.filtered <- filter(vgsales, Platform %in% target)
#' 
#' # filter the data to only show the necessary fields - Name, Platform, Global Sales
#' drops <- c("Rank","Year", "Genre", "Publisher", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
#' vgsales.filtered <- vgsales.filtered[ , !(names(vgsales.filtered) %in% drops)]
#' 
#' # Rename Columns
#' vgsales.filtered <- vgsales.filtered %>%
#'                       rename(
#'                         'Game Name' = Name,
#'                         'Global Sales' = Global_Sales
#'                       )
#' 
#' # TWO SAMPLE T-TEST BETWEEN PS AND WIIU
#' 
#' # Also known as the indepedent samples t-test
#' # We chose the two-sample t-test to test whether the means of the two platforms (WiiU and PS) are equal or not.
#' # Additionally, as shown below the variance of the two are almost identical making this test appropriate to use
#' 
#' # create a dataframe to filter for each platform
#' WiiU.df <- filter(vgsales, Platform == "WiiU")
#' PS.df <- filter(vgsales, Platform == "PS")
#' # show that the variance are the same to ensure we meet the assumptions of the t-test
#' var(PS.df$Global_Sales)
#' var(WiiU.df$Global_Sales)
#' 
#' # get number of samples
#' N <- nrow(vgsales.filtered)
#' 
#' # do the hypothesis test comparing the mean global sales of PS and WiiU
#' hypothesis_test <- t.test(vgsales.filtered$'Global Sales' ~ vgsales.filtered$Platform)
#' 
#' # get and store the p-value of the test to use in our analysis
#' ttest.pvalue <- round(hypothesis_test$p.value, digits = 4)
#' 
#' # find the critical value given a 95% confidence interval
#' tcrit=qt(0.025, df=(N-1))
#' 
#' # create the range for the plot graph
#' dum=seq(-3.5, 3.5, length=10^4)
#' 
#' # Plot the critical values, t-test value, and the curve.
#' plot(dum, dt(dum, df=(N-1)), type='l', main = 'Probability Distribution Curve', xlab='t', ylab='f(t)', cex.main = 0.9,   font.main= 4,)
#' abline(v=hypothesis_test$statistic, lty=2) # t test value
#' abline(v=tcrit, col='red', lty=2) # critical value one
#' abline(v=-tcrit, col='red', lty=2) # critical value two
#' 
#' # code retrieved from "https://stackoverflow.com/questions/36508020/can-r-visualize-the-t-test-or-other-hypothesis-test-results?rq=1"
#' 
#' 
#' # Subquestion 3: Sports Genre has the most statistically significant different in NA_Sales for Action Genre.
#' 
#' # Filter vgsales to include only the relevant columns (Name, Genre, NA_Sales)
#' drops.3 <- c("Rank","Year", "Platform", "Publisher", "Global_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
#' vgsales.filtered.3 <- vgsales[ , !(names(vgsales) %in% drops.3)]
#' 
#' # Rename Columns
#' vgsales.filtered.3 <- vgsales.filtered.3 %>%
#'   rename(
#'     'Game Name' = Name,
#'   )
#' 
#' # CATEGORICAL REGRESSION MODEL FOR GENRE AND NORTH AMERICA SALES
#' 
#' # fit the linear regression between different types of Genre and the vgsales dataframe we just filteres
#' # R has chosen the "Action" Genre to be the base.
#' fit <- lm(NA_Sales ~ Genre, data = vgsales.filtered.3)
#' 
#' summary(fit) # Gives regression summary output
#' 
#' # scrap the Genre from the regression summary
#' Genre = c(as.character(unlist(fit$xlevels)))
#' # scrap the variable from the regression summary
#' Variable=names(coefficients(fit))
#' # scrap the Estimated Coefficient from the regression summary
#' Estimated_Coefficients = as.numeric(coefficients(fit))
#' # scrap the P-Value from the regression summary
#' PValue = round(as.numeric(summary(fit)$coefficients[,  4]), digits = 5)
#' 
#' # create a data frame to hold the important factors in the regression summary
#' regression = data.frame(Variable, Genre, Estimated_Coefficients, PValue)
#' 
#' # Reference: "https://www.r-bloggers.com/2013/01/regression-on-categorical-variables/"
#' 
#' # Graphs the NA Sales value for each Genre
#' plt <- ggplot(vgsales, aes(x=Genre,y=NA_Sales, color=Genre)) +
#'   geom_line(lwd=2) +
#'   labs(x="Genre", y="NA Sales", title="Genre vs NA Sales")
#' print(plt)
#' 
#' 


#Main Question 3 : North America makes the most revenue from games on average. 
#Subquestion 1: XBOx is the most used console in North America 
#Subqeustion 2: Sports games are the most popular in north america 
#Subquestion 2.5: which variable affects sales in north america 
#nova test (same as t test but for multiple samples, null hypothesis has to be that means are roughly equivalent)

install.packages("rvest")
install.packages("dplyr")
install.packages("ggplot2")

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
hypothesis.data <- revenue.data
#Data is cleaned and ready to use

#_________________________________Data Analysis_________________________________

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

#We see a higher mean revenue in Asia

#Calculating Per Capita Mean to see if it is more accurate representation of revenue
#Bringing in 2010 population data by continent to calculate per capita revenue
revenue.data$Population <- c("4.194", "0.366", "0.737", "0.343", "0.597", "0.385")
#https://www.statista.com/statistics/997040/world-population-by-continent-1950-2020/
#https://data.worldbank.org/indicator/SP.POP.TOTL?locations=ZQ

revenue.data[] <- lapply(revenue.data, as.numeric)
str(revenue.data)

#Calculating Per capita based on each year
revenue.data$PerCapita2010 <- revenue.data$`2010` / revenue.data$Population 
revenue.data$PerCapita2011 <- revenue.data$`2011` / revenue.data$Population 
revenue.data$PerCapita2012 <- revenue.data$`2012` / revenue.data$Population 
revenue.data$PerCapita2013 <- revenue.data$`2013` / revenue.data$Population 
#Calculating Mean per capita per continent 
revenue.data$PerCapitaMean <- rowMeans(subset(revenue.data, select = c(PerCapita2010,PerCapita2011, PerCapita2012, PerCapita2013)), na.rm = TRUE)

rownames <- rownames(revenue.data)
rownames(revenue.data) <- c("Asia", "Australia", "Europe", "North America", "Latin America", "Middle East")

#We see a higher per capita revenue in North America

#Creating plot to look at Per Capita Mean Distribution
qplot(x = rownames, y = revenue.data$PerCapitaMean, xlab = "Continent", ylab = "Per Capita Average")
#Creating plot to look at mean distribution 
qplot(x = rownames, y = revenue.data$Mean, xlab = "Continent", ylab = "Revenue")

#Creating proper conditions to run ANOVA testing
#Change format of dataset, transpose column to row so we test for continents not year
#stack data to be able to run ANOVA test
df_transpose = t(hypothesis.data)
transpose <- data.frame(df_transpose)
stacked.data<- stack(transpose)
Anova.results <- aov(values ~ ind, data = stacked.data)

#ANOVA Test Results:
summary(Anova.results)
# Df Sum Sq Mean Sq F value Pr(>F)    
# ind          5   6711  1342.2   465.2 <2e-16 ***
#   Residuals   24     69     2.9                   
# ---

#F-statistic value = 465.2 
#This means that the different continents have no relationship between each other
#The revenue each continent makes is unrelated to other continents
#The Null Hypothesis is FALSE

#Answer to Question 3: North America does not make the most gaming revenue, Asia makes the most. 

#Exporting the data to create visuals using POWER BI 
library("writexl")
write_xlsx(revenue.data,"C:/Users/terri/OneDrive/Desktop/BTMA 431/FINAL PROJECT//q3data.xlsx")
