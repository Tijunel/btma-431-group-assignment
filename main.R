# Copyright Justin Tijunelis, Luke Fouad, Terrin Mathews, Jessica Huong, Faith Nayko
# Completed December 5, 2021

# Library Imports
library('rvest')
library('dplyr')
library('tidyr')
library('purrr')
library('tidyverse')
library('XML')

# Configuration
setwd("/Users/Justin/Documents/GitHub/btma-431-group-assignment")

#### Q1 Data Fetching ##########################################################

parseGameDetails <- function(url) {
  # Get the details of the game
  gamePage <- read_html(url)
  # Get the user score
  userScore <- html_text(html_node(gamePage, "div.userscore_wrap > div.metascore_w.user"))
  userScore <- as.numeric(userScore)  
  # Get the number of reviewers
  reviewers <- html_text(html_node(gamePage, "div.userscore_wrap > div.summary > p > span.count > a"))
  reviewers <- gsub(" Ratings", "", reviewers)
  reviewers <- as.numeric(reviewers)
  # Genres
  genres <- html_text(html_nodes(gamePage, "li.product_genre > span.data"))
  for (i in 1:length(genres)) {
    if (grepl("Action", genres[i])) {
      genres[i] <- "Action"
    }
  }
  genres <- unique(genres)
  # Publishers
  publishers <- html_text(html_nodes(gamePage, "li.publisher > span.data > a"))
  for (i in 1:length(publishers)) {
    if (publishers[i] == "EA Games" || publishers[i] == "EA Sports") {
      publishers <- c("Electronic Arts")
      break
    } else if (publishers[i] == "Rockstar San Diego") {
      publishers <- c("Rockstar Games")
      break
    }
    publishers[i] <- trimws(publishers[i])
  }
  # Ratings
  rating <- html_text(html_node(gamePage, "li.product_rating > span.data"))
  return (list(userScore, reviewers, genres, publishers, rating))
}

parseGameEntry <- function(baseURL, entry) {
  # Get the rank
  rank <- html_text(html_node(entry, "span.numbered"))
  rank <- trimws(rank)
  rank <- as.numeric(rank)
  # Get the meta score
  metaScore <- html_text(html_node(entry, "div.metascore_w"))
  metaScore <- trimws(metaScore)
  metaScore <- as.numeric(metaScore)
  # Name
  name <- html_text(html_node(entry, "a.title > h3"))
  # Release date
  releaseDate <- html_text(html_node(entry, "div.clamp-details > span"))
  releaseDate <- as.Date(releaseDate, format="%B %d, %Y")
  # Details URL
  detailsURL <- paste(baseURL, html_attr(html_node(entry, "a.title"), "href"), sep="")
  
  # Get the game details
  details <- parseGameDetails(detailsURL)
  df <- data.frame(
    rank = rank, 
    name = name,
    metaScore = metaScore,
    releaseDate = releaseDate, 
    userScore = details[[1]],
    reviewers = details[[2]],
    rating = details[[5]],
    stringsAsFactors = FALSE
  )
  df$genres <- list(details[[3]])
  df$publishers <- list(details[[4]])
  
  return (df)
}

getTopGames <- function(url, pages) {
  topGameData <- data.frame()
  out <- tryCatch({
      save(topGameData, file="topGameData.rda")
      for (i in 0:pages) { # Get the top 4000 games. 
        page <- read_html(paste(url, "?page=", i, sep=''))
        gameElements <- html_nodes(page, ".clamp-summary-wrap")
        baseURL <- "https://www.metacritic.com"
        for (entry in gameElements) {
          topGameData <- rbind(topGameData, parseGameEntry(baseURL, entry))
          Sys.sleep(1.0) # Let's be polite
        }
        topGameData <- na.omit(topGameData)
        save(topGameData, file="topGameData.rda")
      }
    },
    error = function(e) {
      save(topGameData, file="topGameData.rda")
      print(e)
    },
    finally = {
      save(topGameData, file="topGameData.rda")
      print("Finished!")
    }
  )
}

# Un-comment to see the auto data fetcher in action
if (!file.exists("topGameData.rda")) {
  url <- "https://www.metacritic.com/browse/games/score/metascore/all"
  getTopGames(url, pages = 40) # Gets the top 40 pages of data (4000 games)
}
load("topGameData.rda")

#### Q1 Data Fetching ##########################################################


### Question 1 #################################################################
#' Which publisher has the highest rated games?
#' Null Hypothesis: Publisher X and Publisher Y produce games with equal user and meta scores.
getPublisherStats <- function(topGames) {
  topGamesIndexed <- topGames %>% separate_rows(publishers, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "genres"))
  stats <- topGamesIndexed %>% group_by(publishers) 
}

# Perform a regression on the userScore dependent on publishers
publisherStats <- getPublisherStats(topGameData)
publisherUserScoreRegression <- lm(userScore ~ publishers, data = publisherStats)
print(summary(publisherUserScoreRegression))
# The publisher with the largest coefficient is Sony Interactive entertainment and VU Games (Each with a coefficient of 0.50)
# The publisher with the lowest coefficient is SEGA (-1.53) Activision (-1.21).

# Perform a regression on the metaScore dependent on publishers
publisherMetaScoreRegression <- lm(metaScore ~ publishers, data = publisherStats)
print(summary(publisherMetaScoreRegression))

testPubRatings <- function(topGameData, pub1, pub2) {
  pubRatings1 <- subset(q1Data, primaryPublisher == pub1, select = c(userScore, metaScore))
  pubRatings2 <- subset(q1Data, primaryPublisher == pub2, select = c(userScore, metaScore))
  print(t.test(pubRatings1$userScore, pubRatings2$userScore, mu=0.5, alternative="greater"))
  print(t.test(pubRatings1$metaScore, pubRatings2$metaScore, mu=0.5, alternative="greater"))
}

testPubRatings(topGameData, "Rockstar Games", "Nintendo")
# Null Hypothesis -> Rockstar Games and Nintendo produce games with equal user and meta score
# The null hypothesis is for user scores false. Nintendo outcompetes Rockstar. 
# The null hypothesis for meta scores is false. Rockstar outcompetes Nintendo.

#' What is the greatest predictor of a high rating?
#https://discuss.analyticsvidhya.com/t/how-to-add-a-column-to-a-data-frame-in-r/3278
# getSeason <- function(dates) {
#   WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
#   SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
#   SS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
#   FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox
#   
#   # Convert dates from any year to 2012 dates
#   d <- as.Date(strftime(dates, format="2012-%m-%d"))
#   
#   ifelse (d >= WS | d < SE, "Winter",
#           ifelse (d >= SE & d < SS, "Spring",
#                   ifelse (d >= SS & d < FE, "Summer", "Fall")))
# }
# 
# q1Data <- mutate(topGameData, releaseSeason = getSeason(topGameData$releaseDate))
# q1Data <- q1Data %>% rowwise() %>% mutate(primaryGenre = genres[[1]])
# q1Data <- q1Data %>% rowwise() %>% mutate(primaryPublisher = publishers[[1]])
# 
# # TODO: Create a regression and find the variable with the highest coefficient for predicting the user score.
# metaRegression <- function(topGameData) {
#   sData <- select(topGameData, userScore, releaseSeason)
#   names(sData) <- c("Score", "Season")
#   print(summary(lm(Score ~ ., data = sData)))
#   
#   gData <- select(topGameData, userScore, primaryGenre)
#   names(gData) <- c("Score", "Genre")
#   print(summary(lm(Score ~ ., data = gData)))
#   
#   pData <- select(topGameData, userScore, primaryPublisher)
#   names(pData) <- c("Score", "Publisher")
#   print(summary(lm(Score ~ ., data = pData)))
#   
#   mData <- select(topGameData, userScore, metaScore)
#   names(mData) <- c("Score", "Meta")
#   print(summary(lm(Score ~ ., data = mData)))
# }
# 
# metaRegression(q1Data)

### Question 1 - Sub Question ##################################################
#' Null Hypothesis: Genres receive equal ratings, there is no difference in ratings between 
getGenreStats <- function(topGames) {
  topGamesIndexed <- topGames %>% separate_rows(genres, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "publishers"))
  stats <- topGamesIndexed %>%
            group_by(genres) #%>%
            # mutate(
            #   mean_ratings = mean(userScore),
            #   count_games = n(),
            #   mean_meta = mean(metaScore),
            # ) %>%
            # summarise_if(is.numeric, mean)
  return (stats)
}

# Test the genre equivalence
groupedGenres <- getGenreStats(topGameData)
testGenreEquivalence(groupedGenres)
anovaTest <- aov(userScore ~ genres, data = groupedGenres)
print(summary(anovaTest))
# The p-value is less than 0.05, so we can conclude there are significant differences between the genres.
# Therefore, the alternative hypothesis is true.

# Create a regression
reg <- lm(userScore ~ genres, data = groupedGenres)
print(summary(reg))


# TODO: Create a visualization of genre prevalence. 
# TODO: Answer the question, perform a t-test, be sure to account for different sample size. 
# TODO: Write a description describing the process and then results of the tests. 

### Question 2 #################################################################
#' Null Hypothesis: The top rated games from the top 100 list on meta critic were released in winter.

getSeasonScores <- function(topGames) {
  # TODO: Group the games by seasons (use a custom implementation to find the season based on the date)
  # TODO: Get aggregate score statistics for each season (average meta score, average user score, number of releases in the season)
}

# TODO: Compare the seasons using a t-test (or maybe something better?)
# TODO: Write description for process and the results we found

### Question 3 #################################################################
#' Null Hypothesis: North America sells the most video games.

### Question 3 - Sub Question 1 ################################################
#' Null Hypothesis: Xbox is the most popular platform in North America all-time.

### Question 3 - Sub Question 2 ################################################
#' Null Hypothesis: What is the greatest predictor of sales for a game. 

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

# set the working directory to where you downloaded archive.zip 
# unzip and load vgsales.csv into the environment. 
vgsales <- read.csv(unz("archive.zip", "vgsales.csv"), stringsAsFactors = FALSE)

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
