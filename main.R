# Copyright Justin Tijunelis, Luke Fouad, Terrin Mathews, Jessica Huong, Faith Nayko
# Completed December 5, 2021

# Library Imports
library('rvest')
library('dplyr')
library('tidyr')
library('purrr')

# Configuration
setwd("/Users/justintijunelis/Documents/GitHub.nosync/btma-431-group-assignment")

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
        page <- read_html(paste(url, "?page=", 0, sep=''))
        gameElements <- html_nodes(page, ".clamp-summary-wrap")
        baseURL <- "https://www.metacritic.com"
        for (entry in gameElements) {
          topGameData <- rbind(topGameData, parseGameEntry(baseURL, entry))
          Sys.sleep(5.0) # Let's be polite
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
#' Null Hypothesis: Rockstar Games produces games with the highest user reviews.
#' What is the greatest predictor of a high rating?

getPublisherStats <- function(topGames) {
  topGamesIndexed <- topGames %>% separate_rows(publishers, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "genres"))
  stats <- topGamesIndexed %>% 
            group_by(publishers) %>%
            mutate(
              mean_ratings = mean(userScore),
              count_games = n(),
              mean_meta = mean(metaScore)
            ) %>%
            summarise_if(is.numeric, mean)
  return (stats)
}

# groupedPublishers <- getPublisherStats(topGameData)
# print(groupedPublishers)

#https://discuss.analyticsvidhya.com/t/how-to-add-a-column-to-a-data-frame-in-r/3278
getSeason <- function(dates) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

topGameData <- mutate(topGameData, releaseSeason = getSeason(topGameData$releaseDate))

topGameData <- topGameData %>% rowwise() %>% 
  mutate(primaryGenre = genres[[1]])

topGameData <- topGameData %>% rowwise() %>% 
  mutate(primaryPublisher = publishers[[1]])

# TODO: Create a regression and find the variable with the highest coefficient for predicting the user score.
metaRegression <- function(topGameData) {
  
  sData <- select(topGameData, userScore, releaseSeason)
  names(sData) <- c("Score", "Season")
  print(summary(lm(Score ~ ., data = sData)))
  
  gData <- select(topGameData, userScore, primaryGenre)
  names(gData) <- c("Score", "Genre")
  print(summary(lm(Score ~ ., data = gData)))
  
  pData <- select(topGameData, userScore, primaryPublisher)
  names(pData) <- c("Score", "Publisher")
  print(summary(lm(Score ~ ., data = pData)))
  
  mData <- select(topGameData, userScore, metaScore)
  names(mData) <- c("Score", "Meta")
  print(summary(lm(Score ~ ., data = mData)))
}

metaRegression(topGameData)

# TODO: Answer the questions with the data. Make sure to check how to compare t-test with different sample sizes.

testPubRatings <- function(topGameData, pub1, pub2) {
  pubRatings1 <- subset(topGameData, primaryPublisher == pub1, select = c(userScore, metaScore))
  pubRatings2 <- subset(topGameData, primaryPublisher == pub2, select = c(userScore, metaScore))
  print(t.test(pubRatings1$userScore, pubRatings2$userScore))
  print(t.test(pubRatings1$metaScore, pubRatings2$metaScore))
}

testPubRatings(topGameData, "Rockstar Games", "Nintendo")

### Question 1 - Sub Question ##################################################
#' Null Hypothesis: Genres receive equal ratings, there is no difference in ratings between 

getGenreStats <- function(topGames) {
  topGamesIndexed <- topGames %>% separate_rows(genres, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "publishers"))
  stats <- topGamesIndexed %>%
            group_by(genres) %>%
            mutate(
              mean_ratings = mean(userScore),
              count_games = n(),
              mean_meta = mean(metaScore),
            ) %>%
            summarise_if(is.numeric, mean)
  return (stats)
}

testGenreEquivalence <- function(genreData) {
  # TODO: Actually implement this
  oneway.test(mean_ratings ~ genres, data = groupedGenres)
}

groupedGenres <- getGenreStats(topGameData)

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
