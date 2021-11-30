# Copyright Justin Tijunelis, Luke Fouad, Terrin Mathews, Jessica Huong, Faith Nayko
# Completed November X, 2021

# Library Imports
library('rvest')
library('dplyr')
library('tidyr')

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

getTopGames <- function(url) {
  page <- read_html(url)
  gameElements <- html_nodes(page, ".clamp-summary-wrap")
  gameEntries <- data.frame()
  baseURL <- "https://www.metacritic.com"
  for (entry in gameElements) {
    gameEntries <- rbind(gameEntries, parseGameEntry(baseURL, entry))
    Sys.sleep(1.0) # Let's be polite
  }
  return (gameEntries)
}

if (!file.exists("top100GameData.rda")) {
  url <- "https://www.metacritic.com/browse/games/score/metascore/all"
  top100GameData <- getTopGames(url)
  top100GameData <- na.omit(top100GameData)
  save(top100GameData, file="top100GameData.rda")
} else {
  load("top100GameData.rda")
}

#### Q1 Data Fetching ##########################################################


### Question 1 #################################################################
#' Null Hypothesis: Rockstar Games produces games with the highest user reviews.
#' What is the greatest predictor of a high rating?

getPublisherStats <- function(top100Games) {
  topGamesIndexed <- top100Games %>% separate_rows(publishers, sep = "\n")
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

groupedPublishers <- getPublisherStats(top100GameData)

# TODO: Answer the questions with the data. Make sure to check how to compare t-test with different sample sizes.
# TODO: Create a regression and find the variable with the highest coefficient for predicting the user score.
# TODO: Perform a chi square test to check if the meta score and user score are independent for the dataset.

### Question 1 - Sub Question ##################################################
#' Null Hypothesis: Rockstar's action games are the best rated among all of their games.

getGenreStats <- function(top100Games) {
  topGamesIndexed <- top100Games %>% separate_rows(genres, sep = "\n")
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

groupedGenres <- getGenreStats(top100GameData)
print(groupedGenres)

# TODO: Answer the question, perform a t-test, be sure to account for different sample size. 
# TODO: Write a description describing the process and then results of the tests. 

### Question 2 #################################################################
#' Null Hypothesis: The top rated games from the top 100 list on meta critic were released in winter.

getSeasonScores <- function(top100Games) {
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
