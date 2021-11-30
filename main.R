# Copyright Justin Tijunelis, Luke Fouad, Terrin Mathews, Jessica Huong, Faith Nayko
# Completed November X, 2021

# Library Imports
library('rvest')
library('dplyr')

# Configuration
setwd("/Users/justintijunelis/Documents/GitHub.nosync/btma-431-group-assignment")

#' Q1 - Rockstar Games produces games with the highest reviews.
#' Subquestion - Rockstar's action games are the best rated among all of their games.
#' Q2 - The top rated games are released in winter.
#' Q3 - North America sells the most video games.
#' Subquestion - Xbox is the most popular platform in north america all-time.
#' Subquestion - Sports games are the most played games on Xbox all-time.

# Part 1
# Fetch data
# Q1/Q2 website: page_url = 'https://www.metacritic.com/browse/games/score/metascore/all/ps4/filtered?view=detailed'
# Q3 website: https://www.kaggle.com/vinodsunny1/insight-s-of-gaming-world/data
# Download data everytime we run and don't save it. 

#### Q1 Data Fetching ##########################################################

parseGameDetails <- function(url) {
  # Get the details of the game
  gamePage <- read_html(url)
  # Get the user score
  print(gamePage)
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
  print(gameElements)
  for (entry in gameElements) {
    gameEntries <- rbind(gameEntries, parseGameEntry(baseURL, entry))
    Sys.sleep(0.2) # Let's be polite
  }
  return (gameEntries)
}

if (!file.exists("top100GameData.rda")) {
  url <- "https://www.metacritic.com/browse/games/score/metascore/all"
  top100GameData <- getTopGames(url)
  save(top100GameData, file="top100GameData.rda")
} else {
  load("top100GameData.rda")
}

print(top100GameData)

# Let's store the data for now

#### Q1 Data Fetching ##########################################################



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
