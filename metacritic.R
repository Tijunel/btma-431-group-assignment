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
# setwd("/Users/Justin/Documents/GitHub/btma-431-group-assignment")

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
      for (i in 0:pages) {
        tryCatch({
          page <- read_html(paste(url, "?page=", i, sep=''))
          gameElements <- html_nodes(page, ".clamp-summary-wrap")
          baseURL <- "https://www.metacritic.com"
          for (entry in gameElements) {
            tryCatch({
              topGameData <- rbind(topGameData, parseGameEntry(baseURL, entry))
            },
            error = function(e) {
              print("Something went wrong fetching data for this game")
            })
            #Sys.sleep(0.2) # Let's be polite
          }
        },
        error = function(e) {
          print("Something went wrong fetching data for this game")
        })
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

# Invert to condition to see the file fetcher in action
if (!file.exists("topGameData.rda")) {
  url <- "https://www.metacritic.com/browse/games/score/metascore/all"
  getTopGames(url, pages = 180) # Gets the top 40 pages of data (4000 games)
}
load("topGameData.rda")

# Saving data for plot generation outside of R
# Create rda file for publishers
savePublisherData <- function(topGames) {
  publisherData <- topGames %>% separate_rows(publishers, sep = "\n") %>% filter(n() > 5)
  write.csv(publisherData, 'publisherData.csv')
}
# Create rda file for genres
saveGenreData <- function() {
  genreData <- topGames %>% separate_rows(genres, sep = "\n") %>% filter(n() > 3)
  write.csv(genreData, 'genreData.csv')
}

# Make sure the meta score is the right scale
topGameData$metaScore <- topGameData$metaScore / 10
# Convert dates to posix time to analyze as a number
#dates <- as.POSIXct(topGameData$releaseDate, format = "%Y-%m-%d")
#topGameData$releaseDate <- as.numeric(dates)

#' #### Q1 Data Fetching END ###################################################


#' ### Question 1 ##############################################################
#' NULL Hypothesis: Large publishers do not higher average user ratings than smaller publishers
#' Alternative Hypothesis: Large publishers do have higher average user average ratings than larger publishers
print("Question 1")
getPublisherStats <- function(topGames, games, min = TRUE) {
  topGamesIndexed <- topGames %>% separate_rows(publishers, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "genres"))
  stats <- c()
  if (min) {
    stats <- topGamesIndexed %>% group_by(publishers) %>% filter(n() >= games) %>% mutate(
      count_games = n(),
      meanUserScore = mean(userScore),
      meanMetaScore = mean(metaScore),
      meanReviewers = mean(reviewers)
    ) %>% summarise_if(is.numeric, mean)
  } else {
    stats <- topGamesIndexed %>% group_by(publishers) %>% filter(n() < games) %>% mutate(
      count_games = n(),
      meanUserScore = mean(userScore),
      meanMetaScore = mean(metaScore),
      meanReviewers = mean(reviewers)
    ) %>% summarise_if(is.numeric, mean)
  }
  stats <- na.omit(stats)
  return (stats)
}

print("Top rated publishers for small, medium, and mid sized publishers")
for(i in seq(0, 10, by=10)) {
  publisherStats <- getPublisherStats(topGameData, i, TRUE)
  bestUserScore <- publisherStats[which.max(publisherStats$meanUserScore),]
  bestMetaScore <- publisherStats[which.max(publisherStats$meanMetaScore),]
  worstUserScore <- publisherStats[which.min(publisherStats$meanUserScore),]
  worstMetaScore <- publisherStats[which.min(publisherStats$meanMetaScore),]
  print(paste("The publisher with the highest user score with at least", i, "games is", bestUserScore$publishers, "with a mean user score of", bestUserScore$meanUserScore))
  print(paste("The publisher with the highest Metascore with at least", i, "games is", bestMetaScore$publishers, "with a mean Metascore of", bestMetaScore$meanMetaScore))
  print(paste("The publisher with the lowest user score with at least", i, "games is", worstUserScore$publishers, "with a mean user score of", worstUserScore$meanUserScore))
  print(paste("The publisher with the lowest Metascore with at least", i, "games is", worstMetaScore$publishers, "with a mean Metascore of", worstMetaScore$meanMetaScore))
}

# Let's plot the meta and user score for the top publishers
publisherStats <- getPublisherStats(topGameData, 200, TRUE)
plottingData <- within(publisherStats, rm("metaScore", "userScore", "reviewers", "count_games", "meanReviewers"))
plottingData <- plottingData %>% rename("Mean Metascore" = meanMetaScore, "Mean User Score" = meanUserScore)
plottingData <- gather(plottingData, variable, value, -publishers)
plt <- ggplot(plottingData, aes(x=publishers, y=value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", width=0.5) +
  ylim(0, 10) +
  guides(fill=guide_legend(title="Score Type")) +
  labs(x="Publisher", y="Mean Score", title="Mean scores for publishers with 200+ games")
print(plt)

# Let's plot the mean number of reviewers for the top publishers
plottingData <- within(publisherStats, rm("metaScore", "userScore", "reviewers", "count_games", "meanMetaScore", "meanUserScore"))
plottingData <- plottingData %>% rename("Mean Reviewer Count" = meanReviewers)
plottingData <- gather(plottingData, variable, value, -publishers)
plt <- ggplot(plottingData, aes(x=publishers, y=value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", width=0.5) +
  labs(x="Publisher", y="Mean Number of Reviewers Per Game", title="Mean number of reviewers for publishers with 200+ games")
print(plt)

#' Let's check our hypothesis
#' This function allows a user to define what a "large" publisher is by passing
#' N, which will be used to parse the game data into publishers that have >= N games, 
#' and < N. The user can also pass a mu value to check if the large publishers get
#' higher ratings by some constant amount (e.g. do publishers get greater average scores by 0.5%?)
print("Let's perform a t-test")
publishersSizeTTest <- function (gameData, N, mu = 0) {
  largePublishers <- getPublisherStats(gameData, N, TRUE)$meanUserScore
  smallPublishers <- getPublisherStats(gameData, N, FALSE)$meanUserScore
  print(t.test(largePublishers, smallPublishers, mu=mu, alternative="greater"))
}
publishersSizeTTest(topGameData, 100)
# The p-value is less than 0.05, thus we reject the null hypothesis and may say
# that larger publishers indeed receive higher average user scores than smaller publishers

### Sub Question 1-1 ###########################################################
#' Null Hypothesis: Publisher X does not make games with greater review scores than Publisher Y.
#' Alternative Hypothesis: Publisher X does make games with a greater review scores than Publisher Y.

#' This function allows a user to compare pub 1, and pub 2. It tests whether the
#' average user and meta scores of pub1 are greater than those of pub1. It also
#' takes a mu value to let the user check if pub1 is greater by some constant mu.
testPublisherRatings <- function(topGameData, pub1, pub2, mu = 0.1) {
  pubRatings1 <- subset(topGameData, publishers == pub1, select = c(userScore, metaScore))
  pubRatings2 <- subset(topGameData, publishers == pub2, select = c(userScore, metaScore))
  print(t.test(pubRatings1$userScore, pubRatings2$userScore, mu=mu, alternative="greater"))
  print(t.test(pubRatings1$metaScore, pubRatings2$metaScore, mu=mu, alternative="greater"))
}
publisherData <- topGameData %>% separate_rows(publishers, sep = "\n")
testPublisherRatings(publisherData, "Rockstar Games", "Nintendo", 0.1)
#' For user scores, Rockstar does not outperform Nintendo. (Accept null hypothesis, reject alternative)
#' For meta scores, Rockstar indeed outperforms Nintendo. (Reject null hypothesis, accept alternative)

### Sub Question 1-2 ###########################################################
#' Null Hypothesis: Larger publishers do not receive greater meta/user score ratios than smaller publishers. 
#' Alternative Hypothesis: Large publishers receive greater meta/user score ratios than smaller publishers.
testAllPublisherRatios <- function(topGames, N) {
  topGamesIndexed <- topGames %>% separate_rows(publishers, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "genres"))
  largePublishers <- topGamesIndexed %>% group_by(publishers) %>% filter(n() >= N) %>% mutate(
    count_games = n(),
    meanUserScore = mean(userScore),
    meanMetaScore = mean(metaScore),
    meanReviewers = mean(reviewers)
  ) %>% summarise_if(is.numeric, mean)
  smallPublishers <- topGamesIndexed %>% group_by(publishers) %>% filter(n() < N) %>% mutate(
    count_games = n(),
    meanUserScore = mean(userScore),
    meanMetaScore = mean(metaScore),
    meanReviewers = mean(reviewers)
  ) %>% summarise_if(is.numeric, mean)
  largePublisherRatios <- largePublishers$meanMetaScore / largePublishers$meanUserScore
  smallPublisherRatios <- smallPublishers$meanMetaScore / smallPublishers$meanUserScore
  print(t.test(largePublisherRatios, smallPublisherRatios, alternative="greater"))
}
testAllPublisherRatios(topGameData, 100) # With 100, we fail to reject the null hypothesis
testAllPublisherRatios(topGameData, 400) # With 100, we fail to reject the null hypothesis

testGroupPublisherRatios <- function(topGames, N1, N2) {
  topGamesIndexed <- topGames %>% separate_rows(publishers, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "genres"))
  largePublishers <- topGamesIndexed %>% group_by(publishers) %>% filter(n() >= N2) %>% mutate(
    count_games = n(),
    meanUserScore = mean(userScore),
    meanMetaScore = mean(metaScore),
    meanReviewers = mean(reviewers)
  ) %>% summarise_if(is.numeric, mean)
  smallPublishers <- topGamesIndexed %>% group_by(publishers) %>% filter(n() < N1) %>% mutate(
    count_games = n(),
    meanUserScore = mean(userScore),
    meanMetaScore = mean(metaScore),
    meanReviewers = mean(reviewers)
  ) %>% summarise_if(is.numeric, mean)
  largePublisherGroupRatios <- largePublishers$meanMetaScore / largePublishers$meanUserScore
  smallPublisherGroupRatios <- smallPublishers$meanMetaScore / smallPublishers$meanUserScore
  print(t.test(largePublisherGroupRatios, smallPublisherGroupRatios, alternative="greater"))
}
# Let's compare two publisher at the tail bounds rather than cutting the publishers into two sections
# Here, we test publishers with 10 or less games and publishers with 300 or more games. 
testGroupPublisherRatios(topGameData, 10, 300) # Still no! We cannot reject the null hypothesis.

# This plots a graph of the percentage of publishers that have higher user than meta scores
# based on publisher size measured by number of games released. 
#' minGames <- seq(3, 450)
#' publisherData <- topGamesIndexed <- topGameData %>% separate_rows(publishers, sep = "\n")
#' publishers <- unique(publisherData$publishers)
#' acceptancePercentages <- c()
#' for (i in minGames) {
#'   greaterScores <- c()
#'   for (publisher in publishers) {
#'     games <- publisherData[publisherData$publishers == publisher,]
#'     userScores <- games$userScore
#'     metaScores <- games$metaScore
#'     if (length(userScores) >= i && length(metaScores) >= i) {
#'       p <- t.test(metaScores, userScores, alternative="greater", paired = FALSE)$p.value
#'       greaterScores <- c(greaterScores, p < 0.05)
#'     }
#'   }
#'   acceptancePercentages <- c(acceptancePercentages, mean(greaterScores))
#' }
#' acceptancePercentages <- acceptancePercentages * 100
#' 
#' rejectionPercentages <- c()
#' for (i in minGames) {
#'   greaterScores <- c()
#'   for (publisher in publishers) {
#'     games <- publisherData[publisherData$publishers == publisher,]
#'     userScores <- games$userScore
#'     metaScores <- games$metaScore
#'     if (length(userScores) <= i && length(metaScores) <= i && length(userScores) > 2 && length(metaScores) > 2) {
#'       p <- t.test(metaScores, userScores, alternative="greater", paired = FALSE)$p.value
#'       greaterScores <- c(greaterScores, p < 0.05)
#'     }
#'   }
#'   rejectionPercentages <- c(rejectionPercentages, mean(greaterScores))
#' }
#' rejectionPercentages <- rejectionPercentages * 100
#' 
#' plotData <- data.frame(minGames = minGames, acceptancePercentages = acceptancePercentages, rejectionPercentages = rejectionPercentages, percentageOfGames = percentageOfGames)
#' plt <- ggplot(plotData, aes(x=minGames)) +
#'         geom_line(aes(y = acceptancePercentages, color='At least X games published'), size=2) +
#'         geom_line(aes(y = rejectionPercentages, color='At most X games published'), size=2) +
#'         ylim(0, 100) +
#'         guides(fill=guide_legend(title="Legend")) +
#'         labs(x="Minimum games published", y="Percent of publishers", title="Percentage of publishers with games that have dominating Metascores")
#' print(plt)

### Question 2 #################################################################
#' Is genre a statistically significant predictor of user scores?
#' Null Hypothesis: Genre is an independent variable to user and meta score. 
#' Alternative Hypothesis: Genre is not an independant variable to user and meta score. 
getGenreStats <- function(topGames, minGames) {
  topGamesIndexed <- topGames %>% separate_rows(genres, sep = "\n")
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating"))
  stats <- topGamesIndexed %>% group_by(genres) %>% filter(n() > minGames) %>% mutate(
    meanUserScore = mean(userScore),
    count_games = n(),
    meanMetaScore = mean(metaScore)
  ) %>%
    summarise_if(is.numeric, mean)
  stats <- na.omit(stats)
  return (stats)
}

genreStats <- getGenreStats(topGameData, 800)
genreStats <- within(genreStats, rm("metaScore", "userScore", "reviewers", "count_games", "publishers", "releaseDate"))
genreStats <- genreStats %>% rename("Mean Metascore" = meanMetaScore, "Mean User Score" = meanUserScore)
plottingData <- gather(genreStats, variable, value, -genres)
plt <- ggplot(plottingData, aes(x=genres, y=value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", width=0.5) +
  ylim(0, 10) +
  guides(fill=guide_legend(title="Score Type")) +
  labs(x="Genre", y="Mean Score", title="Mean scores for genres with 800+ games")
print(plt)

# Test the significance of the genre categorical
explodedGenre <- topGameData %>% rowwise() %>% mutate(genres = genres[[1]], publishers = publishers[[1]])
explodedGenre <- within(explodedGenre, rm("rank", "name", "metaScore"))
genreOnlyModel <- lm(userScore ~ genres, data = genreOnly)
fullModel <- lm(userScore ~ ., data = explodedGenre)
topGamesWithoutGenre <- within(explodedGenre, rm("genres"))
modelWithoutGenre <- lm(userScore ~ ., data=topGamesWithoutGenre)
print(anova(fullModel, modelWithoutGenre))
# The p-value is greater than 0.05, so we reject the null hypothesis. 
# Genre is not an independent variable. 

### Sub Question 2-1 ########################################################
#' Null Hypothesis: Game review scores and their release seasons are independent.
#' Alternative Hypothesis: Game review scores and their release seasons are not independent.
addSeason <- function(topGames) {
  seasonDate <- as.Date(strftime(topGames$releaseDate, format = "2012-%m-%d"))
  winter <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  spring <- as.Date("2012-03-21", format = "%Y-%m-%d") # Spring Solstice
  summer <- as.Date("2012-06-21", format = "%Y-%m-%d") # Summer Solstice
  fall <- as.Date("2012-09-21", format = "%Y-%m-%d") # Summer Solstice
  season <- ifelse (seasonDate >= winter | seasonDate < spring, "Winter",
                    ifelse (seasonDate >= spring & seasonDate < summer, "Spring",
                            ifelse (seasonDate >= summer & seasonDate < fall, "Summer", "Fall")))
  topGames$releaseSeason <- season
  return(topGames)
}

getSeasonScores <- function(topGames) {
  topGamesIndexed <- topGames %>% rowwise() %>% mutate(genres = genres[[1]], publishers = publishers[[1]])
  topGamesIndexed <- within(topGamesIndexed, rm("rank", "rating", "releaseDate"))
  stats <- topGamesIndexed %>% group_by(releaseSeason) %>% mutate(
    meanUserScore = mean(userScore),
    count_games = n(),
    meanMetaScore = mean(metaScore)
  ) %>%
    summarise_if(is.numeric, mean)
  stats <- na.omit(stats)
  return (stats)
}

#Plotting Season Stats
seasonStats <- getSeasonScores(addSeason(topGameData))
plt <- ggplot(seasonStats, aes(x=releaseSeason, y=count_games)) +
  geom_bar(aes(fill = count_games), position = "dodge", stat="identity", width=0.5) +
  ylim(0, 4000) +
  labs(x="Release Season", y="Number of Games", title="Number of Games Released in each Season")
print(plt)

seasonStats <- within(seasonStats, rm("metaScore", "userScore", "reviewers", "count_games"))
seasonStats <- seasonStats %>% rename("Mean Metascore" = meanMetaScore, "Mean User Score" = meanUserScore)
seasonPlottingData <- gather(seasonStats, variable, value, -releaseSeason)
plt <- ggplot(seasonPlottingData, aes(x=releaseSeason, y=value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", width=0.5) +
  ylim(0, 10) +
  guides(fill=guide_legend(title="Score Type")) +
  labs(x="Release Season", y="Mean Score", title="Mean scores for each Release Season")
print(plt)

plt <- ggplot(seasonPlottingData, aes(x=releaseSeason, y=value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", width=0.5) +
  ylim(0, 10) +
  guides(fill=guide_legend(title="Score Type")) +
  labs(x="Release Season", y="Mean Score", title="Mean scores for each Release Season")
print(plt)

#Testing Significance of Release Season as a predictor of score
seasonData <- topGameData
seasonData <- addSeason(seasonData)
explodedSeason <- seasonData %>% rowwise() %>% mutate(genres = genres[[1]], publishers = publishers[[1]])
explodedSeason <- within(explodedSeason, rm("rank", "name", 'releaseDate', 'metaScore'))
fullModel <- lm(userScore ~ ., data = explodedSeason)
explodedSeason <- within(explodedSeason, rm("releaseSeason"))
modelWithoutSeason <- lm(userScore ~ ., data=explodedSeason)
print(summary(fullModel))
print(anova(fullModel, modelWithoutSeason))
# The p-value is greater than 0.05, so we cannot reject the null hypothesis. 
# Thus, release season is independent of user score. 