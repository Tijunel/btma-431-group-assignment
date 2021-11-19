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
