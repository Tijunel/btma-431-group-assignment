#Main Question: North America makes the most revenue from games
  #Subquestion 1: XBOx is the most used console in North America 
  #Subqeustion 2: Sports games are the most popular in north america 
  #Subquestion 2.5: which variable affects sales in north america 
#nova test (same as t test but for multiple samples, null hypothesis has to be that means are roughly equivalent)

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

#Data Collection:
#Scrape data from website
gaming.data <- read_html("https://vgsales.fandom.com/wiki/Video_game_industry")
tables <- gaming.data %>% html_table(fill = TRUE)
#Create dataframe from scraped data 
continent.revenue <- tables[[1]]
world.revenue <- tables[[2]]

#Data Cleanup:
#Remove "Notes" column 
continent.revenue <- select(continent.revenue, -c("Note(s)"))
#Rename first column
colnames(continent.revenue)[1] <- "country"
is.data.frame(continent.revenue)

#Filter for continents only 
#There is no Africa or antartica, and north america is split into north and latin 
continents <- c("Asia-Pacific", "Europe", "Australia", "North America", "Latin America", "Middle East")
filt.cont <- filter(continent.revenue, country %in% continents) 

#Data clean up: Removing "$", "billion", "[#]" 
#Initial part is automating it (incomplete), second part is manually changing which is complete

# filt.cont$`2013` <- gsub("[$]","",as.character(filt.cont$`2013`))
# filt.cont$`2012` <- gsub("[$]","",as.character(filt.cont$`2012`))
# filt.cont$`2011` <- gsub("[$]","",as.character(filt.cont$`2011`))
# filt.cont$`2010` <- gsub("[$]","",as.character(filt.cont$`2010`))
# 
# filt.cont$`2013` <- gsub("billion","",as.character(filt.cont$`2013`))
# filt.cont$`2012` <- gsub("billion","",as.character(filt.cont$`2012`))
# filt.cont$`2011` <- gsub("billion","",as.character(filt.cont$`2011`))
# filt.cont$`2010` <- gsub("billion","",as.character(filt.cont$`2010`))
# 
# filt.cont$`2013` <- gsub("\\[|\\]", "", filt.cont$`2013`)
# filt.cont$`2012` <- gsub("\\[|\\]", "", filt.cont$`2012`)
# filt.cont$`2011` <- gsub("\\[|\\]", "", filt.cont$`2011`)
# filt.cont$`2010` <- gsub("\\[|\\]", "", filt.cont$`2010`)
# 
# filt.cont$`2013` <- gsub("59", "", filt.cont$`2013`)
# filt.cont$`2012` <- gsub("\\[|\\]", "", filt.cont$`2012`)
# filt.cont$`2011` <- gsub("\\[|\\]", "", filt.cont$`2011`)
# filt.cont$`2010` <- gsub("\\[|\\]", "", filt.cont$`2010`)

filt.cont$country <- c("Asia", "Australia", "Europe", "North America", "Latin America", "Middle East")
filt.cont$`2013` <- c("49.623", "2", "20", "22.8", "3.9", "2.6")
filt.cont$`2012` <- c("44.063", "1.16", "21.3", "20.7", "5.4", "2.6")
filt.cont$`2011` <- c("42.358", "1.5", "21.3", "20.7", "5.4", "1.983")
filt.cont$`2010` <- c("38.77", "1.67", "20.66", "20.49", "4.74", "1.2")



setwd("C:/Users/terri/OneDrive/Desktop/BTMA 431/FINAL PROJECT")  
load("vgsales2")

#Rename dataset and columns for ease of use 
GradesData <- btma.431.736.f2018



url <- "https://s3.amazonaws.com/tripdata/202101-citibike-tripdata.csv.zip"
temp <-tempfile()
download.file(url, temp)
citibike <-read.csv(unz(temp, "202101-citibike-tripdata.csv"),stringsAsFactors = FALSE)
unlink(temp)

url <- "https://www.kaggle.com/vinodsunny1/insight-s-of-gaming-world/data?select=vgsales.csv"
temp <-tempfile()
download.file(url, temp)
citibike <-read.csv(unz(temp, "vgsales.csv"),stringsAsFactors = FALSE)
unlink(temp)


setwd("C:/Users/terri/OneDrive/Desktop/BTMA 431/FINAL PROJECT")
load(file = "vgsales2.csv")
