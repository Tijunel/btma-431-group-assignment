# GETTING AN ERROR UNZIPPING FILE FROM TEMP 
# GOOGLED ERROR AND IT SAYS IT'S BECAUSE URL DOES NOT END IN .ZIP 
# CAN'T FIND ANOTHER WAY TO DOWNLOADOAD YET. 

# URL where our data is stored 
# https://www.kaggle.com/vinodsunny1/insight-s-of-gaming-world/data
setwd("C:/Users/jessi/Downloads") # Set working directory where the .rdata and .rda files are  

url <- "https://www.kaggle.com/gregorut/videogamesales/download"   

temp <- tempfile()
download.file(url, temp)
project <- read.csv(unz(temp, "vgsales.csv"),
                     stringsAsFactors = FALSE)
unlink(temp)

# Attempt 2 
url <- "https://www.kaggle.com/gregorut/videogamesales/download"   

temp <- tempfile()
download.file(url, temp)
project <- read.csv(unz("archive.zip", "vgsales.csv"),
                    stringsAsFactors = FALSE)
unlink(temp)

#data <- read.table(unz("archive.zip", "vgsales.csv"), nrows=3550, header=T, quote="\"", sep=",")

# Attempt 3 
url <- "https://www.kaggle.com/gregorut/videogamesales/download"   

temp <- tempfile()
download.file(url, temp)
project <- read.table(unz("archive.zip", "vgsales.csv"), nrows=3551, header=T, quote="\"", sep=",")
unlink(temp)

#project <- read.csv("C:/Users/jessi/Downloads//vgsales.csv", stringsAsFactors = FALSE)

# CODE BELOW IS FROM ASSIGNMENT 3 
url <- "https://s3.amazonaws.com/tripdata/202101-citibike-tripdata.csv.zip"   

temp <- tempfile()
download.file(url, temp)
citibike <- read.csv(unz(temp, "202101-citibike-tripdata.csv"),
                     stringsAsFactors = FALSE)
unlink(temp)
citibike.trips <- citibike

