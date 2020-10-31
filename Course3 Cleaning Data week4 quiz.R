# WEEK 4 QUIZ
library(tidyverse)
library(tidyr)
#QUESTION 1:
getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile = "./United States communities.csv")

survey <- read.csv("./United States communities.csv", header = TRUE)
head(survey)
survey <- as_tibble(survey)
survey
surveysplit <- strsplit(names(survey), "wgtp") 
surveysplit[[123]]


#QUESTION 2
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url2, destfile = "./grossdomestic.csv")
gross <- read.csv("./grossdomestic.csv", nrows = 190, skip =4)
head(gross)
gross <- as_tibble(gross)
gross$X.4 <- as.integer(gsub(",", "", gross$X.4))
gross
mean(gross$X.4)

#QUESTION 3
grep("^United", gross$X.3)

#QUESTION 4
url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url3, destfile = "./education.csv")
eudcation <- as_tibble(read.csv("./education.csv", header = TRUE))
names(gross) <- c( "CountryCode", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9")
mergeddata <- merge(gross, eudcation, by = "CountryCode")
as_tibble(mergeddata)
length(grep("Fiscal year end: June", mergeddata$Special.Notes))
