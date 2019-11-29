# load the ggplot2 library for plotting

library(ggplot2)
options(stringsAsFactors = FALSE)

setwd("D:\\Geodaten\\#Jupiter\\GEO402\\project\\402slangbos")

download.file("https://ndownloader.figshare.com/files/9282364",
              "boulder-precip.csv",
              method = "libcurl")

# import data
boulder_precip <- read.csv(file = "boulder-precip.csv")

# view first few rows of the data
head(boulder_precip)

# plot the data using ggplot
ggplot(data = boulder_precip, aes(x = DATE, y = PRECIP)) +
    geom_point() +
    labs(x = "Date",
         y = "Total Precipitation (Inches)",
         title = "Precipitation Data",
         subtitle = "Boulder, Colorado 2013")

boulder_precip$DATE <- as.Date(boulder_precip$DATE,
                               format = "%m/%d/%y")

ggplot(data = boulder_precip, aes(x = DATE, y = PRECIP)) +
    geom_bar(stat = "identity", fill = "purple") +
    labs(title = "Total daily precipitation in Boulder, Colorado",
         subtitle = "Fall 2013",
         x = "Date", y = "Daily Precipitation (Inches)")
