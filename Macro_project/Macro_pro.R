# import data
library(readxl)

library(dplyr)
library(zoo)

data <- read_excel("Finland_data.xlsx", sheet = 2)

str(data)

View(data)

data$time <- as.Date(as.yearqtr(data$time))
