# import data
library(readxl)
library(mFilter)
library(dplyr)
library(zoo)
library(ggplot2)

data <- read_excel("Macro_project/Finland_data.xlsx", sheet = 2)

data$time <- as.Date(as.yearqtr(data$time))


str(data)

View(data)

# take logs

data_n <- data %>% mutate(
  log_Y = log(Y),
  log_C = log(C),
  log_I = log(I),
  log_K = log(K),
  log_N = log(N)
)

View(data_n)

# HP filter, smoothing coefficient 1600

hp_log_Y <- hpfilter(data_n$log_Y, freq = 1600)
hp_log_C <- hpfilter(data_n$log_C, freq = 1600)
hp_log_I <- hpfilter(data_n$log_I, freq = 1600)
hp_log_K <- hpfilter(data_n$log_K, freq = 1600)
hp_log_N <- hpfilter(data_n$log_N, freq = 1600)


