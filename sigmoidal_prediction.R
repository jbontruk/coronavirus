# Library
library(dplyr)

# Get data from ecdc.europa.eu
setwd('C:/Users/jaros/Downloads')
setwd('C:/Users/jaroslaw.bontruk/Downloads')
list.files()
file = list.files()[6]
data <- read.csv2(file, sep = ',', stringsAsFactors = F)
data <- data %>%
  rename(Country = countriesAndTerritories,
         Cases_Cum = Cases.CumSum)

# Get data from data.humdata.org
setwd('C:/Users/jaros/Downloads')
list.files()
file = list.files()[1]
data <- read.csv2(file, sep = ',', stringsAsFactors = F)
data <- data %>%
  rename(Country = Country.Region,
         Cases_Cum = Value)

# Fit Sigma function
fit_sigma <- function(country = "South_Korea", x_days = 100, y_lim = 5000) {
  country_data <- data %>% 
    filter(Country == country & DayNo != 0 & DayNo <= x_days) %>%
    arrange(DayNo)
  
  x = country_data$DayNo
  y = country_data$Cases_Cum

  fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data = data.frame(x, y))
  print(summary(fit))
  
  plot(y ~ x, xlim = c(0,120), ylim = c(0, y_lim), 
       xlab = 'DayNo', ylab = 'Cases', main = paste0('Cases in ', country))
  lines(seq(0, 120, length.out = 120),
        predict(fit, newdata = data.frame(x = seq(0, 120, length.out = 120))))
}

# Run simulaion
ctr <- 'Poland'
ylim <- 10000

fit_sigma(ctr, 4, ylim)
fit_sigma(ctr, 5, ylim)
fit_sigma(ctr, 6, ylim)
fit_sigma(ctr, 7, ylim)
fit_sigma(ctr, 8, ylim)
fit_sigma(ctr, 9, ylim)
fit_sigma(ctr, 10, ylim)
fit_sigma(ctr, 11, ylim)
fit_sigma(ctr, 12, ylim)
fit_sigma(ctr, 13, ylim)
fit_sigma(ctr, 14, ylim)
fit_sigma(ctr, 15, ylim)
fit_sigma(ctr, 16, ylim)
fit_sigma(ctr, 17, ylim)
fit_sigma(ctr, 18, ylim)
fit_sigma(ctr, 19, ylim)
fit_sigma(ctr, 20, ylim)
fit_sigma(ctr, 21, ylim)
fit_sigma(ctr, 22, ylim)
fit_sigma(ctr, 23, ylim)
fit_sigma(ctr, 24, ylim)
fit_sigma(ctr, 25, ylim)
fit_sigma(ctr, 26, ylim)
fit_sigma(ctr, 27, ylim)
fit_sigma(ctr, 28, ylim)
fit_sigma(ctr, 29, ylim)
fit_sigma(ctr, 30, ylim)
fit_sigma(ctr, 31, ylim)
fit_sigma(ctr, 32, ylim)
fit_sigma(ctr, 33, ylim)
fit_sigma(ctr, 34, ylim)
fit_sigma(ctr, 35, ylim)
fit_sigma(ctr, 36, ylim)
fit_sigma(ctr, 37, ylim)
fit_sigma(ctr, 38, ylim)
fit_sigma(ctr, 39, ylim)
fit_sigma(ctr, 40, ylim)
fit_sigma(ctr, 41, ylim)
fit_sigma(ctr, 42, ylim)
fit_sigma(ctr, 43, ylim)
fit_sigma(ctr, 44, ylim)
fit_sigma(ctr, 45, ylim)
fit_sigma(ctr, 46, ylim)
fit_sigma(ctr, 47, ylim)
fit_sigma(ctr, 48, ylim)
fit_sigma(ctr, 49, ylim)
fit_sigma(ctr, 50, ylim)
fit_sigma(ctr, 51, ylim)
fit_sigma(ctr, 52, ylim)
fit_sigma(ctr, 53, ylim)
fit_sigma(ctr, 54, ylim)
fit_sigma(ctr, 55, ylim)
fit_sigma(ctr, 56, ylim)
fit_sigma(ctr, 57, ylim)
fit_sigma(ctr, 58, ylim)
fit_sigma(ctr, 59, ylim)
fit_sigma(ctr, 60, ylim)
fit_sigma(ctr, 61, ylim)
fit_sigma(ctr, 62, ylim)
fit_sigma(ctr, 63, ylim)
fit_sigma(ctr, 64, ylim)
fit_sigma(ctr, 65, ylim)
fit_sigma(ctr, 66, ylim)
fit_sigma(ctr, 67, ylim)
fit_sigma(ctr, 68, ylim)
fit_sigma(ctr, 69, ylim)
fit_sigma(ctr, 70, ylim)
fit_sigma(ctr, 71, ylim)
fit_sigma(ctr, 72, ylim)
fit_sigma(ctr, 73, ylim)
fit_sigma(ctr, 74, ylim)
fit_sigma(ctr, 75, ylim)
fit_sigma(ctr, 76, ylim)
fit_sigma(ctr, 77, ylim)
fit_sigma(ctr, 78, ylim)
fit_sigma(ctr, 79, ylim)
fit_sigma(ctr, 80, ylim)
fit_sigma(ctr, 81, ylim)
fit_sigma(ctr, 82, ylim)
fit_sigma(ctr, 83, ylim)
fit_sigma(ctr, 84, ylim)
fit_sigma(ctr, 85, ylim)
fit_sigma(ctr, 86, ylim)
fit_sigma(ctr, 87, ylim)
fit_sigma(ctr, 88, ylim)
fit_sigma(ctr, 89, ylim)
fit_sigma(ctr, 90, ylim)
fit_sigma(ctr, 91, ylim)
fit_sigma(ctr, 92, ylim)
fit_sigma(ctr, 93, ylim)
fit_sigma(ctr, 94, ylim)
fit_sigma(ctr, 95, ylim)
fit_sigma(ctr, 96, ylim)
fit_sigma(ctr, 97, ylim)
fit_sigma(ctr, 98, ylim)
fit_sigma(ctr, 99, ylim)
fit_sigma(ctr, 100, ylim)