install.packages("tidyverse")
install.packages("ggplot2")
install.packages("maps")
install.packages("lubridate")
install.packages("astsa")
install.packages("knitr")
install.packages("printr")
install.packages("plyr")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("readxl")
install.packages("readr")
install.packages("tibble")


library(tidyverse)
library(ggplot2)
library(maps)
library(lubridate)
library(astsa)
library(knitr)
library(plyr)
library(dylyr)
library(gridExtra)
library(reshape2)
library(readxl)
library(readr)
library(datasets)
library(tibble)

options(scipen = 999)



# World Map of Life Expectancy of Countries Being Evaluated

world_map <- map_data("world")

## Data for World Map
country_data <- data.frame(
  country = c("Benin", "Congo","Gabon", "Ireland", "Oman", "South Korea", "Sudan", "Urugray", "Zimbabwe"),
  life_expectancy = c(62, 65, 66, 82, 78, 83, 65, 78,61))

## Condition for World Map
top_countries <- head(country_data[order(country_data$life_expectancy, decreasing = TRUE), ], 220)

## Merge of Map data and Country Data                                   
world_map_data <- merge(world_map, top_countries, by.x = "region", by.y = "country", all.x = TRUE)

# World Map displaying life expectancy of the average citizen in countries relevant to report                                  
ggplot() +
  geom_polygon(data = world_map_data, aes(x = long, y = lat, group = group, fill = life_expectancy), color = "lightgrey") +
  scale_fill_gradient(low = "red", high = "blue", name = "Life Expectancy") +
  theme_void() +
  labs(title = "Life Expectancy of Countries")



# Bar Chart of COVID-19 deaths between 2020 to 2020 in countries relevant to the report

## Data for 2020 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Deaths <- c(2230, 1491, 1520, 143)

## Bar Chart Data Frame
Covid_death_bar_chart_2020 <- data.frame(Category = Countries, Value = Deaths)

# 2020 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_2020$Value, names.arg = Covid_death_bar_chart_2020$Category, col = "blue", 
        main = "Covid Deaths in 2020", xlab = "Countries", ylab = "Deaths", ylim = c(0, 3000)) 


## Data for 2021 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Deaths <- c(3824, 2989, 4492, 6019)

## Bar Chart Data Frame
Covid_death_bar_chart_2021 <- data.frame(Category = Countries, Value = Deaths)

# 2021 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_2021$Value, names.arg = Covid_death_bar_chart_2021$Category, col = "blue", 
        main = "Covid Deaths in 2021", xlab = "Countries", ylab = "Deaths", ylim = c(0,8000))


## Data for 2022 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Deaths <- c(2383, 2989, 26582, 1400)

## Bar Chart Data Frame
Covid_death_bar_chart_2022 <- data.frame(Category = Countries, Value = Deaths)

# 2022 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_2022$Value, names.arg = Covid_death_bar_chart_2022$Category, col = "blue", 
        main = "Covid Deaths in 2022", xlab = "Countries", ylab = "Deaths", ylim = c(0,30000)) 


# Bar Chart of COVID-19 cases between 2020 to 2020 in countries relevant to the report

## Data for 2020 Bar Chat on COVID-19 Cases
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Cases <- c(85176, 128290, 56855, 15848)

## Bar Chart Data Frame
Covid_case_bar_chart_2020 <- data.frame(Category = Countries, Value = Cases)

# 2020 Bar Chart of COVID-19 Cases
barplot(Covid_case_bar_chart_2020$Value, names.arg = Covid_case_bar_chart_2020$Category, col = "darkgreen", 
        main = "Covid Cases in 2020", xlab = "Countries", ylab = "Cases", ylim = c(0,140000))

## Data for 2021 Bar Chat on COVID-19 Cases
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Cases <- c(610917, 176768, 554812, 390891)

## Bar Chart Data Frame
Covid_case_bar_chart_2021 <- data.frame(Category = Countries, Value = Cases)

# 2021 Bar Chart of COVID-19 Cases
barplot(Covid_case_bar_chart_2021$Value, names.arg = Covid_case_bar_chart_2021$Category, col = "darkgreen", 
        main = "Covid_Cases in 2021", xlab = "Countries", ylab = "Cases", ylim = c(0,700000)) 


## Data for 2022 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Cases <- c(993671, 94250, 28047388, 605249)

## Bar Chart Data Frame
Covid_case_bar_chart_2022 <- data.frame(Category = Countries, Value = Cases)

# 2020 Bar Chart of COVID-19 death
barplot(Covid_case_bar_chart_2022$Value, names.arg = Covid_case_bar_chart_2022$Category, col = "darkgreen", 
        main = "Covid_Deaths in 2022", xlab = "Countries", ylab = "Cases", ylim = c(0,30000000))

## Data for 2022 Bar Chat on COVID-19 Deaths - Excluding South Korea
Countries <- c("Ireland", "Oman", "Uruguay")
Cases <- c(993671, 94250, 605249)

## Bar Chart Data Frame
Covid_case_bar_chart_exc_SK_2022 <- data.frame(Category = Countries, Value = Cases)

# 2020 Bar Chart of COVID-19 death - Excluding South Korea
barplot(Covid_case_bar_chart_exc_SK_2022$Value, names.arg = Covid_case_bar_chart_exc_SK_2022$Category, col = "darkgreen", 
        main = "Covid_Deaths in 2022 - Excluding South Korea", xlab = "Countries", ylab = "Cases", ylim = c(0,1000000))


# Time Series of Monthly Coronavirus Deaths in Ireland

## Data 
Monthly_Covid_deaths_in_Ireland <- read_csv("Monthly Covid deaths in Ireland.csv")
Monthly_Covid_deaths_in_Ireland

## Conditions for Timespan used in Line Chart 
Monthly_Covid_deaths_in_Ireland <- ts(Monthly_Covid_deaths_in_Ireland,frequency = 12, start = c (2020, 1 ), end = c(2022, 12))
Monthly_Covid_deaths_in_Ireland

# Time Series ploting the peaks and falls in Covid-19 Cases
plot.ts(Monthly_Covid_deaths_in_Ireland)

## Decomposition of Time Series to determine further trends
Monthly_Covid_deaths_in_Ireland_Decomposed <- decompose(Monthly_Covid_deaths_in_Ireland)
Monthly_Covid_deaths_in_Ireland_Decomposed
plot(Monthly_Covid_deaths_in_Ireland_Decomposed)

# Seasonal Time Series of Coronavirus Deaths in Ireland between 2020 to 2023
Monthly_Covid_deaths_in_Ireland_Seasonal_Adj <- Monthly_Covid_deaths_in_Ireland_Decomposed$seasonal
plot.ts(Monthly_Covid_deaths_in_Ireland_Seasonal_Adj)



# Scatterplot investigating the relationship between Covid-19 Cases and Deaths due to Covid-19

# Scatterplot investigating the relationship between Covid-19 Cases and Deaths due to Covid-19

## Data
Scatterplot_Covid_Data <- read_delim("Scatterplot Covid Data.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Scatterplots Investigating the relationship between Covid-19 Cases and Deaths - Colour Based on Pop. Density
ggplot(Scatterplot_Covid_Data, aes(x = new_cases, y = new_deaths, color = country))+
  geom_point() +
  labs(x = 'Deaths due to Covid-19', y = 'Covid Cases-19', title = 'Scatterplot depicting the relationship between Covid-19 Cases and Death') +
  geom_smooth(method = 'lm', se = FALSE)


# Scatterplots Investigating the relationship between Covid-19 Cases and Deaths - Colour Based on Pop. Density
ggplot(Scatterplot_Covid_Data, aes(x = new_cases, y = new_deaths, color = population_density))+
  geom_point() +
  labs(x = 'Deaths due to Covid-19', y = 'Covid-19 Cases', title = 'Scatterplot depicting the relationship between Covid-19 Cases and Death') +
  geom_smooth(method = 'lm', se = FALSE)
