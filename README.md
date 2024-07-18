# Covid

---
title: "Analysing the impact of Covid-19"
subtitle: "Analysing the effects of Covid-19 on Irish Healthcare and Life expectancy against Covid-19 impacts on other nations" 
date: 2024-07-18
author: "Godwill Njekwe"
format: html
theme: sandstone
backgroundcolor: lightgrey
toc: true

---


## Introduction

### Overview of Covid-19

Coronavirus (Covid-19) is a **virus** that caused a ***global pandemic between 2020 to 2023***. COVID-19 is a transmissible virus that causes respiratory problems and is most dangerous to the elderly and individuals suffering from diagnosed conditions such as asthma, diabetes, heart conditions and lung disease.

COVID-19 is scientifically known as 2 (SARS-CoV-2) and originated from a factory in Wuhan, China in December 2019. By March 2020 Covid-19 had spread across the world and was recognised as a pandemic by the World Health Organisation (WHO). COVID-19 is *transmissible through airborne particles* thus requiring individuals to distance themselves to prevent the transmission of the virus. Additionally, ***COVID-19 symptoms range from asymptomatic to death***, thus it can be difficult to test for. *Covid testing was a major issue in 2020*.

Consequently, COVID-19 has led to social distancing, ***increased hospitalisations*** and a decline in global mental health due to stress, bereavement due to deaths from COVID-19 and lack of social interactions for individuals. However, vaccinations were rapidly developed and became publicly available by December 2020, resulting in *declining coronavirus cases and deaths in the first quarter of 2021*. Nevertheless, this trend was reversed due to slow vaccination rollouts, decreased attitudes toward social distancing and mutations of the COVID-19 virus that increased its transmissible between individuals.

### Introduction of Covid-19 to Ireland
The first confirmed case in Ireland was reported on the 26th of February 2020. Public health protocols were in place in Ireland before the first case was confirmed, however, they could not mitigate the rise of cases that occurred between March to July 2022. A task force was set up to respond to Covid-19 in Ireland and was led by Dr Holohan. Ireland’s coronavirus task force established travel restrictions, quarantine on travellers entering Ireland, lockdowns requiring social distancing and staying in bubbles with family or housemates, contact testing and mask mandates. 

```{r message=FALSE}
#| echo: false

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
library(gridExtra)
library(reshape2)
library(readxl)
library(readr)
library(datasets)
library(tibble)

options(scipen = 999)

```

## Spread of Covid-19 
### Response from government, businesses and society
Governments worldwide introduced mitigation measures during the public health emergency, including travel restrictions, lockdowns, business restrictions and closures, workplace hazard controls, mask mandates, quarantines, testing systems, and contact tracing of the infected.
The pandemic resulted in global recessions, widespread supply shortages, reduced pollution, and widespread closures of institutions and public areas. Telecommuting became common, and issues of discrimination, health equity, and public health versus individual rights were brought to the forefront. 
Moreover, the mental health effects of the pandemic continue to linger, with persistent heightened anxiety and increased rates of individuals experiencing depression and loneliness. Covid-19 deepened economic strain on families and educational disparities, particularly affecting disadvantaged children who had less access to telecommunications and study space than their peers.
Additionally, government support has prevented a substantial surge in unemployment, but long-term health conditions still prevent many individuals from returning to work, posing ongoing economic challenges.

## Investigation Details
To evaluate the impact of Covid-19 on the Irish economy, we will compare the Irish response to Covid-19 to eight other countries from four different continents. Our investigation includes five African nations which are Benin, Congo, Gabon, Sudan and Zimbabwe, two Asian nations which are Oman and South Korea, and one South American nation which is Uruguay.  

However, we will only compare Ireland’s COVID-19 response to Oman, South Korea and Uruguay due to similarities in living standards, life expectancy (expected life expectancy of 75 years) and human development index scores (above 0.8 – Human Development Score).

Our investigation showed that the Irish response to Coronavirus was successful and similar to other countries. However, we have determined differences in South Korea’s COVID-19 response and trends compared to Ireland and the other two nations this investigation focuses on analysing.

Below is a world map showing the life expectancy of the countries we are investigating.

## World Map showing Life Expectancy of Countries whose Coronavirus Response are being evaluated

```{r message=FALSE}
#| echo: false

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

```

This world map shows the difference in expected life expectancy between the nations we evaluate.  The life expectancy of the number of deaths relative to a country's population differs between nations. 

As seen, Irish, Omanis, South Koreans and Uruguayan individuals have a high life expectancy (expected to live over 75), resulting in Ireland, Oman, South Korea and Uruguay having large elderly populations, which are more likely to be significantly impacted by COVID-19. 

This became evident in 2022 when governments across the globe changed the COVID-19 response plans to include less social distancing and a decline in face-covering mandates during the beginning of vaccination roll outs, in addition to a seasonal peak of COVID-19 cases that occurred due to countries lowering restrictions for Christmas and a cold winter in 2022.

However, South Korea encountered higher mortality rates in 2022 than Ireland, Oman and Uruguay due to COVID-19 which is shown below through bar charts covering the deaths due to COVID-19 between 2020 to 2023. By analysing this data, we have deduced that population density has a material impact on coronavirus deaths and cases

Below are tables depicting COVID-19 cases and deaths between 2020 to 2022. 

## Bar Charts depicting Coronavirus Cases between 2020 to 2022

```{r message=FALSE}
#| echo: false

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
        main = "Covid Cases in 2021", xlab = "Countries", ylab = "Cases", ylim = c(0,700000)) 


## Data for 2022 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Cases <- c(993671, 94250, 28047388, 605249)

## Bar Chart Data Frame
Covid_case_bar_chart_2022 <- data.frame(Category = Countries, Value = Cases)

# 2020 Bar Chart of COVID-19 death
barplot(Covid_case_bar_chart_2022$Value, names.arg = Covid_case_bar_chart_2022$Category, col = "darkgreen", 
        main = "Covid Cases in 2022", xlab = "Countries", ylab = "Cases", ylim = c(0,30000000))

## Data for 2022 Bar Chat on COVID-19 Deaths - Excluding South Korea
Countries <- c("Ireland", "Oman", "Uruguay")
Cases <- c(993671, 94250, 605249)

## Bar Chart Data Frame
Covid_case_bar_chart_exc_SK_2022 <- data.frame(Category = Countries, Value = Cases)

# 2020 Bar Chart of COVID-19 death - Excluding South Korea
barplot(Covid_case_bar_chart_exc_SK_2022$Value, names.arg = Covid_case_bar_chart_exc_SK_2022$Category, col = "darkgreen", 
        main = "Covid Cases in 2022 - Excluding South Korea", xlab = "Countries", ylab = "Cases", ylim = c(0,1000000))

```

These four bar charts reveal how many coronavirus cases that were recorded in Ireland, Oman, South Korea and Uruguay between 2020 to 2022. Oman is shown to experience the most cases in 2020 and this trend is reversed in 2021 and 2022. Ergo, Oman implemented the most effective measures to reduce the spread of COVID-19. Moreover, South Korea was the most effective at mitigating the spread of COVID-19 in 2020 as it has a population more than ten times the size of Ireland, Oman and Uruguay but less cases than Ireland and Oman.

Population in 2020: South Korea = 51,836,239
                    Ireland = 4,985,382
                    Oman = 4,543,999
                    Uruguay = 3,429,086
                    
Population figures sourced from World Bank database.

Ergo, South Korea in 2020 has significantly less cases per capita than Ireland, Oman and Uruguay. Ireland and Uruguay were not as successful as South Korea in mitigating the spread of Covid-19 but were more effective than Oman. However, it must be noted that Uruguay were more successful than Ireland, however as a country in the European Union closing borders and restricting movement is difficult to implement as the European Union was founded on free movement of people, goods, and service. 

In 2021, Ireland performed quite poorly in mitigating the spread of coronavirus with more cases than Oman, South Korea and Uruguay. Additionally, this result when compared to South Korea is significantly poor due to South Korea's population being ten times larger than Ireland. Consequently, it could be argued Ireland being the only country on this list as part of international federation that allows for free movement across more than 30 countries, made it more difficult to implement COVID-19 lockdowns and quarantines. However, Uruguay also achieved similar results in relation to COVID-19 cases per capia.

South Korea in 2021, once again outperformed expectations in mitigating the spread of COVID-19 by achieving a similar number of cases as Ireland, Oman and Uruguay, despite the substantial difference in population between South Korea and the other countries we are investigating. While Oman were able to make a considerable improvement in mitigating the spread of COVID-19, suggesting they implemented new restrictions and implemented tighter more restrictive lockdowns.

The most significant insight that we obtained from COVID-19 case data was South Korea increase in cases in 2021. South Korea went from substantial outperforming Ireland, Oman and Uruguay in mitigating COVID-19. South Korea experienced more than 29 times the cases of Ireland and 290 times the cases of Iran, which is much more than the expected ten times the COVID-19 cases. 
Consequently, South Korea was excluded from a graph of 2022 Covid-19 cases to ensure results from Ireland, Oman and Uruguay accurately displayed COVID-19 cases. 

Moreover, Oman once again outperformed Ireland, South Korea and Uruguay in mitigating the spread of coronavirus. However, all countries experienced a rise in COVID-19 cases in 2022 as nations all across the world started to implement a Covid-19 vaccination roll-out and reduced COVID-19 restrictions such as lifting lockdowns, ending quarantines and reducing mask mandates. 

After analysing the data further we came to deduce that covid cases were impacted by age, seasons/ weather,events and population density. South Korea has the highest median age and population density and Ireland is second in median age and population density while Oman is the lowest in these to two categories.

We explore the relationship between population density and coronavirus through a scatterplot later in this investigation. Additionally, we use a timeseries to evaluate the relationship between coronavirus and weather, events and seasons.   

## Bar Charts depicting Coronavirus Deaths between 2020 to 2022

```{r message=FALSE}
#| echo: false

# Bar Chart of COVID-19 deaths between 2020 to 2020 in countries relevant to the report

## Data for 2020 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Deaths <- c(2230, 1491, 1520, 143)

## Bar Chart Data Frame
Covid_death_bar_chart_2020 <- data.frame(Category = Countries, Value = Deaths)

# 2020 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_2020$Value, names.arg = Covid_death_bar_chart_2020$Category, col = "blue", 
        main = "Covid Deaths in 2020", xlab = "Countries", ylab = "Deaths", ylim = c(0, 2500)) 


## Data for 2021 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Deaths <- c(3824, 2989, 4492, 6019)

## Bar Chart Data Frame
Covid_death_bar_chart_2021 <- data.frame(Category = Countries, Value = Deaths)

# 2021 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_2021$Value, names.arg = Covid_death_bar_chart_2021$Category, col = "blue", 
        main = "Covid Deaths in 2021", xlab = "Countries", ylab = "Deaths", ylim = c(0,7000))


## Data for 2022 Bar Chat on COVID-19 Deaths
Countries <- c("Ireland", "Oman","South Korea", "Uruguay")
Deaths <- c(2383, 2989, 26582, 1400)

## Bar Chart Data Frame
Covid_death_bar_chart_2022 <- data.frame(Category = Countries, Value = Deaths)

# 2022 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_2022$Value, names.arg = Covid_death_bar_chart_2022$Category, col = "blue", 
        main = "Covid Deaths in 2022", xlab = "Countries", ylab = "Deaths", ylim = c(0,30000)) 

## Data for 2022 Bar Chat on COVID-19 Deaths - Excluding South Korea
Countries <- c("Ireland", "Oman", "Uruguay")
Deaths <- c(2383, 2989, 1400)

## Bar Chart Data Frame
Covid_death_bar_chart_exc_SK_2022 <- data.frame(Category = Countries, Value = Deaths)

# 2022 Bar Chart of COVID-19 death
barplot(Covid_death_bar_chart_exc_SK_2022$Value, names.arg = Covid_death_bar_chart_exc_SK_2022$Category, col = "blue", 
        main = "Covid Deaths in 2022 - Excluding South Korea", xlab = "Countries", ylab = "Deaths", ylim = c(0,3500))

```

These bar charts show general trends in COVID-19 response by nations which are deaths rose between 2020 to 2021 and start to decline in 2022 when COVID-19 vaccinations increased in availability, except for the previously explained exceptions which are South Korea.

In relation to coronavirus deaths in 2020, Ireland and Oman were not effective as Uruguay or South Korea in treating coronavirus and mitigating deaths.Ireland was the least effective in preventing deaths at the beginning of the pandemic which could be due to having the least amount control of implementing quarantines and lockdowns due to free movement within the European Union and European Economic Area. 

South Korea in 2020 significantly outperformed countries in preventing COVID-19 deaths, particularly concerning coronavirus cases per capita. This result ges against our deductions, however after research we discovered South Korea COVID-19 response plan was the most effective as South Korea implemented the most restricitive COVID-19 restrictions, which were reduced over time leading to what we expect to occur, which is higher COVID-19 mortality rates in 2021 and particularly 2022. Additionally, Uruguay is also effective at preventing Covid-19 deaths.

In 2021, South Korea once again outperformed what the expected COVID-19 death moralities should be, if population per case is accounted for. Oman's Covid response plan and mitigation measures are slightly more effective than Ireland's in preventing COVID-19 deaths. However, Ireland experiences the smallest growth in new COVID-19 deaths in percentage terms, illustrating Ireland is becoming much more efficient in preventing COVID-19 deaths. Which suggests Ireland is becoming better at treating those hospitalised with COVID-19 and successful implemention of the Irish vaccination roll-out in late 2021.

Uruguay is ineffective in preventing COVID-19 deaths and has the most deaths due to coronavirus from the countries which this study is investigating. Uruguay's COVID-19 response plan can be considered particularly ineffective when compared to South Korea who achieved less deaths due to COVID-19 than Uruguay and less COVID deaths per capita than Ireland and Oman. 

In 2021, the is a general trend of COVID-19 deaths declining which occurs in the countries we investigated and evaluated except for South Korea. Thus, suggesting COVID-19 vaccinations were successful preventing coronavirus deaths but not cases. 

However, this was not true with South Korea which will believe was due to age and population density, We found that population density had a rlantionship with increasing coronavirus cases and deaths. South Korea is most more dense than other countries with a vulnerable elderly generation living in close proximity with other less vulnerable society groups. Due to coronavirus being asymptomatic and South Korea lifting Covid-19 restrictions, COVID-19 is ble to become more deadly and spread throughout South Korean society without being noticed to increased deaths are recorded in the summer and winter of 2022.  

We were able to conclude COVID-19 deths experienced peaks in winter and would rise throughout summer after falling in the firt few months of 2020,2021 and 2022. Consequently, we have determined that COVID-19 deaths and cases were seasonal with various peaks and falls between 2020 to 2022.

This is illustrated below in our table and time series of coronavirus deaths in Ireland between 2020 to 2022.


## Time Series and Table of Monthly Coronavirus deaths in Ireland

```{r message=FALSE}
#| echo: false

# Time Series of Monthly Coronavirus Deaths in Ireland

## Data 
Monthly_Covid_deaths_in_Ireland <- read_csv("Monthly Covid deaths in Ireland.csv")

## Conditions for Timespan used in Line Chart 
Monthly_Covid_deaths_in_Ireland <- ts(Monthly_Covid_deaths_in_Ireland,frequency = 12, start = c (2020, 1 ), end = c(2022, 12))

# Time Series ploting the peaks and falls in Covid-19 Cases
plot.ts(Monthly_Covid_deaths_in_Ireland)

```

Time Series shows the trends we concluded in relation COVID-19 deaths in Ireland. We have deduced that more restrictions should have been implemented in summer, autumn and winter. We believe less coronavirus deaths and cases would occur if restrictions were not lifted and reintroduced when the was surge in cases. Nonetheless, if COVID-19 restrictions implemented after a coronavirus surge were maintained throughout the year economic conditions would worsen and the mental health crisis intensified. Further research on the costs and benefits of preventing coronavirus deaths or increasing poverty and loneliness is recommended. 

## Decomposition of Time Series to determine further trends

```{r message=FALSE}
#| echo: false

## Decomposition of Time Series to determine further trends
Monthly_Covid_deaths_in_Ireland_Decomposed <- decompose(Monthly_Covid_deaths_in_Ireland)
plot(Monthly_Covid_deaths_in_Ireland_Decomposed)
```

We decomposed our time series of monthly COVID-19 deaths in Ireland to expound on trends in coronavirus effects on Ireland and determine further trends. Our decomposition allowed us to determined Coronavirus deaths peaked in early 2020 and fell unto early 2021 and then rose. The general trend is a V-shape that portrays tht it was difficult to present COVID-19 deaths in Ireland in early 2020 but over time this changed, until Ireland began to reduce COVID-19 restrictions. 

Additionally, we illustrated the seasonal trends of COVID-19 which shows falls in COVID-19 deaths between February to June and a rise in summer that peaks in winter. We believe this is due to Christmas and winter increasing interaction between individuals and weakening immune systems due to cold causing increased sickness. This would cause a rise in coronavirus deaths and cases that were reduced by the government implementing new COVID-19 restrictions and mandates, that were lifted in summer, which slowly began a new cycle that would peak in winter due to cold weather and Christmas encouraging parties, dinners in-store shopping and more social events.

## Seasonal Time Series - Smoothed Time Series

```{r message=FALSE}
#| echo: false

# Seasonal Time Series of Coronavirus Deaths in Ireland between 2020 to 2023
Monthly_Covid_deaths_in_Ireland_Seasonal_Adj <- Monthly_Covid_deaths_in_Ireland_Decomposed$seasonal
plot.ts(Monthly_Covid_deaths_in_Ireland_Seasonal_Adj)

```

As seen and explained before, COVID-19 deaths are seasonal and rise towards the end of the year in the build-up to Christmas (due to the cold of winter increasing the chance of flu and other sicknesses weakening individuals immune system).

Furthermore, we investigate if the is a link between coronavirus cases and deaths. We determine that there is a weak positive relationship between COVID-19 cases and deaths. Additionally, we discover that COVID-19 is more dangerous in dense populated areas and increases the probability of high cases and high deaths in a region. 

Our results displayed in the scatterplots below.

## Scatterplot depicting the relantionship between population density and Coronavirus cases and deaths

```{r message=FALSE}
#| echo: false
#| warning: false

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

```

We concluded that more population dense regions with high elderly population are at risk. This risk will be reduced after vaccinations, as illustrated in the COVID-19 cases bar charts and death bar charts showing increased COVID-19 cases but reduced COVID-19 deaths. However, the effect of vaccinations is less effective in countries which have large high urban population dense areas were surges in COVID-19 can spread very quickly without being detected. In additional, the age of individuals is likely to affect COVID-19 deaths illustrated by South Korea and Ireland experiencing more COVID-19 deaths and more COVID-19 deaths per person than other countries such as Oman and Uruguay.  

## Evaluation of Response to Coronavirus and Investigations Conclusion

WHO ended its classification of COVID-19 as a global pandemic on the 5th of May 2023. Nonetheless, the coronavirus is still circulating and mutating.  As of 12 July 2024, COVID-19 is reported to have caused 7,051,600 is reported deaths. The COVID-19 pandemic ranks as the fifth-deadliest pandemic/epidemic in history.

Studies such as the Health Foundation’s impact inquiry and WHO reports, clarify that coronavirus impacted certain economic and social groups more than others. Moreover, lower-income families in deprived regions experience mortality rates 3 to 4 times higher than other economic groups globally. However, the overall number of COVID-19 deaths is significantly lower than during the first year of the pandemic.

This study has discovered that population density, age and weather, events and social interaction has a material effect on coronavirus impact on society. High population density, large eldery populations cold weather, and holidays such as Christmas and Easter increase individuals hospitalised, ICU patients and coronavirus cases and deaths.

Additionally, COVID-19 impacts the elderly more than their younger individual and is seasonal and becomes much more dangerous in winter as immune systems weaken due to cold and social interaction in society increases during the build-up to Christmas. 

Moreover, population density can be significant determining facctor on the spread of coronavirus, with high dense areas being at greater risk of the virus. For instance population dense urban centres and countries such as Seoul and South Korea will suffer more from coronavirus than less populated urban centres and countries such as Dublin and Ireland. 


### Healthcare 
Vaccination programmes have been critical to reducing COVID-19 mortality rates. However, for individuals in less developed regions and nations vaccination uptake is still low, especially for people in poorer areas. Also, hospitals have changed their protocols and increased their bed and ICU capacity. Furthermore, countries have devoted more resources to formulating and implementing health protocols to mitigate the impacts of potential future pandemics. 

### Mental Health
Covid-19 also has significant mental health effects which are expected to linger with many members of society facing depression, loneliness and anxiety. Additionally, an impact on the social development of young children is expected.

In Ireland, the will be profound impact on young adults who received calculated grades for their Leaving Certificate results. Additionally many adults in third level education or work have developed a feeling ofbeing social isolated and have found it difficult adapting to competing task through zoom meetings and virtual solutions. Young children are missing vital years of social development learning social skills and how to adept to new environments such as school or nursery.

### Economy
Government support such as paying furlough support for employees in industries impacted and increasing society's social nets through social welfare and support payments prevented significant economic impacts on families and prevented surges in unemployment.  However long-term coronavirus cases pose a risk to the Irish and global economy and reduce the size of workforce in Ireland and in economies across the world. conditions are still preventing many individuals from returning to work, posing ongoing economic challenges.   

The Irish government reduced an unemployment crisis and mitigated a recession through furlough schemes and increasing social welfare reducing poverty and allowing those who were not essential workers to isolate and reduce the spread of the virus. Ireland was able to do this better than other countries with higher population density as lockdowns were easier to enforce as generally families and bubbles were able to spread out and interact less.

Further Information and data utilised in this investigation is available on Git Hub.

Thank you for your attention.

Data: 

In order -    World Map Data, 2020 COVID Cases Data (Bar Chart), 2021 COVID Cases Data (Bar Chart), 2022 COVID Cases Data (Bar Chart), 2020 COVID Cases Excluding South Korea Data (Bar Chart),2020 COVID Cases Data (Bar Chart), 2021 COVID Cases Data (Bar Chart), 2022 COVID Cases Data (Bar Chart), 2020 COVID Cases Excluding South Korea Data (Bar Chart), Monthly COVID Deaths (Time Series), Scatterplot Data
              
           

```{r message=FALSE}
#| echo: false

country_data
Covid_death_bar_chart_2020
Covid_death_bar_chart_2021
Covid_case_bar_chart_2022
Covid_case_bar_chart_exc_SK_2022
Covid_death_bar_chart_2020
Covid_death_bar_chart_2021
Covid_death_bar_chart_2022
Covid_death_bar_chart_exc_SK_2022
Monthly_Covid_deaths_in_Ireland

```

