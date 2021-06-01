library(tidyverse)
library(curl)
library(class)
library(e1071)
library(caret)
library(plotly)
library(fuzzyjoin)
library(RCurl)
library(selectr)
library(tidyselect)
library(mvtnorm)
library(stringr)
library(disdat)
library(carData)
library(caret)
library(plotly)
#library(dbplyr)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(GGally)
library(gridExtra)
library(psych)
library(ggpubr)
library(gridGraphics)
library(reshape2)
library(tuneGrid)
library(plyr)
library(randomForest)
library(earth)
library(corrplot)
library(Metrics)
library(readr)
library(Zelig)
library(faraway)
library(survival)
library(magrittr)
library(dbplyr)
library(sjmisc)
library(Hmisc)
library(visdat)
library(mice)
library(VIM)
library(ggstatsplot)


Life_Expectancy_Data <- read.csv("C:/Users/mcp/Desktop/Project1_Summer_2021/MSDS_6372_AppliedStatistics_ProjectOne/Life Expectancy Data.csv")

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text()) #the default for fivethirtyeight is to not show axis labels, this removes that default so we can choose to specify and display axis titles
theme_update(plot.title = element_text(hjust = 0.5)) # changing default to center all titles

View(Life_Expectancy_Data)


summary(Life_Expectancy_Data)
glimpse(Life_Expectancy_Data)


#Setting as factor Country and Status
Life_Expectancy_Data$Status=as.factor(Life_Expectancy_Data$Status)
Life_Expectancy_Data$Country=as.factor(Life_Expectancy_Data$Country)

#grouping education level and governmnet expenditure
Life_Expectancy_Data=Life_Expectancy_Data %>% 
  mutate(SchoolingGrouped = sjmisc::rec(Schooling, rec = "4.2:15.0=Undergarduate; 15.1:20.7=Graduate"))%>%
  mutate(Total_expenditureGrouped=sjmisc::rec("Total expenditure", rec = "0.74:3.99=Low; 4.00:14.39=High"))
Life_Expectancy_Data$Total_expenditureGrouped=as.factor(Life_Expectancy_Data$Total_expenditureGrouped)
Life_Expectancy_Data$SchoolingGrouped=as.factor(Life_Expectancy_Data$SchoolingGrouped)


md.pattern(Life_Expectancy_Data)

aggr_plot <- aggr(Life_Expectancy_Data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Life_Expectancy_Data), cex.axis=.7, gap=3, 
                  ylab=c("Percent data missing","Combinations Missing"))
# 
# tempData <- mice(Life_Expectancy_Data,m=5,maxit=10,meth='cart',seed=500)
# 
# aggr_plot <- aggr(tempData$data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(tempData$data), cex.axis=.7, gap=3, 
#                   ylab=c("Percent data missing","Combinations Missing"))


#Chceking missing values and gandling missing values
pureData <- na.omit(Life_Expectancy_Data)
na.fail(pureData)
#Missing values, pure data has zero missing values, good to start analysis
sum(is.na(pureData))
#Data Summary, after clearning missing values
summary(pureData)

md.pattern(pureData)

pureData %>%ggplot(aes(x = SchoolingGrouped)) + geom_bar() +
  ggtitle("Distribution of SchoolingGrouped") + coord_flip() + xlab("SchoolingGrouped") + ylab("Count")

pureData %>%ggplot(aes(x = Status)) + geom_bar() +
  ggtitle("Distribution of Status") + coord_flip() + xlab("Status") + ylab("Count")

ggplot(pureData, aes(x=Life.expectancy)) + 
  geom_histogram()

ggplot(pureData, aes(x=Adult.Mortality)) + 
  geom_histogram()

ggplot(pureData, aes(x=infant.death)) + 
  geom_histogram()

ggplot(pureData, aes(x=Alcohol)) + 
  geom_histogram()

ggplot(pureData, aes(x=percentage.expenditure)) + 
  geom_histogram()

ggplot(pureData, aes(x=Hepatitis.B)) + 
  geom_histogram()

ggplot(pureData, aes(x=Measles)) + 
  geom_histogram()

ggplot(pureData, aes(x=BMI)) + 
  geom_histogram()

ggplot(pureData, aes(x=under.five.deaths)) + 
  geom_histogram()

ggplot(pureData, aes(x=Polio)) + 
  geom_histogram()

ggplot(pureData, aes(x=Total.expenditure)) + 
  geom_histogram()

ggplot(pureData, aes(x=Diptheria)) + 
  geom_histogram()

ggplot(pureData, aes(x=HIV.AIDS)) + 
  geom_histogram()

ggplot(pureData, aes(x=GDP)) + 
  geom_histogram()

ggplot(pureData, aes(x=Population)) + 
  geom_histogram()

ggplot(pureData, aes(x=thinness..1.19.years)) + 
  geom_histogram()

ggplot(pureData, aes(x=thinness.5.9.years)) + 
  geom_histogram()

ggplot(pureData, aes(x=Income.composition.of.resources)) + 
  geom_histogram()

ggplot(pureData, aes(x=Schooling)) + 
  geom_histogram()

ggplot(pureData, aes(x=SchoolingGrouped)) + 
  geom_histogram()

ggplot(pureData, aes(x=Year, y=Life.expectancy)) + 
  geom_bar(stat = "identity")

pureData %>% ggplot(aes(x = Year, y = Life.expectancy)) + 
  geom_point(position = "jitter") + 
  ggtitle("Year vs. Life Expectancy") + 
  xlab("Year") + ylab("Life Expectancy")


ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Adult.Mortality)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = infant.deaths)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Alcohol)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = percentage.expenditure)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Hepatitis.B)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Measles)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = BMI)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = under.five.deaths)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Polio)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Total.expenditure)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Diphtheria)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = HIV.AIDS)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = GDP)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Population)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Income.composition.of.resources)
ggstatsplot::ggscatterstats(data = pureData, x = Life.expectancy, y = Schooling)


#Remove Total_expenditureGrouped (only 1 value)
#Remove thinness.5.9.years, thinness..1.19.years
#Remove Country
#Scatterplot - Remove Population, GDP, Diphtheria,
#              Total Expenditure, Polio, under.five.deaths,
#              Measles, Hepatitis.B, Percentage.expediture,
#              Alcohol, infant.deaths, 