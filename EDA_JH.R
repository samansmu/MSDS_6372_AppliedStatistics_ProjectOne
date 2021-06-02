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
library(ggExtra)
library(corrplot)
library(Boruta)
library(mboost)
library(kernlab)
library(doParallel)
library(HH)

options(scipen=999) #prevent scientific notation

Life_Expectancy_Data <- read.csv("Life Expectancy Data.csv")

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



Life_Expectancy_Data$Diphtheria <- ifelse(Life_Expectancy_Data$Diphtheria < 15, Life_Expectancy_Data$Diphtheria * 10, Life_Expectancy_Data$Diphtheria)
Life_Expectancy_Data$Polio <- ifelse(Life_Expectancy_Data$Polio < 15, Life_Expectancy_Data$Polio * 10, Life_Expectancy_Data$Polio)
Life_Expectancy_Data$BMI <- ifelse(Life_Expectancy_Data$BMI < 9, Life_Expectancy_Data$BMI * 10, Life_Expectancy_Data$BMI)
Life_Expectancy_Data$Adult.Mortality <- ifelse(Life_Expectancy_Data$Adult.Mortality < 4, Life_Expectancy_Data$Adult.Mortality * 100, Life_Expectancy_Data$Adult.Mortality)

Life_Expectancy_Data$Adult.Mortality <- ifelse(Life_Expectancy_Data$Adult.Mortality < 45, Life_Expectancy_Data$Adult.Mortality * 10, Life_Expectancy_Data$Adult.Mortality)

Life_Expectancy_Data$Adult.Mortality <- ifelse(Life_Expectancy_Data$Adult.Mortality < 100 &  Life_Expectancy_Data$Life.expectancy < 65, Life_Expectancy_Data$Adult.Mortality * 10, Life_Expectancy_Data$Adult.Mortality)

Life_Expectancy_Data$Income.composition.of.resources <- ifelse(Life_Expectancy_Data$Income.composition.of.resources < .2, NA, Life_Expectancy_Data$Income.composition.of.resources)

Life_Expectancy_Data$Schooling <- ifelse(Life_Expectancy_Data$Schooling == 0, NA, Life_Expectancy_Data$Schooling)

Life_Expectancy_Data$Population <- ifelse(Life_Expectancy_Data$Population > 1000000000, NA, Life_Expectancy_Data$Population)

Life_Expectancy_Data$infant.deaths <- ifelse(Life_Expectancy_Data$infant.deaths > 750, Life_Expectancy_Data$infant.deaths * .1, Life_Expectancy_Data$infant.deaths)



aggr_plot <- aggr(Life_Expectancy_Data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Life_Expectancy_Data), cex.axis=.7, gap=3, 
                  ylab=c("Percent data missing","Combinations Missing"))


LifeExpData <- mice(Life_Expectancy_Data, method="rf")
LifeExpData <- complete(LifeExpData)

aggr_plot <- aggr(LifeExpData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(LifeExpData), cex.axis=.7, gap=3, 
                  ylab=c("Percent data missing","Combinations Missing"))



#Chceking missing values and gandling missing values
LifeExpData <- na.omit(Life_Expectancy_Data)
na.fail(LifeExpData)
#Missing values, pure data has zero missing values, good to start analysis
sum(is.na(LifeExpData))
#Data Summary, after clearning missing values
summary(LifeExpData)

md.pattern(LifeExpData)

LifeExpData %>%ggplot(aes(x = SchoolingGrouped)) + geom_bar() +
  ggtitle("Distribution of SchoolingGrouped") + coord_flip() + xlab("SchoolingGrouped") + ylab("Count")

LifeExpData %>%ggplot(aes(x = Status)) + geom_bar() +
  ggtitle("Distribution of Status") + coord_flip() + xlab("Status") + ylab("Count")

ggplot(LifeExpData, aes(x=Life.expectancy)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Adult.Mortality)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=infant.death)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Alcohol)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=percentage.expenditure)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Hepatitis.B)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Measles)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=BMI)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=under.five.deaths)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Polio)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Total.expenditure)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Diptheria)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=HIV.AIDS)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=GDP)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Population)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=thinness..1.19.years)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=thinness.5.9.years)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Income.composition.of.resources)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Schooling)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=SchoolingGrouped)) + 
  geom_histogram()

ggplot(LifeExpData, aes(x=Year, y=Life.expectancy)) + 
  geom_bar(stat = "identity")

LifeExpData %>% ggplot(aes(x = Year, y = Life.expectancy)) + 
  geom_point(position = "jitter") + 
  ggtitle("Year vs. Life Expectancy") + 
  xlab("Year") + ylab("Life Expectancy")


ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Adult.Mortality)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = infant.deaths)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Alcohol)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = percentage.expenditure)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Hepatitis.B)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Measles)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = BMI)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = under.five.deaths)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Polio)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Total.expenditure)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Diphtheria)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = HIV.AIDS)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = GDP)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Population)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Income.composition.of.resources)
ggstatsplot::ggscatterstats(data = LifeExpData, x = Life.expectancy, y = Schooling)


quant <- subset(LifeExpData, select=-c(Country, Status))
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(quant, type="full")

cormat<-rquery.cormat(cars, graphType="heatmap")



#Remove Total_expenditureGrouped (only 1 value)
#Remove thinness.5.9.years, thinness..1.19.years
#Remove Country
#Scatterplot - Remove Population, GDP, Diphtheria,
#              Total Expenditure, Polio, under.five.deaths,
#              Measles, Hepatitis.B, Percentage.expediture,
#              Alcohol, infant.deaths, 


set.seed(123)
smp_size <- floor(0.75 * nrow(LifeExpData))
train_ind  <- sample(seq_len(nrow(LifeExpData)), size = smp_size)
train <- LifeExpData[train_ind, ]
test <- LifeExpData[-train_ind, ]



#Let’s make default model.
model1 = lm(Life.expectancy~., data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)


#Boruta Feature Engineering
boruta.talent_train <- Boruta(Life.expectancy~., data = LifeExpData, doTrace = 2)
print(boruta.talent_train)

#take a call on tentative features
boruta.talent <- TentativeRoughFix(boruta.talent_train)
print(boruta.talent)

getSelectedAttributes(boruta.talent_train, withTentative = F)

plot(boruta.talent, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.talent$ImpHistory),function(i)
  boruta.talent$ImpHistory[is.finite(boruta.talent$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.talent$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.talent$ImpHistory), cex.axis = 0.7)


LifeExp_lm_1_full <- lm(Life.expectancy ~ Adult.Mortality + HIV.AIDS + Alcohol, data=train)
summary(LifeExp_lm_1_full)



LifeExp_lm_2_full <- lm(Life.expectancy ~ Income.composition.of.resources + Schooling + BMI, data=train)
summary(LifeExp_lm_1_full)


train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model.1 <- train(Life.expectancy ~ ., data = train,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model.1$results

step.model.1$bestTune

summary(step.model.1$finalModel)

coef(step.model.1$finalModel, 5)

