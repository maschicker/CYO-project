######### FINAL PROJECT - LEAD TIME #######
######### HarvardX - DATA SCIENCE ########

#######INSTALL PACKAGES#######
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(rpart)
library(matrixStats)
library(gam)
library(splines)


######## Load data set #######





#######PRE-PROCESSING DATA########

### data exploration
summary(edx)
dim(edx)
head(edx)
sapply(edx, class)

#data mutation
#turn countries to factors
raw_data$country <- as.factor(raw_data$country)


#look for duplicate entries
problems <- which(duplicated(edx))
length(problems)


#Correlation of predictors
d <- data.frame(userId = edx$userId,
                movieId = edx$movieId,
                rating = edx$rating,
                timestamp = edx$timestamp,
                rel_year = edx$rel_year,
                ts_year = edx$ts_year,
                ts_month = edx$ts_month,
                ts_day = edx$ts_day
)
corrplot(cor(d), method = "number")


# removing predictors with non-unique values or zero-variation
nzv <- nearZeroVar(edx, saveMetrics = TRUE)
nzv[,3:4]

#### various exploration
#How many distinct values and how often?  
as.data.frame(table(raw_data[,1]))

# How many different orders total/per year / per country

# How many correction projects total/per year / per country

# 


### data visualization

# histograms

# X=duration days y= n() faceted by Sparte and order status - flipcoord
