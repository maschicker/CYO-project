######### FINAL PROJECT - LEAD TIME #######
######### HarvardX - DATA SCIENCE ########

#######INSTALL PACKAGES#######
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(hablar)) install.packages("hablar", repos = "http://cran.us.r-project.org")

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
library(hablar)


######## Load data set #######
### load order-to-cash-data
### URL: http://marcoschicker.de/data/O2C.csv
dl <- tempfile()
download.file("http://marcoschicker.de/data/O2C.csv", dl)
df_O2C <- read.csv(dl)

### adapt classes


### load project data
### URL: http://marcoschicker.de/data/project.csv
dl2 <- tempfile()
download.file("http://marcoschicker.de/data/project.csv", dl2)
df_project <- unique(read.csv(dl2) %>% select(PROJID, channel_name, division, divisiongroup))

# eliminate duplicate PROJID and only keep the first
df_project <- df_project[!duplicated(df_project[,1]),]


###join project table to include direct sales organizations as possible predictors
O2C <- df_O2C %>% left_join(df_project, by="PROJID")






#######PRE-PROCESSING DATA########

### data exploration
summary(O2C)
dim(O2C)
head(O2C)
sapply(O2C, class)


#data mutation
#change classes and erase empty column (mod_dt) and duplicate column (division.y)
O2C <- O2C %>% mutate(country = as.factor(country),
                      DS.PB = as.factor(ifelse(DS.PB=="DV", "DS", "PB")),
                      division = as.factor(division.x),
                      statusID = as.factor(statusID),
                      created_dt = as_datetime(created_dt),
                      corr_project = as.factor(corr_project),
                      divisiongroup = as.factor(divisiongroup),
                      channel_name = as.factor(channel_name)
)%>%
  select(-"mod_dt", -"division.y", -"division.x", -"status_name")

### how many unique sales_ID (=order lines)?
n_distinct(O2C$salesID)

### find and count problematic order lines (salesID) with the same statusID multiple times
O2C %>% find_duplicates(salesID, statusID) %>% nrow()

### for each combination of salesID and statusID only keep the row with the earlier timestamp
O2C <- O2C %>% arrange(created_dt) %>% distinct(salesID, statusID, .keep_all=TRUE)

# check if duplicates have been successfully eliminated
O2C %>% find_duplicates(salesID, statusID) %>% nrow()


### erase status IDs A000 and A320 and drop column "status_name"
O2C <- O2C %>% filter(statusID != "A000" & statusID != "A320")%>%
  select(-"status_name")

### erase Service orders (services are no hardware and follow a separate workflow ==> excluded from the project)
#amount of rows associated with service
table(O2C$divisiongroup)
O2C <- O2C %>% filter(divisiongroup != "SERV")

### erase "P90X"-products (These are mostly administrative hours)
#amount of rows with P90X-items
nrow(O2C[(str_detect(O2C$itemID, "P90")),])
#erase rows
O2C <- O2C[!(str_detect(O2C$itemID, "P90")),]

### erase correction projects (correction projects follow a separate workflow and will be excluded in this project)
# count how many rows concerning correction projects are in the database 
table(O2C$corr_project)

O2C <- O2C %>% filter(corr_project == 0)
O2C <- O2C %>% select(-"corr_project")

### erase intercompany orders (IC-orders always have a data twin and would bias the data)
table(O2C$division)
O2C <- O2C %>% filter(division != "IC")


### convert to wide data in order to have one row per salesID
O2C_wide <- O2C %>% pivot_wider(
  id_cols = c(salesID, country, DS.PB, division, divisiongroup, PROJID, itemID, sum_qty, sum_m2, channel_name), 
  names_from = statusID, 
  values_from = c(created_dt, days_on_status)
)


### Create column for complete orders
### Create calculated row for total lead time <== Target value

O2C_wide <- O2C_wide %>% mutate(order_complete = (!is.na(created_dt_A400)),
                                total_lt = created_dt_A400 - created_dt_A105
)

### look for duplicate entries
problems <- which(duplicated(O2C_wide))
length(problems)


### Correlation of predictors
d <- data.frame(country = country,
                statusID = statusID,
                
)
corrplot(cor(d), method = "number")




# removing predictors with non-unique values or zero-variation
nzv <- nearZeroVar(O2C, saveMetrics = TRUE)
nzv[,3:4]

#### various explorations
#How many distinct values and how often?  
as.data.frame(table(df_project$proj_country))

# How many different orders total/per year / per country

# orders per project

# total run time per order DV/FH

# total run time per project DV/FH



### data visualization

# histograms

# X=duration days y= n() faceted by division and order status - flipcoord)

# mosaic itemID vs. country/division


####### MODELING #######

##### CREATE TRAINING, TESTING and VALIDATION SET #####

### Split O2C into 10% Validation and 90% main data set


### Split main data set into 90% training and 10% test data set


### create a small data subset to tryout code fast
try_index <- createDataPartition(y = , times = 1, p = 0.001, list = FALSE)
O2C_try <- O2C_train [try_index,]




