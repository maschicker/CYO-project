######### FINAL PROJECT - LEAD TIME #######
######### HarvardX - DATA SCIENCE ########

#######INSTALL PACKAGES#######
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(hablar)) install.packages("hablar", repos = "http://cran.us.r-project.org")
if(!require(rcompanion)) install.packages("rcompanion", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")


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
library(rcompanion)

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
orig_salesID <- n_distinct(O2C$salesID)
orig_salesID


### erase status IDs A000 and A320
O2C <- O2C %>% filter(statusID != "A320")

### erase Service orders (services are no hardware and follow a separate workflow ==> excluded from the project)
#amount of rows associated with service
table(O2C$divisiongroup)
O2C <- O2C %>% filter(divisiongroup != "SERV")
serv_salesID <- orig_salesID - n_distinct(O2C$salesID)

### erase "P90X"-products (These are mostly administrative hours)
#amount of rows with P90X-items
nrow(O2C[(str_detect(O2C$itemID, "P90")),])
#erase rows
O2C <- O2C[!(str_detect(O2C$itemID, "P90")),]

P90_salesID <- serv_salesID - n_distinct(O2C$salesID)

### erase correction projects (correction projects follow a separate workflow and will be excluded in this project)
# count how many rows concerning correction projects are in the database 
table(O2C$corr_project)

O2C <- O2C %>% filter(corr_project == 0)
O2C <- O2C %>% select(-"corr_project")

corrproj_salesID <- P90_salesID - n_distinct(O2C$salesID)

### erase intercompany orders (IC-orders always have a data twin and would bias the data)
table(O2C$division)
O2C <- O2C %>% filter(division != "IC")

ic_salesID <- corrproj_salesID - n_distinct(O2C$salesID)

### look for duplicate entries
problems <- which(duplicated(O2C))
length(problems)

### find and count problematic order lines (salesID) with the same statusID multiple times
O2C %>% find_duplicates(salesID, statusID) %>% nrow()

### for each combination of salesID and statusID only keep the row with the earlier timestamp
O2C <- O2C %>% arrange(created_dt) %>% distinct(salesID, statusID, .keep_all=TRUE)

# check if duplicates have been successfully eliminated
O2C %>% find_duplicates(salesID, statusID) %>% nrow()



### convert to wide data in order to have one row per salesID
O2C_wide <- O2C %>% pivot_wider(
  id_cols = c(salesID, country, DS.PB, division, divisiongroup, PROJID, itemID, sum_qty, sum_m2, channel_name), 
  names_from = statusID, 
  values_from = c(created_dt, days_on_status)
)


### Create column for complete orders
### Create calculated row for total lead time <== Target value

O2C_wide <- O2C_wide %>% mutate(order_complete = (!is.na(created_dt_A400)),
                                lt_105 = as.numeric(difftime(created_dt_A400, created_dt_A105, units = "days"))+days_on_status_A400,
                                lt_000 = as.numeric(difftime(created_dt_A400, created_dt_A000, units = "days"))+days_on_status_A400
                                )


### investigate new data frame
# orders in the data frame:
wide_salesID <- nrow(O2C_wide)


#### exclude incomplete orders
#proportion of complete orders total
mean(O2C_wide$order_complete)

#unfinished orders per year (startdate)
O2C_wide %>% mutate(year = year(created_dt_A000)) %>%
            select(year, order_complete) %>%
            group_by(year)%>%
            summarise(unfinished_orders = 100*(1-mean(order_complete)),
                      n=n()) %>%
  ggplot(aes(x=year, y=unfinished_orders))+
  geom_bar(stat = "identity")+
  ylab("% unfinished orders")
  
#### eliminate unfinished orders
O2C_wide <- O2C_wide %>% filter(order_complete == 1)

# amount of incomplete orders erased
incompl_salesID <- wide_salesID - nrow(O2C_wide)

# NA´s per column

NAs_per_column <- colSums(is.na(O2C_wide))%>%
  as.data.frame()%>% rownames_to_column()
colnames(NAs_per_column) <- c("columns", "NAs")
    
#visualization of NA´s per order status    
NAs_per_column%>%filter(NAs>0 & str_detect(columns, "created")) %>%
  ggplot(aes(x=columns, y=NAs))+
  geom_bar(stat="identity")+
  ylab("orders not passing this status")+
  theme(axis.text.x = element_text(angle = 60))

#### eliminate orders without A000 timestamp

O2C_wide <- O2C_wide %>% filter(!is.na(created_dt_A000))

#amount of erased rows with NA in A000-timestamp
noA000_salesID <- incompl_salesID - nrow(O2C_wide)

#### replace NA´s in A105 timestamp with A100

O2C_wide <- O2C_wide %>% mutate(created_dt_A105)

O2C_wide$created_dt_A105[is.na(O2C_wide$created_dt_105),]

#amount of erased rows with NA in A105-timestamp
noA105_salesID <- noA000_salesID - nrow(O2C_wide)



### Correlation of predictors
#################################################################
####### mixed correlation as provided on stackoverflow: #########
#################################################################

# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}


###create dataframe with selected variables
d <- data.frame(country = O2C_wide$country,
                DS.BP = O2C_wide$DS.PB,
                division = O2C_wide$division,
                divisiongroup = O2C_wide$divisiongroup,
                itemID = O2C_wide$itemID,
                QTY = O2C_wide$sum_qty,
                m2 = O2C_wide$sum_m2,
                total_Lead_time = O2C_wide$total_lt
)
mixed_assoc(d)

#### create a networkplot
d %>%
  mixed_assoc() %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot()



# removing predictors with non-unique values or zero-variation
nzv <- nearZeroVar(O2C, saveMetrics = TRUE)
nzv[,3:4]

#### various explorations
#How many distinct values and how often?  


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




