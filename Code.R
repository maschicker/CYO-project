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
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")



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
library(ggridges)

options(scipen = 999)

##### Load data set #####
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






####### PRE-PROCESSING DATA ########

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


### erase status ID A320
O2C <- O2C %>% filter(statusID != "A320")

### how many unique sales_ID (=order lines)?
orig_salesID <- n_distinct(O2C$salesID)
orig_salesID


### erase Service orders (services are no hardware and follow a separate workflow ==> excluded from the project)
#amount of rows associated with service
table(O2C$divisiongroup)
O2C <- O2C %>% filter(divisiongroup != "SERV")
serv_salesID <- orig_salesID - n_distinct(O2C$salesID)
rem_orders <- n_distinct(O2C$salesID)

# DS.PB hold the same information, so divisiongroup can be dropped
O2C <- O2C%>% select(-"divisiongroup")

### erase "P90X"-products (These are mostly administrative hours)
#amount of rows with P90X-items
nrow(O2C[(str_detect(O2C$itemID, "P90")),])
#erase rows
O2C <- O2C[!(str_detect(O2C$itemID, "P90")),]

P90_salesID <- rem_orders - n_distinct(O2C$salesID)
rem_orders <- n_distinct(O2C$salesID)

### erase correction projects (correction projects follow a separate workflow and will be excluded in this project)
# count how many rows concerning correction projects are in the database 
table(O2C$corr_project)

O2C <- O2C %>% filter(corr_project == 0)
O2C <- O2C %>% select(-"corr_project")

corrproj_salesID <- rem_orders - n_distinct(O2C$salesID)
rem_orders <- n_distinct(O2C$salesID)


### erase intercompany orders (IC-orders always have a data twin and would bias the data)
table(O2C$division)
O2C <- O2C %>% filter(division != "IC")
# amount of order line cleaned
ic_salesID <- rem_orders - n_distinct(O2C$salesID)
rem_orders <- n_distinct(O2C$salesID)


### look for duplicate entries
problems <- which(duplicated(O2C))
length(problems)

### find and count problematic order lines (salesID) with the same statusID multiple times
O2C %>% find_duplicates(salesID, statusID) %>% nrow()

### for each combination of salesID and statusID only keep the row with the earlier timestamp
O2C <- O2C %>% arrange(created_dt) %>% distinct(salesID, statusID, .keep_all=TRUE)



# check if duplicates have been successfully eliminated
#O2C %>% find_duplicates(salesID, statusID) %>% nrow()



### convert to wide data in order to have one row per salesID
O2C_wide <- O2C %>% pivot_wider(
  id_cols = c(salesID, country, DS.PB, division, PROJID, itemID, sum_qty, sum_m2, channel_name), 
  names_from = statusID, 
  values_from = c(created_dt, days_on_status)
)


### Create column for complete orders
### Create 2 x calculated row for total lead time <== Target values
### Create 2 columns for start_year and start_month as factors
### round ordered m² to full integer
### Drop duration columns, as they are only needed for lead time calculation

O2C_wide <- O2C_wide %>% mutate(order_complete = (!is.na(created_dt_A400)),
                                lt_105 = as.numeric(difftime(created_dt_A400, created_dt_A105, units = "days"))+days_on_status_A400,
                                lt_000 = as.numeric(difftime(created_dt_A400, created_dt_A000, units = "days"))+days_on_status_A400,
                                start_year = as.factor(year(created_dt_A000)),
                                start_month = as.factor(month(created_dt_A000)),
                                sum_m2 = round(sum_m2,0)
                                ) %>%
                          select(-days_on_status_A000,
                                 -days_on_status_A100,
                                 -days_on_status_A105,
                                 -days_on_status_A300,
                                 -days_on_status_A330,
                                 -days_on_status_A340,
                                 -days_on_status_A310
                                )


### investigate new data frame
summary(O2C_wide)

# orders in the data frame:
wide_salesID <- nrow(O2C_wide)

#### handle incomplete orders
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

#### eliminate unfinished orders and drop column
O2C_wide <- O2C_wide %>% filter(order_complete == 1) %>% select(-order_complete)
# amount of incomplete orders erased and remaining orders
incompl_salesID <- wide_salesID - nrow(O2C_wide)
rem_orders <- nrow(O2C_wide)

# NA´s per column
NAs_per_column <- colSums(is.na(O2C_wide))%>%
  as.data.frame()%>% rownames_to_column()
colnames(NAs_per_column) <- c("columns", "NAs")

#visualization of NA´s per order status    
NAs_per_column%>%filter(NAs>0 & str_detect(columns, "created")) %>%
  ggplot(aes(x=columns, y=NAs))+
  geom_bar(stat="identity")+
  ylab("orders not passing this status")+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

### Drop created_dt_Axxx columns not needed != A000/A105/A400
O2C_wide <- O2C_wide %>% select(-created_dt_A300,
                                -created_dt_A100,
                                -created_dt_A330,
                                -created_dt_A340,
                                -created_dt_A310)

#### eliminate orders without A000 timestamp
O2C_wide <- O2C_wide %>% filter(!is.na(created_dt_A000))

#amount of erased rows with NA in A000-timestamp and remaining orders
noA000_salesID <- rem_orders - nrow(O2C_wide)
rem_orders <- nrow(O2C_wide)


#### handling NA´s in A105 timestamp
# which division by year?
O2C_wide %>% group_by(division, start_year)%>%
  summarize(A105_NA = sum(is.na(created_dt_A105)),
            n = n(),
            prop = A105_NA/n) %>%
  ggplot(aes(x=start_year, y= A105_NA))+
  geom_bar(stat="identity")+
  ylab("orders not passing A105")+
  facet_wrap(.~ division)

# what proportion by year?
O2C_wide %>% group_by(division, start_year)%>%
  summarize(A105_NA = sum(is.na(created_dt_A105)),
            n = n(),
            prop = A105_NA/n) %>%
  ggplot(aes(x=start_year, y= prop))+
  geom_bar(stat="identity")+
  ylab("proportion of orders not passing A105")+
  facet_wrap(.~ division)

# for all FH-orders ==> copy A000 timestamp to A105
O2C_wide <- O2C_wide %>% mutate(created_dt_A105 = ifelse(division=="FH" & is.na(created_dt_A105), created_dt_A000, created_dt_A105))

# for all other divisions ==> erase rows w/NA in column "created_dt_A105"
O2C_wide <- O2C_wide %>% filter(!is.na(created_dt_A105))

# recalculate lt_105 and lt_000
O2C_wide <- O2C_wide %>% mutate(lt_105 = as.numeric(difftime(created_dt_A400, as_datetime(created_dt_A105), units = "days"))+days_on_status_A400,
                                lt_000 = as.numeric(difftime(created_dt_A400, created_dt_A000, units = "days"))+days_on_status_A400) %>%
  select(-days_on_status_A400)

#amount of erased rows with NA in A105-timestamp
noA105_salesID <- rem_orders - nrow(O2C_wide)


### removing predictors with non-unique values or zero-variation
nzv <- nearZeroVar(O2C_wide, saveMetrics = TRUE)
nzv[,3:4]


### aligning O2C and O2C_wide
salesIDs <- O2C_wide$salesID
O2C <- O2C %>% filter(salesID %in% salesIDs)

### dropping unused level from O2C_wide and O2C
O2C <- droplevels(O2C)
O2C_wide <- droplevels(O2C_wide)

###total proportion of kept order lines
prop_salesID <- nrow(O2C_wide)/orig_salesID



####### DATA VISUALIZATION #######


# How many different orders total/per year / per country
O2C_wide %>% ggplot(aes(x=start_year))+
  geom_bar()+
  facet_grid(country~DS.PB)


# difference between LT105 and LT000
O2C_wide %>% mutate(lt_diff = lt_000-lt_105) %>%  
  ggplot(aes(x=lt_diff))+
  geom_histogram(binwidth = 1)+
  facet_grid(start_year~DS.PB)+
  xlim(0,30)+
  ylim(0,2000)+
  labs(title = "Lead time comparison", x="Lead time difference [days]")





### mixed correlation as provided on stackoverflow: ###

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


# create dataframe with selected variables
d <- data.frame(country = O2C_wide$country,
                DS.PB = O2C_wide$DS.PB,
                division = O2C_wide$division,
                itemID = O2C_wide$itemID,
                QTY = O2C_wide$sum_qty,
                m2 = O2C_wide$sum_m2,
                LT_000 = O2C_wide$lt_000,
                LT_105 = O2C_wide$lt_105,
                year = O2C_wide$start_year,
                month = O2C_wide$start_month,
                channel_name = O2C_wide$channel_name
)
d_corr <- mixed_assoc(d) # <== use for numerical analysis
d_corr

# create a networkplot
d_corr %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot()



### days on status per status - distribution deep dive for DS.PB und over time

# ridgeline for quick overview by DS.PB
O2C %>% mutate(year= year(created_dt),
               month=month(created_dt),
)%>%
  ggplot(aes(x=days_on_status, y=statusID, fill=statusID))+
  stat_density_ridges(alpha=0.6,
                      quantile_lines = TRUE,
                      quantiles = c(0.05, 0.95),
                      scale = 5)+
  theme_ridges()+
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"))+
  xlim(0,50)+
  labs(title = "Days on status by Business model w/ 5%/95%-quantile", x="")+
  facet_grid(.~DS.PB)

# Boxplot for different Business models per year
O2C %>% mutate(year= year(created_dt),
               month=month(created_dt),
)%>%
  ggplot(aes(x=statusID, y=days_on_status))+
  geom_boxplot(
    outlier.alpha = 0.3,
    na.rm = TRUE,
    show.legend = TRUE,
    varwidth = TRUE)+
  scale_y_log10()+
  facet_grid(year~DS.PB)

# ridgeline for quick overview by country
O2C %>% mutate(year= year(created_dt),
               month=month(created_dt),
)%>%
  ggplot(aes(x=days_on_status, y=statusID, fill=statusID))+
  stat_density_ridges(alpha=0.6,
                      quantile_lines = TRUE,
                      quantiles = c(0.05, 0.95),
                      scale = 5)+
  theme_ridges()+
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"))+
  xlim(0,50)+
  labs(title = "Days on status by country incl. 5%/95%-quantile", 
       x="")+
  facet_grid(.~country)



# lead time per order DV/FH
O2C_wide %>% group_by(DS.PB)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  ggplot(aes(x=DS.PB, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Direct Sales / Retail", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# lead time per division
O2C_wide %>% group_by(division)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  ggplot(aes(x=division, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Division", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# lead time per channel_name
O2C_wide %>% group_by(channel_name)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  arrange(avg)%>%
  filter(n>2000)%>%
  ggplot(aes(x=channel_name, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Channel", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# lead time per start year
O2C_wide %>% group_by(start_year)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  ggplot(aes(x=start_year, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Year", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# lead time per start month
O2C_wide %>% group_by(start_month)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  ggplot(aes(x=start_month, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Month", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#lead time per itemID
O2C_wide %>% group_by(itemID)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  filter(n>500)%>%
  ggplot(aes(x=itemID, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="ItemID", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#lead time per order size QTY
O2C_wide %>% group_by(sum_qty)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  ggplot(aes(x=sum_qty, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Order-QTY", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_log10()

#lead time per order size m²
O2C_wide %>% group_by(sum_m2)%>%
  summarize(n=n(),
            avg=mean(lt_105),
            med=median(lt_105),
            se= sd(lt_105)
  )%>%
  ggplot(aes(x=sum_m2, y=avg, ymin=avg-se, ymax=avg+se, col="avg"))+
  geom_point()+
  geom_point(aes(y=med, col="median"))+
  geom_errorbar(aes(alpha=0.3), show.legend = FALSE)+
  labs(title="Order-m²", 
       y="Lead time 105-410",
       color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_log10()



  
  
# raster itemID vs. country/division, colored by lt_105
  
O2C_wide %>% ggplot(aes(x=country, y=as.factor(itemID), fill=lt_105))+
              geom_raster()+
              scale_fill_gradient2(limits=c(0,100),
                                   low="blue", 
                                   mid="yellow", 
                                   high="red", 
                                   midpoint = 20)+
              labs(title="Lead time per item and country",
                   x="",
                   y="items",
                   fill = "Lead time [days]"
                   )+
              theme(axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1)
                    )




####### MODELING #######
##### INITIALIZATION #####

#### CREATE TRAINING, TESTING and VALIDATION SET
### drop all columns in O2C_wide, that are not used as predictors and factorize variables
O2C_wide_final <- O2C_wide %>% mutate(itemID= as.factor(itemID),
                                      )%>%
                                select(-lt_000,
                                -created_dt_A000,
                                -created_dt_A105,
                                -created_dt_A400,
                                -salesID,
                                -PROJID)


### Split O2C into 10% Validation and 90% main data set
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = O2C_wide_final$lt_105, times = 1, p = 0.1, list = FALSE)
O2C_val <- O2C_wide_final[test_index,]
O2C_wide_tt <- O2C_wide_final[-test_index,]

### Split test&training (tt) data set into 90% training and 10% test data set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = O2C_wide_tt$lt_105, times = 1, p = 0.1, list = FALSE)
O2C_test <- O2C_wide_tt[test_index,]
O2C_train <- O2C_wide_tt[-test_index,]

### create a small data subset to tryout code fast
set.seed(1, sample.kind="Rounding")
try_index <- createDataPartition(y = O2C_train$lt_105, times = 1, p = 0.01, list = FALSE)
O2C_try <- O2C_train [try_index,]


#DEFINE RMSE-FUNCTION
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Calculate Mu (average)
mu <- mean(O2C_train$lt_105) 
mu

#set up results dataframe
rmse_results <- data.frame(method = character(),
                           RMSE = numeric())

#str(rmse_results)






###### MODELS ######

#### MODEL 1 - AVERAGE ####
avg_rmse <- RMSE(O2C_test$lt_105, mu)

# add results to dataframe to compare performance of models#
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="1 - Average",
                                     RMSE = avg_rmse ))
rmse_results %>% knitr::kable()





#### MODEL 2 - separate AVERAGES FOR DS/PB####
#calculate separate avg per business model
DS_avg <- mean(O2C_train %>% filter(DS.PB=="DS")%>%.$lt_105)
PB_avg <- mean(O2C_train %>% filter(DS.PB=="PB")%>%.$lt_105)

#predict according to business model
DSPB_avg_pred <- ifelse(O2C_test$DS.PB =="DS", DS_avg, PB_avg)
DSPB_avg_rmse <- RMSE(O2C_test$lt_105, DSPB_avg_pred)

# add results to dataframe to compare performance of models#
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="2 - Average by DS/PB",
                                     RMSE = DSPB_avg_rmse ))
rmse_results %>% knitr::kable()



#### MODEL 3 - LDA ####
set.seed(1, sample.kind = "Rounding")
lda_train <- train(lt_105 ~division, method = "lda", data = O2C_try)
lda_predict <- predict(lda_train, O2C_test)

# add results to dataframe to compare performance of models#
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="2 - Average by DS/PB",
                                     RMSE = DSPB_avg_rmse ))
rmse_results %>% knitr::kable()



#### MODEL 4 - QDA ####
set.seed(1, sample.kind = "Rounding")
train_qda <- train(Survived ~Fare, method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set)

#### Logistic regression based on Age
set.seed(1, sample.kind = "Rounding")
train_glm <- train(Survived ~Age, method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set)





#### MODEL X - RANDOM FOREST #### <== open
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])


#### ENSEMBLES #### <== open

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


#Q2 - create matrix of predictions of each model
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)
head(pred)

#Q3 - compute accuracy for each model on the test set

mean(pred == mnist_27$test$y)
####official solution
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#Q4 - build an ensemble prediction by majority vote
pred_en <- vector(length = nrow(pred))
for(i in 1:nrow(pred)){
  pred_en[i] <- ifelse(mean(pred[i,]=="7")>0.5, "7", "2")
}
mean(pred_en == mnist_27$test$y)
####official solution
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5 - which individual models did better than the ensemble?
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]
