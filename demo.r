rm(list=ls())
graphics.off()
library(SciViews)
library(DAAG)
library(pracma)
library(pls)
library(vars)
library(dplyr)
library(xlsx)
library(doParallel)
library(glmnet)
library(changepoint)
library(randomForest)
library(xgboost)
cores = detectCores() - 1
registerDoParallel(cores)
source('decorrelate.r')
source('linear_source.r')
source('RF_source.r')
source('boosting_source.r')

## generate toys samples 
## let us generate 20 quarters of weekly data 
f1 = rnorm(n=20 * 13, mean=100 , sd = 25)
f2 = f1 + rnorm(n=20 * 13, mean=0 , sd = 25)
f_other = matrix(runif(20* 13 * 6, min = -10, max = 10), ncol = 6)
season = rep(1:4, each = 13, times = 5)
quarter_week = rep(1:13, each = 1, times = 20)
year = rep(2016:2020, each = 52, times = 1)
response = 0.1* f1 + f_other[,1] + runif(20* 13, min = -1, max = 1)

data =data.frame(cbind(response, f1, f2, f_other, year, season, quarter_week))
names(data) = c('response', 'f_1', 'f_1_similar',  'f_other1', 'f_other2', 'f_other3', 'f_other4', 'f_other5','f_other6', 'year', 'season', 'quarter_week')
row.names(data) = paste(data$year, "Q", data$season, "WW", data$quarter_week)

# change the time related into factors
data = data %>%
  mutate_each(funs(as.factor(.)), year, season, quarter_week)
summary(data)
#############################
# Before do analysis, make sure the response variable is the first column in the data frame and named as 'response'
#############################

#############################
# decorrelate step 
#############################
decor_result = decorrelate(data, 5, 5)
target_all1= decor_result[[1]]
decor_result[[3]] # call this to see whether 'f_r1' and 'f_r1_similar' are in the same group

KK = nrow(target_all1)
target = target_all1[1:(KK - 1),, drop= FALSE]
target_test = target_all1[KK,, drop = FALSE]
test_number = 40
Kmax = 3
nmodel = 20
sample_time = TRUE
rank1 = TRUE

lm_output = LM_ensemble_ts_para(target, target_test, nmodel, Kmax, test_number,sample_time, rank1)
rf_output = RF_ensemble_ts_para(target, target_test, nmodel, Kmax, test_number, sample_time, rank1)
boost_output = XGBoost_ensemble_ts_para(target, target_test, nmodel, Kmax, test_number, sample_time, rank1)

paste('linear model frct', lm_output[[1]])
paste('random forest model frct', rf_output[[1]])
paste('Boosting tress model frct', boost_output[[1]])
paste('average model frct', (lm_output[[1]] + rf_output[[1]] + boost_output[[1]])/3)
paste('acutual value is', target_test[1, 1])

paste('linear model top variables', lm_output[[3]][1,1], 'and', lm_output[[3]][1,2])
paste('random forest top variables', rf_output[[3]][1,1], 'and', rf_output[[3]][1,2])
paste('Boosting tress top variables', boost_output[[3]][1,1], 'and', boost_output[[3]][1,2])
paste('acutual related variables', 'f1 and f_other1')
