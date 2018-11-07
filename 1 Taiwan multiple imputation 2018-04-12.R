# Development of Taiwan models for mortality with imputation
# HKU team: J Lam & J Quan 
# file version: 2018-04-09

# ----------------------------------------------

# MODEL 4: using continuous variables
rm(list=ls())
load("MI(cont)_20180406.Rdata") # load saved imputation results
load("mortalityTW.Rdata") # load your data, dataset is named as 'd'
ls()

str(d); View(d)

# format variables in dataset ----
d$event <- as.factor(d$event) # alive during follow-up coded as "0", death during follow-up as "1"
d$age <- as.numeric(d$age)
  # categorical variables formatted as factor
  d$female <- as.factor(d$female)
  d$cancer <- as.factor(d$cancer)
  d$htn <- as.factor(d$htn)
  d$antilipid <- as.factor(d$antilipid)
  # continuous variables formatted as numeric, keep hdl and ldl separated (unlike current TW model)
  d$bmi <- as.numeric (d$bmi)
  d$hba1c <- as.numeric (d$hba1c)
  d$hdl <- as.numeric (d$hdl)
  d$ldl <- as.numeric (d$ldl)
  d$creatinine <- as.numeric(d$creatinine)
  
# extract categorical variables ----
cat <- c(names(d)[sapply(d, is.factor)], names(d)[sapply(d, is.logical)])
d[, cat] <- lapply(d[, cat], as.factor)
sapply(d[, cat], table) # tabulate T/F results
colSums(is.na(d[, cat])) # see amount of missing data

# extract continuous variables ----
cont <- names(d)[sapply(d, is.numeric)]
colSums(is.na(d[, cont])) # see amount of missing data
miss <- sort(round(colSums(is.na(d[, cont])) / nrow(d), 2), decreasing = TRUE)
print(data.frame(miss)) # see % of missing data

#Visualise missing data
library(VIM)
missing_plot <- aggr(d, col=c('lightblue1','lightsalmon2'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# only continuous variables have missing data. Do imputation instead of coding as "missing value"", do imputation" 

# Number of imputations is set at [100 * (fraction of incomplete cases)]
# (Ref: Frank E Harrell, Regression Modeling Strategies, P.54)
sum(complete.cases(d)) 
round(100 * (sum(!complete.cases(d)) / nrow(d)), 0)
# number of imputations needed = 63

# visualise distribution of continuous variables with missing values ----
var <- cont[as.vector(sapply(d[, cont], function(cont) any(is.na(cont))))]
par(mfrow=c(2,3)) # configure output plot
for (i in 1:length(var)) {
  hist(d[, var[i]], main=var[i])
}

# since distribution of creatinine is right-skewed, visualise distribution after log transformation ----
par(mfrow=c(2,3)) # configure output plot
for (i in 1:length(var)) {
  heading <- paste("log", var[i])
  hist(log(d[, var[i]]), main=heading)
}

# do log transformation for creatinine
d$log.creatinine <- log(d$creatinine)

# Using Nelson-Aalen estimate (of cumulative hazard to survival time) rather than log.t
# (Ref: White & Royston (2009) "Imputing missing covariate values for the Cox model")
d$event <- as.numeric(d$event)-1 # code event as numeric 0/1
str(d)
d$chr <- nelsonaalen(data=d, timevar=time, statusvar=event) #chr stands for cumulative hazard rate

par(mfrow=c(1,1)) # configure output plot
plot(x = d$years, y = d$chr, ylab='Cumulative hazard rate', xlab='Year', main="Nelson-Aalen Estimate of Cumulative Hazard Rate")

# construct model with all variables 
d$event <- as.factor(d$event) # code event as factor (categorical)
str(d)
fm_imp_cont <- as.formula(~ female + age + event + bmi + hba1c + hdl + ldl + cancer + htn + antilipid + log.creatinine + chr) 
fm_imp_cont

# do imputation
before <- proc.time()
'imp <- aregImpute(fm_imp_cont, data = d, n.impute = 5, nk=4)'
(proc.time() - before)/60/60 # find out how many hours imputation took

imp
View(imp$imputed$bmi) # View e.g. imputed results of BMI
save(d, imp, file = "MI(cont)_20180409.Rdata") # save imputation results
 
# create "years" for survival model development
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
label(d$years) <- "Survival Time"

d$event <- as.numeric(d$event)-1

# full model development
fm_cont <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + rcs(hba1c, 4) + rcs(log.creatinine, 4) + rcs(hdl, 4) + rcs(ldl, 4) + htn + antilipid + cancer + rcs(bmi, 4))
dd <<- datadist(d); options(datadist ="dd")
fullmodel_cont <- fit.mult.impute(fm_cont, fitter = cph, xtrans = imp, data = d, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel_cont
# Obs 678750  Events 143047
summary(fullmodel_cont)

# internal validation
c_index_bootstrap <- function(model, bootstrap_n) {
  set.seed (1)
  v <- rep(NA, bootstrap_n)
  for (i in 1:bootstrap_n) {
    v[i] <- (validate(model, B=1)["Dxy", "index.corrected"])
  }
  # c-index = Dxy/2+0.5
  quantile(v, c(0.5, 0.025, 0.975))/2+0.5 
}

c_index_bootstrap(fullmodel_cont, bootstrap_n=5)
# 50%      2.5%     97.5% 
# 0.8148307 0.8143856 0.8158286

# Replace NAs in dataset with imputed values (using values from 1st iteration)
imputed <-impute.transcan(imp, data=d, imputation=1, list.out=TRUE, check=FALSE, trantab=FALSE) 
head(imputed) # see imputed values are marked by asterisk
completed_cont<-d # copy original data to new dataset
completed_cont[names(imputed)] <- imputed # NAs are replaced
View(completed_cont)

# Model development with multiple imputation results

# reformat imputed variables as numeric
completed_cont$event <- as.numeric(completed_cont$event)
completed_cont$bmi <-as.numeric (completed_cont$bmi)
completed_cont$hba1c <- as.numeric (completed_cont$hba1c)
completed_cont$hdl <- as.numeric (completed_cont$hdl)
completed_cont$ldl <- as.numeric (completed_cont$ldl)
completed_cont$log.creatinine <- as.numeric(completed_cont$log.creatinine)
completed_cont$years <- as.numeric(completed_cont$years)
                                   
fullmodel_completed <- cph(fm_cont, data = completed_cont, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel_completed
# Obs 678750  Events 143047

# Calibration plot ---- 
newdata <- completed_cont
test_model <- fullmodel_completed
source("run_calibration_plot.R")
figure1

graph <-impute.transcan(imp, data=d, imputation=1, list.out=TRUE, check=FALSE, trantab=TRUE) 
ggplot(graph, aes(x=bmi), scale=FALSE)
densityplot(imp)

#------------------------------------------------------------
# MODEL 5: using categorical variables and HDL/LDL ratio

load('mi(categorical)_20180406.Rdata') # load saved imputation results
load("mortalityTW.Rdata") # load your data, dataset is named as 'd'
ls()

str(d) 

# format variables in dataset as TW model ----
d$event <- as.factor(d$event) # alive during follow-up coded as "0", death during follow-up as "1"
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
label(d$years) <- "Survival Time"
d$age <- as.numeric(d$age)
d$creatinine_mg<-d$creatinine*0.0113 # unit converted from umol/L (in HK data) to mg/dL (in TW model), may not be needed in your dataset
d$ratio <- as.numeric(d$ldl/d$hdl)

# Convert continuous data to categorical ----
# normal; abnormal; missing values
d$hba1c <- ifelse(d$hba1c<7, "normal", "abnormal")

d[d$female=="TRUE", c("creatinine_mg")] <- sapply(d[d$female=="TRUE", c("creatinine_mg")], function(x) ifelse(x >= 0.44 & x <= 1.13, "normal", "abnormal"))
d[d$female=="FALSE", c("creatinine_mg")] <- sapply(d[d$female=="FALSE", c("creatinine_mg")], function(x) ifelse(x >= 0.64 & x <= 1.27, "normal", "abnormal"))
d$creatinine_mg<-(d$creatinine_mg)

d[d$female=="TRUE", c("ratio")] <- sapply(d[d$female=="TRUE", c("ratio")], function(x) ifelse(x < 3.22, "normal", "abnormal"))
d[d$female=="FALSE", c("ratio")] <- sapply(d[d$female=="FALSE", c("ratio")], function(x) ifelse(x < 3.55, "normal", "abnormal"))

d$bmi <- ifelse(d$bmi<24, "normal", "abnormal")

d[, c("hba1c", "creatinine_mg", "ratio", "bmi")] <- lapply(d[, c("hba1c", "creatinine_mg", "ratio", "bmi")], as.factor)
dd <<- datadist(d); options(datadist ="dd")

str(d)

# extract risk factors included in model ----
rf.cat <- c(names(d)[sapply(d, is.factor)], names(d)[sapply(d, is.logical)])
d[, rf.cat] <- lapply(d[, rf.cat], as.factor)
rf <- c(rf.cat, "age")
sapply(d[, rf], table) # tabulate results
colSums(is.na(d[, rf])) # see amount of missing data
miss <- sort(round(colSums(is.na(d[,rf])) / nrow(d[,rf]), 2), decreasing = TRUE)
print(data.frame(miss)) # see % of missing data

# Number of imputations is set at [100 * (fraction of incomplete cases)]
# (Ref: Frank E Harrell, Regression Modeling Strategies, P.54)
sum(complete.cases(d[,rf])) 
round(100 * (sum(!complete.cases(d[,rf])) / nrow(d[,rf])), 0)
# number of imputations needed = 63

library(Hmisc); library(mice); library(rms)

# Using Nelson-Aalen estimate (of cumulative hazard to survival time) rather than time / log.t
# (Ref: White & Royston (2009) "Imputing missing covariate values for the Cox model")
d$event <- as.numeric(d$event)-1 # code event as numeric 0/1
str(d)
d$chr <- nelsonaalen(data=d, timevar=time, statusvar=event)

par(mfrow=c(1,1)) # configure output plot
plot(x = d$time/365.25, y = d$chr, ylab='Cumulative hazard rate', xlab='Year')

# construct model with all variables 
d$event <- as.factor(d$event) # code event as factor (categorical)
str(d)
fm_imp_cat <- as.formula(~ age + event + female + hba1c + creatinine_mg + ratio + htn + antilipid + cancer + bmi + chr)
fm_imp_cat

# do imputation
before <- proc.time()
'imp <- aregImpute(fm_imp_cat, data = d, n.impute = 5, nk=4)'
(proc.time() - before)/60/60 # find out how many hours imputation took

imp
View(imp$imputed$bmi) # View e.g. imputed results of BMI
save(d, imp, file = "MI(categorical)_20180409.Rdata") # save imputation results

str(d)
d$event <- as.numeric(d$event)-1 # convert event to numeric for model development
str(d)

# full model development
fm_cat <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + hba1c + creatinine_mg + ratio + htn + antilipid + cancer + bmi)
dd <<- datadist(d); options(datadist ="dd")
fm_cat <- fit.mult.impute(fm_cat, fitter = cph, xtrans = imp, data = d, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fm_cat
# Obs 678750  Events 143047
summary(fm_cat)

# internal validation
c_index_bootstrap(fm_cat, bootstrap_n=5)
# 50%      2.5%     97.5% 
# 0.8066561 0.8063862 0.8077712

# Replace NAs in dataset with imputed values (using values from 1st iteration)
imputed <-impute.transcan(imp, data=d, imputation=1, list.out=TRUE, check=FALSE) 
head(imputed) # see imputed values are marked by asterisk
completed_cat<-d # copy original data to new dataset
completed_cat[names(imputed)] <- imputed # NAs are replaced
View(completed_cat)

# Model development with multiple imputation results
fullmodel_completed <- cph(fm_cat, data = completed_cat, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel_completed
# Obs 678750  Events 143047

# Calibration plot ---- 
dataset <- completed_cat
test_model <- fullmodel_completed
source("run_calibration_plot 20180409.R")
figure1