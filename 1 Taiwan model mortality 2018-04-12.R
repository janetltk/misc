# Development of Taiwan models for Mortality 
# HKU team: J Lam & J Quan 
# file version: 2018-04-12

library(rms); library(VIM); library(Hmisc); library(mice)

# load the dataset
load("mortalityTW.Rdata") # dataset is named "d"
'd <- read.csv(file="taiwan_data.csv")'
View(d)

# 5 modelling options are demonstrated here:
# OPTION 1: Using continuous variables and keeping HDL & LDL separate ----

str(d)
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
label(d$years) <- "Survival Time"
d$event <- as.numeric(d$event)

# Use complete case analysis
d <- na.omit(d)
dd <<- datadist(d); options(datadist ="dd")
# TW full model using all variables
fm_all <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + rcs(hba1c, 4) + rcs(log(creatinine), 4) + rcs(hdl, 4) + rcs(ldl, 4) + htn + antilipid + cancer + rcs(bmi, 4))

# Model development 
fm <- fm_all
fullmodel <- cph(fm, data = d, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel
# Obs 251331  Events 25751

# internal validation 
# Calculate c-index using 5 bootstraps as example
c_index_bootstrap <- function(model, bootstrap_n) {
  set.seed (1)
  v <- rep(NA, bootstrap_n)
  for (i in 1:bootstrap_n) {
    v[i] <- (validate(model, B=1)["Dxy", "index.corrected"])
  }
  # c-index = Dxy/2+0.5
  quantile(v, c(0.5, 0.025, 0.975))/2+0.5 
}

c_index_bootstrap(fullmodel, bootstrap_n=5) 
# c-index result as follows:
# 50%      2.5%     97.5% 
# 0.8030550 0.8005602 0.8063840 

# Calibration plot ---- 
newdata <- d
test_model <- fullmodel
source("run_calibration_plot_updated.R")
figure1

# --------------------------------------

# OPTION 2: Combine LDL & HDL intp "LDL/HDL RATIO" ----

# Create new variable: ratio
d$ratio <- as.numeric(d$ldl/d$hdl)
str(d)

# full model using all variables and 
fm2 <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + rcs(hba1c, 4) + rcs(log(creatinine), 4) + rcs(ratio, 4) + htn + antilipid + cancer + rcs(bmi, 4))

# Model development 
# Full model development ----
fm <- fm2
fullmodel2 <- cph(fm, data = d, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel2
# Obs 251331  Events 25751

# Calculate c-index using bootstrap as before
c_index_bootstrap(fullmodel2, bootstrap_n=5) 
# 50%      2.5%     97.5% 
# 0.8018472 0.7990486 0.8053393  

# (2.3) Calibration plot as before
newdata <- d
test_model <- fullmodel2
source("run_calibration_plot_updated.R")
figure1

# --------------------------------------

# OPTION 3: CURRENT TAIWAN VARIABLES including missing data ----
# Data is reloaded to include missing values ----
load("mortalityTW.Rdata")
str(d)
d$event <- as.numeric(d$event)
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
label(d$years) <- "Survival Time"

d$ratio <- as.numeric(d$ldl/d$hdl)
d$creatinine_mg<-d$creatinine*0.0113 # convert unit from umol/L to mg/dL

missing_plot <- aggr(d, col=c('lightsteelblue1','rosybrown1'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Convert continuous data to categorical ----
# normal; abnormal; missing values
d$hba1c <- (ifelse(d$hba1c<7, "normal", "abnormal"))

d[d$female=="TRUE", c("creatinine_mg")] <- sapply(d[d$female=="TRUE", c("creatinine_mg")], function(x) ifelse(x >= 0.44 & x <= 1.13, "normal", "abnormal"))
d[d$female=="FALSE", c("creatinine_mg")] <- sapply(d[d$female=="FALSE", c("creatinine_mg")], function(x) ifelse(x >= 0.64 & x <= 1.27, "normal", "abnormal"))

d[d$female=="TRUE", c("ratio")] <- sapply(d[d$female=="TRUE", c("ratio")], function(x) ifelse(x < 3.22, "normal", "abnormal"))
d[d$female=="FALSE", c("ratio")] <- sapply(d[d$female=="FALSE", c("ratio")], function(x) ifelse(x < 3.55, "normal", "abnormal"))

d$bmi <- (ifelse(d$bmi<24, "normal", "abnormal"))

d[, c("hba1c", "creatinine_mg", "ratio", "bmi")][is.na(d[, c("hba1c", "creatinine_mg", "ratio", "bmi")])] <- "missing"
d[, c("hba1c", "creatinine_mg", "ratio", "bmi")] <- lapply(d[, c("hba1c", "creatinine_mg", "ratio", "bmi")], as.factor)
dd <<- datadist(d); options(datadist ="dd")

# Model using categorical variables
fm_3 <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + hba1c + creatinine_mg + ratio + htn + antilipid + cancer + bmi)

# Model development 
fm <- fm_3
fullmodel3 <- cph(fm, data = d, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel3
# Obs    678750    Events 143047

# Calculate c-index using bootstrap as before
c_index_bootstrap(fullmodel3, bootstrap_n=5) # internal validation
# c-index result as follows:
# 50%      2.5%     97.5% 
# 0.8278429 0.8273641 0.8287655

# Calibration plot as before
dataset <- d
test_model <- fullmodel3
source("run_calibration_plot 20180409.R")
figure1

# try different scenarios: filter out cases all continuous variables missing
no.miss <- subset(d, !(bmi=="missing" & hba1c=="missing" & creatinine_mg=="missing" & ratio=="missing"))
sum(!is.na(no.miss$serial_no))

# Model development 
fm <- fm_3
fullmodel3 <- cph(fm, data = no.miss, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel3
# Obs    615578    Events 136071

# Calculate c-index using bootstrap as before
c_index_bootstrap(fullmodel3, bootstrap_n=5) # internal validation
# c-index result as follows:
# 50%      2.5%     97.5% 
# 0.8235763 0.8234027 0.8242209

# To obtain the risk score, e.g. for model3 ----
latex(fullmodel3)

# ---------------------------------------------------
# MODEL 4: using continuous variables
rm(list=ls())
load("MI(cont)_20180406.Rdata") # load saved imputation results
load("mortalityTW.Rdata") # load your data, dataset is named as 'd'
ls()

str(d); View(d)

# format variables in dataset ----
d$event <- as.factor(d$event) # alive during follow-up coded as "0", death during follow-up as "1"
d$age <- as.numeric(d$age)
# create "years" 
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
label(d$years) <- "Survival Time"

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

# only continuous variables have missing data. Do imputation on these variables" 

# Number of imputations is set at [100 * (fraction of incomplete cases)]
# (Ref: Frank E Harrell, Regression Modeling Strategies, P.54)
sum(complete.cases(d)) 
round(100 * (sum(!complete.cases(d)) / nrow(d)), 0)
# number of imputations needed = 63

# visualise distribution of continuous variables ----
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

d$event <- as.numeric(d$event)-1
# full model development
fm_cont <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + rcs(hba1c, 4) + rcs(log.creatinine, 4) + rcs(hdl, 4) + rcs(ldl, 4) + htn + antilipid + cancer + rcs(bmi, 4))
dd <<- datadist(d); options(datadist ="dd")
fullmodel_cont <- fit.mult.impute(fm_cont, fitter = cph, xtrans = imp, data = d, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)
fullmodel_cont
# Obs 678750  Events 143047
summary(fullmodel_cont)

# internal validation
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
source("run_calibration_plot_updated.R")
figure1