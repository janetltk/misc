# Development of Taiwan models for Mortality 
# HKU team: J Lam & J Quan 
# file version: 2018-04-13

library(rms); library(VIM); library(Hmisc); library(mice)
setwd("/Volumes/secure/riskmodels/TW")
load("mortalityTW.Rdata") # load the dataset named "d"
'd <- read.csv(file="taiwan_data.csv")'

# ---------------------------------------------------
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
d$event <- as.numeric(d$event)-1 # code event as numeric 0/1
str(d)
d$chr <- nelsonaalen(data=d, timevar=time, statusvar=event) #chr stands for cumulative hazard rate

# construct formula with all variables 
d$event <- as.factor(d$event) # code event as factor (categorical)
str(d)
fm_imp_cont <- as.formula(~ female + age + event + bmi + hba1c + hdl + ldl + cancer + htn + antilipid + log.creatinine + chr) 
fm_imp_cont

# do imputation (could take hours depending on data size)
before <- proc.time()
imp <- aregImpute(fm_imp_cont, data = d, n.impute = 5, nk = 4)
(proc.time() - before)/60/60 # find out how many hours imputation took

imp
View(imp$imputed$bmi) # View e.g. imputed results of BMI
save(d, imp, file = "filename.Rdata") # save imputation results

load("filename.Rdata")
d$event <- as.numeric(d$event)-1
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
dd <<- datadist(d); options(datadist ="dd")

# full model development
fm_cont <- as.formula(Surv(years, event) ~ rcs(age, 4) + female + rcs(hba1c, 4) + rcs(log.creatinine, 4) + rcs(hdl, 4) + rcs(ldl, 4) + htn + antilipid + cancer + rcs(bmi, 4))
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
# imputed values are marked by asterisk
completed_cont <- d # copy original data to new dataset
completed_cont[names(imputed)] <- imputed # NAs are replaced
head(completed_cont)

# Calibration plot ---- 
dataset <- completed_cont
test_model <- fullmodel_completed
source("run_calibration_plot_updated.R")
figure1

# Model formula
latex(fullmodel_cont)

# mortality risk at 5-years
100*(1-0.909^exp(6.15215
                 +0.02855246*age+3.431428e-5*(age-41)^3-8.32653e-5*(age-58)^3
                 +4.922231e-5*(age-70)^3-2.712941e-7*(age-85)^3
                 -0.2378848*(female==TRUE)
                 -0.3001069*hba1c+0.08215744*(hba1c-5.6)^3-0.17445*(hba1c-6.6)^3  
                 +0.09476451*(hba1c-7.6)^3-0.002471974*(hba1c-11.7)^3  
                 -1.123334*log.creatinine+3.912268*(log.creatinine-3.931826)^3  
                 -9.983845*(log.creatinine-4.26268)^3  
                 +6.198407*(log.creatinine-4.488636)^3  
                 -0.1268295*(log.creatinine-5.099866)^3  
                 -0.7632311*hdl+1.504995*(hdl-0.76)^3-3.645661*(hdl-1.09)^3  
                 +2.133387*(hdl-1.32)^3+0.007279503*(hdl-1.91)^3  
                 -0.09795773*ldl-0.004602819*(ldl-1.569545)^3+0.05734351*(ldl-2.6209)^3  
                 -0.07816688*(ldl-3.3591)^3+0.02542619*(ldl-4.7)^3  
                 +0.5672903*(htn==TRUE)
                 -0.1183621*(antilipid==TRUE)
                 +0.6697368*(cancer==TRUE)
                 -0.06176018*bmi+0.0002595073*(bmi-19.73)^3-0.0002422071*(bmi-24.01)^3  
                 -0.0002012172*(bmi-26.89)^3+0.0001839169*(bmi-33.2)^3))
