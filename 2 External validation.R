# External validation on HKU-SG model (Taiwan)
# HKU team: J Lam & J Quan 
# file version: 2018-08-13

##########################################################
rm(list = ls())

# read Stata data
library (foreign); newdata <- read.dta("your_file.dta")
# read Stata 13 data
library(readstata13); newdata <- read.dta13("your_file.dta")
# read csv data
newdata <- read.csv("your_file.csv") # if csv format

# newdata = your dataset
str(newdata)
# check continuous variables are coded as numerical, e.g. newdata$age<-as.numerical(newdata$age)
# check categorial variables are coded as TRUE/FALSE, e.g. newdata$female<-as.logical(newdata$female)
# check smoking are coded as "Ex"/"Non-smoker"/"Yes", e.g. newdata$smoking<-as.factor(newdata$smoking)

# newdata$years = time to event
# newdata$event = event status
# newdata$duration = duration of diabetes
# newdata$chronic_kidney = chronic kidney disease
# newdata$cerebrovascular = cerebrovascular disease
# newdata$ischemic_heart = ischaemic heart disease

# newdata$sbp = systolic blood pressure 
# newdata$dbp = diastolic blood pressure
# newdata$ldl = LDL-cholesterol (mmol/L)


# Mortality: 5-year risk prediction score---

dat <- data.frame(years=newdata$years, event=newdata$event)
# censor after 5 years - better performance
dat$event[dat$event == 2 & dat$years > 5] <- 1

dat$pred <- with(newdata, 100*(1-0.912^exp(2.727159
                                           + 0.02659452*age+4.075628e-5*(age-41)^3-0.0001070358*(age-58)^3  
                                           +7.311264e-5*(age-70)^3-6.833147e-6*(age-85)^3  
                                           + 0.2650322*duration-0.01608406*(duration-0.04654346)^3  
                                           +0.01883374*(duration-0.9609856)^3-0.00277583*(duration-6.466804)^3  
                                           +2.614735e-5*(duration-22.96235)^3  
                                           -0.1983312*(female==T) 
                                           -0.3118533*(smoking=="Ex")-0.6109742*(smoking=="Non-smoker")
                                           +0.5252391*(atrial_fibrillation==T) 
                                           +1.077321*(chronic_kidney==T) 
                                           +0.4913603*(cerebrovascular==T)
                                           +0.2324324*(ischemic_heart==T)
                                           -0.3320009*hba1c+0.06135776*(hba1c-5.6)^3-0.1198288*(hba1c-6.6)^3  
                                           +0.05774934*(hba1c-7.6)^3+0.0007216831*(hba1c-11.6)^3  
                                           -0.006923551*sbp+3.548158e-6*(sbp-108)^3-8.185037e-6*(sbp-130)^3  
                                           +4.343557e-6*(sbp-145)^3+2.93321e-7*(sbp-174)^3  
                                           -0.00510383*dbp+8.585339e-6*(dbp-58)^3-1.604159e-5*(dbp-71)^3  
                                           +4.674797e-6*(dbp-80)^3+2.781449e-6*(dbp-96)^3  
                                           -0.1802774*ldl+0.03426755*(ldl-1.62)^3-0.06139979*(ldl-2.6606)^3  
                                           +0.01499461*(ldl-3.3636)^3+0.01213762*(ldl-4.73)^3  
                                           -0.0506029*bmi+0.0003252084*(bmi-19.7)^3-0.0004954199*(bmi-23.95)^3  
                                           +2.750309e-5*(bmi-26.83)^3+0.0001427083*(bmi-33.08)^3))) 

# Bootstrap 95% CI for c-index
library(boot)
boot_c_index <- function(mydata,i){
  mydata$years <- mydata$years[i]
  mydata$event <- mydata$event[i]
  mydata$pred <- mydata$pred[i]
  return(survConcordance(Surv(years, event) ~ pred, mydata)$concordance)
}
# bootstrapping with 100 replications
set.seed(1)
results <- boot(dat, boot_c_index, R=100)
# get 95% confidence interval
boot.ci(results, type="basic")
results.ci <- boot.ci(results, type="basic")

c_index <- c(results$t0, "lower" = results.ci$basic[4], "upper" = results.ci$basic[5])
c_index

# Calibration using one (imputed) dataset
library(pec)
library(reshape2) 
library(tidyr)

# Calibration (Full model)-----------------------------------------------------------------
# cut data.frame into 100 groups
dataset <- newdata
dataset$pred_surv <- as.numeric(1 - predictSurvProb(test_model, dataset, times=5))
# event after 5 years is set to censored status
table(dataset$event)
dataset$event[dataset$event == 2 & dataset$years > 5] <- 1
table(dataset$event)
# predict survival probability and observed probability for overall
dataset <- dataset[order(dataset$pred_surv),]
dataset <- dataset[, c("event", "time", "pred_surv"),]
dataset <- dataset %>%
  arrange(pred_surv) %>%
  mutate(group = cut2(pred_surv, g=100),
         years = time / 365.25,
         event = as.numeric(event))
levels(dataset$group) <- c(1:100)
observed <- rep(NA, 100)
observed <- rep(NA, 100)
for (i in 1:100) {
  km <- survfit(Surv(years, event)~1, data=dataset[dataset$group==i, ])
  survest <- stepfun(km$time, c(1, km$surv))
  observed[i] <- 1-survest(5) # risk at 5-years
}
mean_pred <- tapply(dataset$pred_surv, dataset$group, mean)
dat <- data.frame(observed, pred = mean_pred, percentile=1:100)
observed <- rep(NA, 100)
for (i in 1:100) {
  km <- survfit(Surv(years, event)~1, data=dataset[dataset$group==i, ])
  survest <- stepfun(km$time, c(1, km$surv))
  observed[i] <- 1-survest(5) # risk at 5-years
}
mean_pred <- tapply(dataset$pred_surv, dataset$group, mean)
dat <- data.frame(observed, pred = mean_pred, percentile=1:100)
dat.melt <- melt(dat, id.vars = "percentile", variable.name = "group", value.name = "risk")

# ggplot lines
ggplot_lines <- function (dat.melt){
  ggplot(dat.melt, aes(x = percentile, y = risk*100, color=factor(group))) +
    geom_line(size=1)     +
    labs(x="Percentile", y="5 year risk (%)") +
    theme(axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          plot.title=element_text(size=12)) +
    scale_x_continuous(breaks=seq(0, 100, 20)) +
    scale_y_continuous(breaks=seq(0, 100, 10)) +
    scale_colour_manual(values = c("observed"="coral2", "pred"="turquoise3"),
                        labels=c("observed" = "Observed","pred" = "Predicted")) +
    theme_minimal() +
    theme(legend.position="bottom", legend.title=element_blank(), legend.text = element_text(size = 12, face="bold"))
}

# Results
row1 <- c("data"="mortality", c_index)
row1
figure1 <- ggplot_lines(dat.melt) + ggtitle ("HK-SG model")
figure1
