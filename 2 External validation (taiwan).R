# Codes for external validation
# HKU team: J Lam & J Quan 
# file version: 2018-08-13

rm(list=ls()); gc()
load("1 mortalityTW.Rdata") # load the dataset named "d"

library(pec)
library(reshape2) 
library(tidyr)
library(plyr)
library(Hmisc)
library(rms)

# ---------------------------------------------------
str(d); View(d)
d$event <- as.numeric(d$event)
d$years <- as.numeric(d$time / 365.25)
units(d$years) <- "year"
label(d$years) <- "Survival Time"

d$ratio <- as.numeric(d$ldl/d$hdl)
d$creatinine_mg<-d$creatinine*0.0113 # convert unit from umol/L to mg/dL

# Convert continuous data to categorical ----
# normal; abnormal; missing values
d$hba1c <- (ifelse(d$hba1c<7, "normal", "abnormal"))

d[d$female=="TRUE", c("creatinine_mg")] <- sapply(d[d$female=="TRUE", c("creatinine_mg")], function(x) ifelse(x >= 0.44 & x <= 1.13, "normal", "abnormal"))
d[d$female=="FALSE", c("creatinine_mg")] <- sapply(d[d$female=="FALSE", c("creatinine_mg")], function(x) ifelse(x >= 0.64 & x <= 1.27, "normal", "abnormal"))

d[d$female=="TRUE", c("ratio")] <- sapply(d[d$female=="TRUE", c("ratio")], function(x) ifelse(x < 3.22, "normal", "abnormal"))
d[d$female=="FALSE", c("ratio")] <- sapply(d[d$female=="FALSE", c("ratio")], function(x) ifelse(x < 3.55, "normal", "abnormal"))

d[, c("hba1c", "creatinine_mg", "ratio")][is.na(d[, c("hba1c", "creatinine_mg", "ratio")])] <- "missing"
d[, c("hba1c", "creatinine_mg", "ratio")] <- lapply(d[, c("hba1c", "creatinine_mg", "ratio")], as.factor)
dd <<- datadist(d); options(datadist ="dd")
str(d)

dat <- data.frame(years=d$years, event=d$event)
# censor after 5 years - better performance
dat$event[dat$event == 2 & dat$years > 5] <- 1

dat$pred <- with(d, 100*(1 - 0.946959409^exp((age*0.05532 
                                         + (female==FALSE)*0.19746 
                                         + (cancer==TRUE)*0.35057 
                                         + (htn==TRUE)*0.22759 
                                         + (antilipid==TRUE)*-0.50986  
                                         + (hba1c=="abnormal")*0.26331 
                                         + (hba1c=="missing")*0.20082 
                                         + (creatinine_mg=="abnormal")*0.8926 
                                         + (creatinine_mg=="missing")*-0.27562 
                                         + (ratio=="abnormal")*0.25423 
                                         + (ratio=="missing")*0.91676) 
                                         - (61.5121*0.05532)))
)

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

# Calibration using HK dataset

# Calibration (Full model)-----------------------------------------------------------------
# cut data.frame into 100 groups
dataset <- d
dataset$pred_surv <- with(d, (1 - 0.946959409^exp((age*0.05532 
                                              + (female==FALSE)*0.19746 
                                              + (cancer==TRUE)*0.35057 
                                              + (htn==TRUE)*0.22759 
                                              + (antilipid==TRUE)*-0.50986  
                                              + (hba1c=="abnormal")*0.26331 
                                              + (hba1c=="missing")*0.20082 
                                              + (creatinine_mg=="abnormal")*0.8926 
                                              + (creatinine_mg=="missing")*-0.27562 
                                              + (ratio=="abnormal")*0.25423 
                                              + (ratio=="missing")*0.91676) 
                                             - (61.5121*0.05532))))

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

dataset$event <- as.numeric(dataset$event-1)
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

