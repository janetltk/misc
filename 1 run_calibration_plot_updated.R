# Calibration plot -----------------------------------------------------------------
library(pec)
library(reshape2) 
library(tidyr)
library(plyr)
library(Hmisc)

# cut data.frame into 100 groups
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
    scale_colour_manual(values = c("observed"="lightpink2", "pred"="lightskyblue"),
                        labels=c("observed" = "Observed","pred" = "Predicted")) +
    theme_minimal() +
    theme(legend.position="bottom", legend.title=element_blank(), legend.text = element_text(size = 12, face="bold"))
}
figure1 <- ggplot_lines(dat.melt) + ggtitle ("Taiwan model")