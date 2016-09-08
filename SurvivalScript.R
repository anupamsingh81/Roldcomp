CTVS <- read.csv("F:/Survival/Survival.txt")
library("survival")
myctvssurv <- Surv(CTVS$Time,CTVS$Event)
myctvssurv
myctvssurvkm <- survfit(myctvssurv~1,conf.int = FALSE)
myctvssurvkm
summary(myctvssurvkm)
ggsurv(myctvssurvkm)
myctvsssurv2 <- survfit(Surv(Time,Event) ~ FACTOR, data = CTVS)
myctvsssurv2
ggsurv(myctvsssurv2)
plot1 <- ggsurv(myctvsssurv2)
med.surv <- data.frame(time = c(49.4,49.4, 44.7,44.7), quant = c(.5,0,.5,0),
                       fACTOR = c('AORTA', 'AORTA', 'DOUBLE', 'DOUBLE' ))
plot1 + geom_line(data = med.surv, aes(time, quant, group = fACTOR),
                col = 'darkblue', linetype = 3) +
  geom_point(data = med.surv, aes(time, quant, group =fACTOR), col = 'darkblue')


survdiff(Surv(Time,Event) ~ FACTOR, rho=0, data=CTVS)


plot2 <- plot1 + 
head(Valve)
str(Valve)
library("survival")
attach(Valve)
Valve.surv <- survfit(Surv(Time,Event) ~ 1, data = Valve)
plot(Valve.surv)
ggsurv(Valve.surv)
Valve.surv2 <- survfit(Surv(Time,Event) ~ FACTOR, data = Valve)
ggsurv(Valve.surv2)
Valve.surv3 <- survfit(Surv(Time,Event) , data = Valve)