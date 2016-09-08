set.seed(1234)
BF = rnorm(50,10,2)
p = rnorm(50,20,4)
es = rnorm(50,40,8)
evidence = data.frame(BF,p,es)
dim(with(evidence, evidence[BF > 8 & BF < 12 & p > 18 & p < 22, ]))[1]
datfinal = subset(evidence,BF>8 & BF <12 & p > 18 & p < 22)
evidence$new <- factor(evidence$new,levels=c(6,10,14),
                      labels=c("low","medium","high")) 
evidence$p <- factor(evidence$p,levels=c(12,20,28),
                    labels=c("good","better","best")) 
evidence$es <- factor(evidence$es,levels=c(24,40,56),
                     labels=c("low","medium","high")) 
library(ggplot2)
qplot(BF, p, data=evidence, 
       size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")