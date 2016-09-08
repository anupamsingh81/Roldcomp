library(xlsx)
write.xlsx(ichscore,"F:/mydata.xlsx" )

# Roc curve

# Generation of derived variables for GCS and Age
ichscore$GCSscore2 =  ifelse(ichscore$GCS<=4,2,ifelse(ichscore$GCS<=12,1,ifelse(ichscore$GCS<=15,0)))
ichscore$Age2 = ifelse(ichscore$Age<80,0,1)

# Calculation of Revised ICH score

ichscore$rev.ich = as.numeric(ichscore$VE)+ as.numeric(ichscore$volume)+ as.numeric(ichscore$tentorium)+ ichscore$Age2+ichscore$GCSscore2

# Plotting ROC curve and optimal cut off

library(ROCR)
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
 
# creating prediction and performance objects

pred = prediction(ichscore$rev.ich, ichscore$outcome)
perf <- performance(pred, "tpr", "fpr")

x = print(opt.cut(perf,pred ))

plot(perf)
library(plotROC)

ROCd = calculate_roc(ichscore$rev.ich, ichscore$outcome)
ggroc(ROCd)

3-4: 2 points
5-12: 1 point
13-15: 0 points
ICH volume

???30 cm3: 1 point
< 30 cm3: 0 points
IVH

Yes: 1 point
No: 0 points
Infratentorial origin of ICH

Yes: 1 point
No: 0 points
Age

Age 80 years or older: 1 point
Younger than 80 years: 0 points