x <- c(4, TRUE)
class(x)
x <- list(2, "a", "b", TRUE)
x[[1]]
p <- 1:4
y <- 2:3
p+y
z <- c(3, 5, 1, 10, 12, 6)
z[z <= 5] <- 0
z
head(ozone)
str(ozone)
ozone[c(152,153),]
nrow(ozone)

ozone[47,]
is.na(ozone$Ozone)
targetColumn <- ozone$ozone
length(ozone$ozone[is.na(ozone$ozone)])
sapply(ozone, function(x) sum(is.na(x)))
a <- ozone$Ozone
complete.cases(a)
z <- complete.cases(a)
t <- is.na(ozone$Ozone)
t
k <-ozone$Ozone[!t]
summary(k)
newerdata <- ozone[ which(ozone$Ozone > 31
                         & ozone$Temp > 90), ]
summary(newerdata$Solar.R)
newestdata <- ozone[which(ozone$Month = 6)]
t.test(ozone$Temp~)
library("plyr")
ddply(ozone,~Month,summarise,mean=mean(ozone$Temp),sd=sd(ozone$Temp))
split(ozone,ozone$Month)
v <- $'6'
library("dplyr")
grp <- group_by( ozone$Month)