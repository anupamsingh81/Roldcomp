library("R2jags")
lognormOR <- function(x,y) {

# Odds ratio from change
  
ORlow = 1- y
ORhigh =1- x

a= c(log(ORlow))
b= c(log(ORhigh))
CI = c(a,b)

# calculate mean and sd from 95% ci of logOR
LORlow = x - 1.96* y
LORhigh = x + 1.96*y
A =  matrix(data = c(1,-1.96,1,1.96),nrow =2,ncol=2,byrow =TRUE)
B = matrix (data = CI,nrow =2,ncol=1,byrow =FALSE)
round(solve(A,B),2)
G= round(solve(A,B),2)
H = c(G[1,])
I = c(1/G[2,]^2)
J = c(H,I)
return(J)
}
lognormOR(0,0.4)


# THE MODEL.
modelString = "
# JAGS model specification begins here...
model { 
logit(pi1) <- alpha + delta/2
logit(pi2) <- alpha - delta/2
r1 ~ dbin(pi1, n1)
r2 ~ dbin(pi2, n2)
alpha ~ dnorm(0, 0.0001)
#delta ~ dnorm(-0.255, 59.17) # Prior 1
delta ~ dnorm(0, 8.16) # Prior 2
# delta ~ dunif(-10, 10) # Prior 3

}
# ... end JAGS model specification
" # close quote for modelstring

# Write the modelString to a file, using R commands:
writeLines(modelString,con="model.txt")

# THE DATA.


dataList =  
  list(n1 = 366, n2 = 364, r1 = 134, r2 = 158)

# INTIALIZE THE CHAIN.

# Can be done automatically in jags.model() by commenting out inits argument.
# Otherwise could be established as:
#initsList = list(theta_1=0.8,theta_2=0.8)


# Specify the data in a form that is compatible with JAGS model, as a list:

model.fit <- jags(data=dataList, 
                   model.file="model.txt",
                   parameters.to.save=c("delta","alpha"),
                   n.chains= 3, 
                   n.iter=5000,
                   n.burnin=1000,
                   n.thin =1,
                   DIC=FALSE)
#Results

model.fit
plot(as.mcmc(model.fit))

exp(-1.522)
exp(-0.057)
exp(-0.588)
exp(-0.004)
exp(-0.286)
M1 = matrix(c(134,158,232,206),nrow=2)
library(epitools)
oddsratio(M1)