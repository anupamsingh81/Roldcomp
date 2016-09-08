


cat("model {
                         # Likelihood
for (j in 1:Nstud) {
    P[j] <- 1/V[j]      # Calculate precision
    Y[j] ~ dnorm(delta[j],P[j])
    delta[j] ~ dnorm(d,prec)
    }
    
    # Priors
    d ~ dnorm(0,1.0E-6)
    prec <- 1/tau.sq
    tau.sq <- tau*tau   # tau.sq = between-study variance
    tau ~ dunif(0,10)   # Uniform on SD
    OR<-exp(d)          #exponentiate to get back to OR scale
    prob.OR1<-step(d)   #calc prob of OR > 1
    }", file="betaMeta1.txt")

blocker <- read.csv("~/BLOCKER.csv",sep=",", header=T)
Nstud<-22

blockerDat <- list(Y=blocker$Y,V=blocker$V,Nstud=Nstud)
blockerParams <- c("d","tau","OR", "prob.OR1")
blockerInits<-function(){
  list("delta"=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),"d"=c(0),
       "tau"=c(1))
}
set.seed(100)
library(R2jags)

jag.blocker1<-jags(data=blockerDat, 
                   inits=blockerInits, 
                   parameters.to.save=blockerParams, n.iter=1000, n.thin=20,
                   model.file="betaMeta1.txt")

jag.blocker1