cat("model{
    for( i in 1 : N ) {
    r[i] ~ dbin(p[i],n[i])    #Likelihood
    logit(p[i]) <- b[i]       #Log-odds of mortality
    b[i] ~ dnorm(mu,prec)     #Random effects model for log-odds mortality  
    }
    
    mu ~ dnorm(0.0,1.0E-6)    #Priors
    sd <- 1 / sqrt(prec)
    prec ~ dgamma(0.001,0.001)       
    }", file="bristol.txt")



bristolDat<-list(N=12,r=c(25, 24, 23, 25, 42,24,53,26,25,58,31,41),
                 n=c(187,323,122,164,405,239,482,195,177,581,301,143))


bristolInits<-function(){list(b = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1), 
                              prec = 2, mu = -1)}

bristolParams<-c("p", "mu", "sd")

library(R2jags)

jag.bristol<-jags(data=bristolDat, 
                  inits=bristolInits, 
                  parameters.to.save=bristolParams, n.iter=1000, n.thin=20,
                  model.file="bristol.txt")

jag.bristol

plot(as.mcmc(jag.bristol))
plot(jag.bristol)
install.packages("mcmcplots")
library(mcmcplots)
caterplot(as.mcmc(jag.bristol), parms="p")