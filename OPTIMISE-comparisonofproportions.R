

# OPTIMISE trial in a Bayesian framework
# JAMA. 2014;311(21):2181-2190. doi:10.1001/jama.2014.5305
# Ewen Harrison
# 15/02/2015

# Primary outcome: composite of 30-day moderate or major complications and mortality
N1 <- 366
y1 <- 134
N2 <- 364
y2 <- 158
# N1 is total number in the Cardiac Output-Guided Hemodynamic Therapy Algorithm (intervention) group
# y1 is number with the outcome in the Cardiac Output-Guided Hemodynamic Therapy Algorithm (intervention) group
# N2 is total number in usual care (control) group
# y2 is number with the outcome in usual care (control) group

# Risk ratio
(y1/N1)/(y2/N2)

library(epitools)
riskratio(c(N1-y1, y1, N2-y2, y2), rev="rows", method="boot", replicates=100000)

# Using standard frequentist approach
# Risk ratio (bootstrapped 95% confidence intervals) = 0.84 (0.70-1.01) 
# p=0.07 (Fisher exact p-value)

# Reasonably reported as no difference between groups.

# But there is a difference, it just not judged significant using conventional
# (and much criticised) wisdom.

oddsratio(c(N1-y1, y1, N2-y2, y2), rev="rows")

Interventions <- c(158,134)
Patients <- c(364,366)
prop.test(Interventions,Patients)

# Bayesian Analysis Of Same Data

#------------------------------------------------------------------------------
source("~/DBDA2E-utilities.R")
source("~/plotPost.R")

require(rjags) # Kruschke, J. K. (2011). Doing Bayesian Data Analysis, Academic Press / Elsevier.
#------------------------------------------------------------------------------
# Important
# The model will be specified with completely uninformative prior distributions (beta(1,1,).
# This presupposes that no pre-exisiting knowledge exists as to whehther a difference
# may of may not exist between these two intervention. 

# Plot Beta(1,1)
# 3x1 plots
par(mfrow=c(3,1))
# Adjust size of prior plot
par(mar=c(5.1,7,4.1,7))
plot(seq(0, 1, length.out=100), dbeta(seq(0, 1, length.out=100), 1, 1), 
     type="l", xlab="Proportion",
     ylab="Probability", 
     main="OPTIMSE Composite Primary Outcome\nPrior distribution", 
     frame=FALSE, col="red", oma=c(6,6,6,6))
legend("topright", legend="beta(1,1)", lty=1, col="red", inset=0.05)

# THE MODEL.
modelString = "
# JAGS model specification begins here...
model {
# Likelihood. Each complication/death is Bernoulli. 
for ( i in 1 : N1 ) { y1[i] ~ dbern( theta1 ) }
for ( i in 1 : N2 ) { y2[i] ~ dbern( theta2 ) }
# Prior. Independent beta distributions.
theta1 ~ dbeta( 1 , 1 )
theta2 ~ dbeta( 1 , 1 )
}
# ... end JAGS model specification
" # close quote for modelstring

# Write the modelString to a file, using R commands:
writeLines(modelString,con="model.txt")


#------------------------------------------------------------------------------
# THE DATA.

# Specify the data in a form that is compatible with JAGS model, as a list:
dataList =	list(
  N1 = N1 ,
  y1 = c(rep(1, y1), rep(0, N1-y1)),
  N2 = N2 ,
  y2 = c(rep(1, y2), rep(0, N2-y2))
)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAIN.

# Can be done automatically in jags.model() by commenting out inits argument.
# Otherwise could be established as:
# initsList = list( theta1 = sum(dataList$y1)/length(dataList$y1) , 
#                   theta2 = sum(dataList$y2)/length(dataList$y2) )

#------------------------------------------------------------------------------
# RUN THE CHAINS.

parameters = c( "theta1" , "theta2" )     # The parameter(s) to be monitored.
adaptSteps = 500              # Number of steps to "tune" the samplers.
burnInSteps = 1000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=200000           # Total number of steps in chains to save.
thinSteps=1                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:
jagsModel = jags.model( "model.txt" , data=dataList , # inits=initsList , 
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nIter , thin=thinSteps )
# resulting codaSamples object has these indices: 
#   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]

#------------------------------------------------------------------------------
# EXAMINE THE RESULTS.

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
mcmcChain = as.matrix( codaSamples )

theta1Sample = mcmcChain[,"theta1"] # Put sampled values in a vector.
theta2Sample = mcmcChain[,"theta2"] # Put sampled values in a vector.

# Plot the chains (trajectory of the last 500 sampled values).
par( pty="s" )
chainlength=NROW(mcmcChain)
plot( theta1Sample[(chainlength-500):chainlength] ,
      theta2Sample[(chainlength-500):chainlength] , type = "o" ,
      xlim = c(0,1) , xlab = bquote(theta[1]) , ylim = c(0,1) ,
      ylab = bquote(theta[2]) , main="JAGS Result" , col="skyblue" )

# Display means in plot.
theta1mean = mean(theta1Sample)
theta2mean = mean(theta2Sample)
if (theta1mean > .5) { xpos = 0.0 ; xadj = 0.0
} else { xpos = 1.0 ; xadj = 1.0 }
if (theta2mean > .5) { ypos = 0.0 ; yadj = 0.0
} else { ypos = 1.0 ; yadj = 1.0 }
text( xpos , ypos ,
      bquote(
        "M=" * .(signif(theta1mean,3)) * "," * .(signif(theta2mean,3))
      ) ,adj=c(xadj,yadj) ,cex=1.5  )

# Plot a histogram of the posterior differences of theta values.
thetaRR = theta1Sample / theta2Sample # Relative risk
thetaDiff = theta1Sample - theta2Sample # Absolute risk difference
thetaOR = (1-theta2Sample)/(1-theta1Sample) * theta1Sample / theta2Sample  # Odds ratio
NNT = 1/thetaDiff # Numbers Needed to Treat

par(mar=c(5.1, 4.1, 4.1, 2.1))
plotPost( thetaRR , xlab= expression(paste("Relative risk (", theta[1]/theta[2], ")")) , 
          compVal=1.0, ROPE=c(0.9, 1.1),
          main="OPTIMSE Composite Primary Outcome\nPosterior distribution of relative risk")
plotPost( thetaDiff , xlab=expression(paste("Absolute risk difference (", theta[1]-theta[2], ")")) ,
          compVal=0.0, ROPE=c(-0.05, 0.05),
          main="OPTIMSE Composite Primary Outcome\nPosterior distribution of absolute risk difference")
plotPost( thetaOR , xlab= expression(paste("Odds ratio (", OR, ")")) , 
          compVal=1.0, ROPE=c(0.8, 1.2),
          main="OPTIMSE Composite Primary Outcome\nPosterior distribution of Odds Ratio")
plotPost( NNT , xlab=expression(paste("NNT (", theta, ")")) ,
          compVal= 10, ROPE=c(30, 50),
          main="OPTIMSE Composite Primary Outcome\nPosterior distribution of NNT")

#-----------------------------------------------------------------------------
# Use posterior prediction to determine proportion of cases in which 
# using the intervention would result in no complication/death 
# while not using the intervention would result in complication death 

chainLength = length( theta1Sample )

# Create matrix to hold results of simulated patients:
yPred = matrix( NA , nrow=2 , ncol=chainLength ) 

# For each step in chain, use posterior prediction to determine outcome
for ( stepIdx in 1:chainLength ) { # step through the chain
  # Probability for complication/death for each "patient" in intervention group:
  pDeath1 = theta1Sample[stepIdx]
  # Simulated outcome for each intervention "patient"
  yPred[1,stepIdx] = sample( x=c(0,1), prob=c(1-pDeath1,pDeath1), size=1 )
  # Probability for complication/death for each "patient" in control group:
  pDeath2 = theta2Sample[stepIdx]
  # Simulated outcome for each control "patient"
  yPred[2,stepIdx] = sample( x=c(0,1), prob=c(1-pDeath2,pDeath2), size=1 )
}

# Now determine the proportion of times that the intervention group has no complication/death
# (y1 == 0) and the control group does have a complication or death (y2 == 1))
(pY1eq0andY2eq1 = sum( yPred[1,]==0 & yPred[2,]==1 ) / chainLength)
(pY1eq1andY2eq0 = sum( yPred[1,]==1 & yPred[2,]==0 ) / chainLength)
(pY1eq0andY2eq0 = sum( yPred[1,]==0 & yPred[2,]==0 ) / chainLength)
(pY10eq1andY2eq1 = sum( yPred[1,]==1 & yPred[2,]==1 ) / chainLength)

# Conclusion: in 27% of cases based on these probabilities,
# a patient in the intervention group would not have a complication,
# when a patient in control group did. 