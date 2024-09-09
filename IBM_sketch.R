# script to begin dreaming simulation up

# random note but I'm committed to using the native R pipe for the first time

# load some libraries
# library(GillespieSSA2)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(ggplot2)
# library(patchwork) # for assembling ggplot objects

# the idea here is that individuals will differ in their mixing propensity.
# for now I will not include preferential mixing
# (question: what is the bivariate gamma?)
# we will consider recovery time to be an (unrealistic but broad) exponential
# distribution, like in a basic SIR model
# we will also assume that transmission probability, given contact, is constant
# during the infectious stage and equal for all individuals
# we will assume that recovered individuals cannot be reinfected.

# first, play with this mixing idea
popSize <- 2e3
# some shape parameter, assume scale = 1 for now
mixShape <- 2.2
# mixing propensity
mixProp <- qgamma(p = ((1:popSize) - 1/2)/popSize, shape = 2.2)

# epidemic parameters
setBeta <- 0.2 # this may be approximate, hack to estimate r0
setGamma <- 0.1 # this is a recovery rate

tProb <- 0.1 # transmission event in 10% of effective contacts
cProb <- setBeta/tProb # mean daily per capita contacts
# interesting contacts
mat <- outer(mixProp, mixProp)
# first, check with SIR assumption
# mat <- outer(rep(1, popSize), rep(1, popSize))
# seems good and kappa for secondary cases is nearly 1
contactProb <- mat/sum(mat)*2
contactProb[lower.tri(contactProb, diag = TRUE)] <- 0

# Initialization
tCur <- 0 # set time t0
tMax <- 2e3
# let states 1 = susceptible, 2 = infectious, 3 = removed
states <- rep(1, popSize)
# initialize with one random infection
states[sample(1:popSize, 1)]<- 2


# keep track of cases per person

caseTally <- rep(0, popSize)
# steps 
i <- 0

totalContacts <- 0

# Not really initialization, but pre-computing to avoid loading up the loop
contactsToSim <- cProb * popSize * tMax

contactOrder <- sample(1:length(contactProb[upper.tri(contactProb)])
                                   , size = contactsToSim
                                   , prob = contactProb[upper.tri(contactProb)]
                       , replace = TRUE
)
coms <- combn(1:popSize,2)

while(tCur < tMax){
  # currently infected
  I <- sum(states == 2)
  # generate an event
  i <- i +1
  # is it a contact or a recovery?
  waitContact <- min(rexp(popSize, cProb)) # check not off by a factor of two or 
  # need to take a geometric mean
  waitRecovery <- min(rexp(I,  setGamma))
  # if it is a contact, do we get a new infection?
  if(waitContact< waitRecovery){
    totalContacts <- totalContacts + 1
    tCur <- tCur + waitContact
    myContact <- coms[, contactOrder[totalContacts]]


      if(sum(states[c(myContact)]) == 3){
        newI <- rbinom(1,1,  prob = tProb)
    caseTally[myContact[states[c(myContact)] == 2]] <- caseTally[myContact[states[c(myContact)] == 2]] +
      newI
    if(newI ==1){states[c(myContact)] <- 2}
    }
  }
# if not, then somebody recovers
  else{
  tCur <- tCur +  waitRecovery
  states[sample((1:popSize)[states ==2], 1)] <- 3
  }
# would like to print this (or something) every large jump instead of every event, don't know how
  cat(paste0("t = ", tCur, ",  I = ", I, ", maxCases = "
      , max(caseTally)
      , ", per capita contacts = "
      , totalContacts/(popSize * tCur)
      , "\n")
  )
  # make sure to stop when no new infections
  if(sum(states == 1)==0| sum(states ==2)==0){break}
}


# check Kappa
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}
kd(caseTally[states == 3]) # slightly higher than 1 with this setup. 







