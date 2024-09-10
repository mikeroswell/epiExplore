# script to begin dreaming simulation up

# random note but I'm committed to using the native R pipe for the first time

# load some libraries
# library(GillespieSSA2)
library(dplyr)
# library(purrr)
library(tidyr)
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
popSize <- 5 #2e3
# some shape parameter
mixKap <- 2.2
mixMean <- 3 # I think we can set the mean per capita mix rate here
# mixing propensity
mixProp <- qgamma(p = ((1:popSize) - 1/2)/popSize, shape = 1/mixKap
                  , scale = mixMean*mixKap)
rateInds <-combn(1:popSize, 2) 
rateFrame <- rateInds |> 
  rbind(rateInds |> 
          apply(MARGIN = 2, FUN = function(x){ mixProp[x[1]]*mixProp[x[2]]}) ) |> 
  t() |> 
  data.frame() 
names(rateFrame)<- c("ind1", "ind2", "mixRate")

# get event timings: here is when each individual contact occurs
# Initialization
tCur <- 0 
i <- 0 # meaningless counter
tMax <- 2e3
Who <- rep(0L, popSize*mixMean*tMax)
When <- rep(0, popSize*mixMean*tMax)
# make and sort events


contactInds <- while(tCur < tMax){
  i <- i + 1
  ev <- rexp(1:length(rateFrame$mixRate), rate = rateFrame$mixRate) 
  tE <- min(ev)
  tCur <- tE +tCur
  When[i] <- tCur
  Who[i] <- which.min(ev)
}

When <- When[ When> 0]
Who <- Who[When > 0]
contactOrder <- cbind(t(rateInds)[ Who,], When)

# We will also pre-compute recovery times
# First lets assume it is exponential to compare to basic model
setGamma <- 0.1 # 
recDelay <- rexp(1:popSize, rate = setGamma)
iTime <- rep(0, popSize)
# first, check with SIR assumption
# mat <- outer(rep(1, popSize), rep(1, popSize))
# # seems good and kappa for secondary cases is nearly 1
# contactProb <- 2*mat/sum(mat)
# contactProb[lower.tri(contactProb, diag = TRUE)] <- 0 

# epidemic parameters



tProb <- 0.1 # transmission event in 10% of effective contacts
setBeta <- mixMean*tProb



# we're getting towards the simulation now
Sstate <- 1 
Istate <- 2
Rstate <- 3

states <- rep(Sstate, popSize)
# initialize with one random infection
p0 <- sample(1:popSize, 1)
states[p0]<- Istate
tCur <- 0 
iTime[p0] <- tCur

# keep track of cases per person

caseTally <- rep(0, popSize)

# steps 
i <- 0
totalContacts <- 0

I <- rep(0L, popSize)


#### 
# a bunch of stuff I want to do for each row...
  # check if anyone is infectious
  if(any(states[contactOrder[,1:2]]) == Istate){
        # see if anyone has already recovered
    if(iTime + )
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







