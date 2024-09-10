# script to begin dreaming simulation up

# random note but I'm committed to using the native R pipe for the first time

# load some libraries
# library(GillespieSSA2)
library(dplyr)
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
popSize <- 2e2
# some shape parameter
mixKap <- 2.2
mixScale <- 1 # Too hard to think about this so leave at default to rescale later
# mixing propensity
mixProp <- qgamma(p = ((1:popSize) - 1/2)/popSize, shape = 1/mixKap
                  , scale = mixScale)
rateInds <-combn(1:popSize, 2)
dailyRate <- 3

rateFrame <- rateInds |>
  rbind(rateInds |>
          apply(MARGIN = 2, FUN = function(x){ mixProp[x[1]]*mixProp[x[2]]}) ) |>
  t() |>
  data.frame()
names(rateFrame)<- c("ind1", "ind2", "mixRate")
rateFrame <- rateFrame |>
  mutate(mixRate = dailyRate * popSize * mixRate / sum(mixRate))

# get event timings: here is when each individual contact occurs
# Initialization
tCur <- 0
i <- 0 # meaningless counter
tMax <- 2e2
Who <- rep(0L, popSize*mixMean*tMax)
When <- rep(0, popSize*mixMean*tMax)
# make and sort events


# I think I can do this smarter

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
iTime <- rep(tMax, popSize)
# first, check with SIR assumption
# mat <- outer(rep(1, popSize), rep(1, popSize))
# # seems good and kappa for secondary cases is nearly 1
# contactProb <- 2*mat/sum(mat)
# contactProb[lower.tri(contactProb, diag = TRUE)] <- 0

# epidemic parameters



tProb <- 0.1 # transmission event in 10% of effective contacts
setBeta <- dailyRate*tProb



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

caseTally <- rep(0L, popSize)

# Days
dayz <- 0L
# totalContacts <- 0

I <- sum(states == Istate)
S <- sum(states == Sstate)

####
# Think about if this gets params as inputs too.
atEachStep <- function(co){
# a bunch of stuff I want to do for each row...
  # check if anyone is infectious
  tCur <- co[3]
  if(any(states[co[1:2]] == Istate)){
        # see if anyone has already recovered
    recVec <- (iTime[co[1:2]] + recDelay[co[1:2]]) <= tCur
    # count them
    I <- I - sum(recVec)
    # remove them
    states[co[1:2]][recVec] <- Rstate
    # if one is infectious AND one is susceptible, lots to do
    if(sum(states[co[1:2]]) == Istate+Sstate){
      # first flip the coin
     if(rbinom(1,1, prob = tProb)){
       # update cumulative state counters
        I <- I + 1
        S <- S - 1
        # count the win
        caseTally[co[1:2][states[co[1:2]] == 2]] <- caseTally[co[1:2][states[co[1:2]] == 2]] + 1
        # record the infection time
        iTime[co[1:2][states[co[1:2]] == 1]] <- tCur
        # update the states
        states[co[1:2][states[co[1:2]] == 1]] <- Istate

    }
  }
}
# [save state? and] print some stuff once per day
  if(floor(tCur>dayz)){
      dayz <- dayz + 1
      cat(paste0("t = ", tCur, ",  I = ", I, ", S = ", S, ", maxCases = "
      , max(caseTally)
      , "\n")
    )
    }
  # stop when no new infections
 # if(S == 0 | I == 0){stop}
}

apply(contactOrder, 1, atEachStep)

# check Kappa
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}
kd(caseTally[states == 3]) # slightly higher than 1 with this setup.







