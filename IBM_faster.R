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
popSize <- 5e3
# some shape parameter
mixKap <- 0.1 # 1e-16 ## verify kappa = 1 if here kappa ==0
mixScale <- 1 # think about whether this makes any sense
# mixing propensity
mixProp <- qgamma(p = ((1:popSize) - 1/2)/popSize, shape = 1/mixKap
                  , scale = mixScale*mixKap)
rateInds <-combn(1:popSize, 2)
dailyRate <- 3

rateFrame <- rateInds |>
  rbind(rateInds |>
          apply(MARGIN = 2, FUN = function(x){ mixProp[x[1]]*mixProp[x[2]]}) ) |>
  t() |>
  data.frame()
names(rateFrame)<- c("ind1", "ind2", "mixRate")


# rhexp <- function(n, probs, rates) {
#   x <- vapply(rates, function(lambda) {
#     rexp(n, lambda)
#   }, numeric(n))
#   i <- sample.int(length(probs), size = n, replace = TRUE, prob = probs)
#   x[cbind(seq_len(n), i)]
# }
#
#
# rhexp(1, rates = c(2/10, 4/10, 1/10, 3/10), probs = c(2/10, 4/10, 1/10, 3/10))

rateFrame <- rateFrame |>
  mutate(mixRate = mixRate * popSize/2 * dailyRate / sum(mixRate))
sum(rateFrame$mixRate)
# get event timings: here is when each individual contact occurs
# Initialization

tMax <- 2e3
toSim <- floor(1.1*dailyRate*popSize*tMax)
r2 <- runif(toSim)
# make and sort events
cs <- cumsum(rateFrame$mixRate)

contTime <- cumsum(rexp(toSim,sum(rateFrame$mixRate)))
# I feel
contInd <- sample( 1:length(rateFrame$mixRate)
                            , size = toSim
                            , replace = TRUE
                            , rateFrame$mixRate)


contactOrder <- cbind(t(rateInds)[ contInd,], contTime)

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

caseTally <- rep(0, popSize)

# Days
dayz <- 1
# totalContacts <- 0

I <- sum(states == Istate)
S <- sum(states == Sstate)

####
# Think about if this gets params as inputs too.
for(i in 1:length(contactOrder[,3])){
  co <- contactOrder[i,]
# a bunch of stuff I want to do for each row...
  # check if anyone is infectious
  tCur <- co[3]
  if(any(states[co[1:2]] == Istate)){
        # see if anyone has already recovered
    recVec <- (iTime[co[1:2]] + recDelay[co[1:2]]) <= tCur
    # count them
    # if(sum(recVec>0)){cat("recovery")}
    I <- I - sum(recVec)
    # remove them
    states[co[1:2]][recVec] <- Rstate
    # if one is infectious AND one is susceptible, lots to do
    if(sum(states[co[1:2]]) == (Istate+Sstate)){
      # first flip the coin
     if(rbinom(1,1, prob = tProb)){
       # cat("infection")
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

      cat(paste0("day ", dayz
                 , "; contact #", i
                 ,"; I = ", I
                 , ", S = " , S
                 , "; maxCases = ", max(caseTally)
                 , "\n")
    )
    dayz <- dayz + 1
    }
  # stop when no new infections
 if(S == 0 | I == 0){
   cat(paste0("END!  day ", dayz
              , "; contact # ", i
              ,",  I = ", I
              , ", S = " , S
              , ", maxCases = ", max(caseTally)
              , "\n")
 )
   break}
}


# check Kappa
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}
kd(caseTally[states != Sstate]) # slightly higher than 1 with this setup.







