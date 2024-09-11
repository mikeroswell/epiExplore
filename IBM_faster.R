# script to begin dreaming simulation up

# random note but I'm committed to using the native R pipe for the first time

# load some libraries
# library(GillespieSSA2)
library(dplyr)
# library(purrr)
library(tidyr)
library(ggplot2)
# library(patchwork) # for assembling ggplot objects

# the idea here is that individuals will differ in their mixing propensity.
# for now I will not include preferential mixing
# (question: what is the bivariate gamma?)
# we will consider recovery time to be an (unrealistic but broad) exponential
# distribution, like in a basic SIR model
# we will also assume that transmission probability, given contact, is constant
# during the infectious stage and equal for all individuals
# we will assume that recovered individuals cannot be reinfected.
set.seed(4228)

# first, play with this mixing idea
popSize <- 1e4
# some shape parameter
mixKap <- 1e-16 ## verify kappa = 1 if here kappa ==0
# mixKap <- 0.1 #
mixScale <- 1 # think about whether this makes any sense
# mixing propensity
mixProp <- qgamma(p = ((1:popSize) - 1/2)/popSize, shape = 1/mixKap
                  , scale = mixScale*mixKap)

# combn is very slow with big numbers
# looks good https://stackoverflow.com/a/49153855/8400969
comb2.int <- function(n, rep = FALSE){
  if(!rep){
    # e.g. n=3 => (1,2), (1,3), (2,3)
    x <- rep(1:n,(n:1)-1)
    i <- seq_along(x)+1
    o <- c(0,cumsum((n-2):1))
    y <- i-o[x]
  }else{
    # e.g. n=3 => (1,1), (1,2), (1,3), (2,2), (2,3), (3,3)
    x <- rep(1:n,n:1)
    i <- seq_along(x)
    o <- c(0,cumsum(n:2))
    y <- i-o[x]+x-1
  }
  return(rbind(x,y))
}
rateInds <-comb2.int(popSize)

# daily per-person interactions
dailyRate <- 5

# generate un-scaled interaction rates by dyad
rateFrame <- rateInds |>
  rbind(rateInds |>
          apply(MARGIN = 2, FUN = function(x){ mixProp[x[1]]*mixProp[x[2]]}) ) |>
  t() |>
  data.frame()
names(rateFrame)<- c("ind1", "ind2", "mixRate")


# rescale to match daily rate
rateFrame <- rateFrame |>
  mutate(mixRate = mixRate * popSize/2 * dailyRate / sum(mixRate))
sum(rateFrame$mixRate)

# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3

# This is 10% more than the expected number of contacts up to tMax
toSim <- floor(0.55*dailyRate*popSize*tMax)

# make events
contTime <- cumsum(rexp(toSim,sum(rateFrame$mixRate)))
# link to contact matrix
contInd <- sample( 1:length(rateFrame$mixRate)
                            , size = toSim
                            , replace = TRUE
                            , rateFrame$mixRate)

# save as a data.frame (will be longer than necessary)
contactOrder <- cbind(t(rateInds)[ contInd,], contTime)

# We will also pre-compute recovery times
# First lets assume it is exponential to compare to basic model
setGamma <- 0.1 #
recDelay <- rexp(1:popSize, rate = setGamma)
# initialize infection time with a large number for logical testing
iTime <- rep(tMax, popSize)

# epidemic parameters
tProb <- 0.075 # transmission event in 10% of effective contacts
setBeta <- dailyRate*tProb

# map integers to infection status
Sstate <- 1
Istate <- 2
Rstate <- 3

states <- rep(Sstate, popSize)
# initialize with one random infection
p0 <- sample(1:popSize, 1)
states[p0]<- Istate

# start the clock
tCur <- 0
iTime[p0] <- tCur

# keep track of cases per person and counts for each state
caseTally <- rep(0, popSize)

I <- sum(states == Istate)
S <- sum(states == Sstate)

# and Days
dayz <- 1


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
# is this the way to think about kappa? or do we only want to focus on recovered individuals during the epidemic? What is kappa_effective?


ktdt <- function(startT, deltaT){
  who <- which(startT <= iTime & iTime < startT+deltaT)
  n <- sum(startT <= iTime & iTime < startT+deltaT)
  secDist <- caseTally[who]
  kappa_discrete = kd(secDist)
  mu <- mean(secDist)
  sig <- sd(secDist)
  kappa_naive = sig^2/mu^2
  return(data.frame(n, kappa_discrete, kappa_naive, mu, sig, startT))
}

# hist(iTime[iTime< tMax])
# plot(sapply(seq(0, 40, 1), function(st){
#   ktdt(st, 10)
# }))

keff <- purrr::map_dfr(0:33, function(d){
  ktdt(d, 6)
})


keff |>
  pivot_longer(cols = c("kappa_discrete", "kappa_naive"), names_to = "kappa_approximation", values_to = "Kappa") |>
  ggplot(aes(startT, Kappa, color = kappa_approximation)) +
  geom_point() +
  geom_hline(yintercept = 1, color = "blue") +
  scale_y_log10()+
  theme_classic() +
  labs(x = "day", y = "kappa")

#confirm kappa
kd(caseTally[states != Sstate])

