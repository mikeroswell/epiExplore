# script to begin dreaming simulation up

# random note but I'm committed to using the native R pipe for the first time

# load some libraries
library(shellpipes)
rpcall("IBM.base.Rout IBM_minimal.R IBM_base_pars.rda")
rpcall("IBM.lowGamma.Rout IBM_minimal.R IBM_lowGamma_pars.rda")
rpcall("IBM.highGamma.Rout IBM_minimal.R IBM_highGamma_pars.rda")
rpcall("highGamma.IBM.Rout IBM_minimal.R IBM_highGamma_pars.rda")
rpcall("lowGamma.IBM.Rout IBM_minimal.R IBM_lowGamma_pars.rda")
rpcall("gamma3.IBM.Rout IBM_minimal.R recFun.rda IBM_gamma3_pars.rda")
rpcall("const.IBM.Rout IBM_minimal.R recFun.rda IBM_const_pars.rda")
rpcall("highR.IBM.Rout IBM_minimal.R recFun.rda IBM_highR_pars.rda")
rpcall("base.IBM.Rout IBM_minimal.R recFun.rda IBM_base_pars.rda")
rpcall("change_6.IBM.Rout IBM_minimal.R recFun.rda IBM_change_6_pars.rda")
rpcall("change_1p5.IBM.Rout IBM_minimal.R recFun.rda IBM_change_1p5_pars.rda")
rpcall("change_3.IBM.Rout IBM_minimal.R recFun.rda IBM_change_3_pars.rda")
rpcall("change_12.IBM.Rout IBM_minimal.R recFun.rda IBM_change_12_pars.rda")
rpcall("change_2.IBM.Rout IBM_minimal.R recFun.rda IBM_change_2_pars.rda")
manageConflicts()

library(dplyr)
# library(purrr)
library(tidyr)
loadEnvironments()

#This is a script to streamline the IBM script for single-class SIR models.
set.seed(seed)

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
dailyRate <- setBeta/tProb

# generate interaction rates by dyad
rateFrame <- rateInds |>
  rbind(dailyRate/(popSize-1)) |>
  t() |>
  data.frame()
names(rateFrame)<- c("ind1", "ind2", "mixRate")

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
# Default assumes an exponential duration distribution

recDelay <- recFun(1:popSize, rPar = rPar)
# initialize infection time with a large number for logical testing
iTime <- rep(tMax, popSize)

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

for(i in 1:nrow(contactOrder)){
  co <- contactOrder[i,]
# a bunch of stuff I want to do for each row...
  # check if anyone is infectious
  tCur <- co[3]
  cont <- states[co[1:2]] == Istate
  if(any(cont)){
        # see if anyone has already recovered
    recVec <- (iTime[co[1:2]] + recDelay[co[1:2]]) <= tCur
    # count them
    # if(sum(recVec>0)){cat("recovery")}
    I <- I - sum((cont) * (recVec))
    # remove them (this will needlessly update state from R to R sometimes)
    states[co[1:2]][recVec] <- Rstate
    # if one is infectious AND one is susceptible, lots to do
    if(any(states[co[1:2]] == Istate) & any(states[co[1:2]] == Sstate)){
      # first flip the coin
	  ## set tProb to 1 for now; more efficient and doesn't matter in this version
     if(rbinom(1,1, prob = tProb)){
       # cat("infection")
       # update cumulative state counters
        I <- I + 1
        S <- S - 1
        # count the win
        caseTally[co[1:2][states[co[1:2]] == Istate]] <- caseTally[co[1:2][states[co[1:2]] == Istate]] + 1

        # record the infection time
        iTime[co[1:2][states[co[1:2]] == Sstate]] <- tCur
        # update the states
        states[co[1:2][states[co[1:2]] == Sstate]] <- Istate
    }
  }
}
# [save state? and] print some stuff once per day
  if(floor(tCur)>dayz){

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
# questionable move to save storage
rm(list= c("rateFrame", "rateInds", "contactOrder", "contInd", "contTime" ))

saveEnvironment()
