library(shellpipes)
manageConflicts()
library(dplyr)
library(tidyr)

loadEnvironments()

#This is a script to streamline the IBM script for single-class SIR models.
set.seed(seed)

Sstate <- 1
Istate <- 2
Rstate <- 3

contactList <- function(pop, beta, timeSpan, t0, q){
	eventRate <- pop*beta
	eventNumber <- qpois(q, lambda=timeSpan*eventRate)
	primary <- sample(1:popSize, eventNumber, replace=TRUE)
	offset <- sample(1:(popSize-1), eventNumber, replace=TRUE)
	secondary <- 1 + (primary+offset-1) %% popSize
	delay <- rexp(eventNumber, rate=eventRate)
	eTime <- t0 + cumsum(delay)
	stopifnot(max(eTime)>timeSpan) ## This should happen 1/q of the time; increase q or change seed
	## stopifnot(sum(primary==secondary)==0) ## This should not happen
	return(data.frame(
		primary, secondary, eTime
	))
}

## contactList(popSize, beta*popSize, 1, 0, 0.999)

initPop <- function(S, I, R){
	
}

quit()

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
# First lets assume it is exponential to compare to basic model

recDelay <- rexp(1:popSize, rate = setGamma)
# initialize infection time with a large number for logical testing
iTime <- rep(tMax, popSize)

# map integers to infection status

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
  if(any(states[co[1:2]] == Istate)){
        # see if anyone has already recovered
    recVec <- (iTime[co[1:2]] + recDelay[co[1:2]]) <= tCur
    # count them
    # if(sum(recVec>0)){cat("recovery")}
    I <- I - sum(recVec)
    # remove them
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
# questionable move to save storage
rm(list= c("rateFrame", "rateInds", "contactOrder", "contInd", "contTime" ))

saveEnvironment()

