# script to begin dreaming simulation up


# load some libraries
library(shellpipes)
rpcall("IBM_for_v1.Rout IBM_for_v1.R recFun.rda")
manageConflicts()
library(dplyr)
library(pracma)
# library(purrr)
library(tidyr)
loadEnvironments()

IBM_v1<- function(setBeta = 1.5, seed = 10, popSize = 1e4, tProb = 1
                  ,tMax = 2e3){
#This script is based on IMB_minimal.R
set.seed(seed)
results <- vector("list", 50)
thr_counter <-1
threshold <- seq(0.1,1,by=0.1)
results_infected <- vector("list", length(threshold))
log_thr_counter <-1
log_threshold <- logspace(-4,0,5)
results_log_infected <- vector("list", length(log_threshold))
Ifinal<-popSize*finalSize(setBeta)
flag <- 1 #flag is one if the total infected individuals is less than Ifinal
log_flag <-1
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
# half Days
halfDayz <- 1
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
  if(floor(tCur)>=dayz){
    dayz <- dayz + 1
  }
  if((sum(states!=Sstate)>=Ifinal*threshold[thr_counter] & flag ==1 )| 
     (thr_counter == length(threshold) & sum(states!=Sstate) > Ifinal*0.99 & flag ==1 )){
    df_I <- as.data.frame(table(caseTally[states !=Sstate]))  
    names(df_I) <- c("num_cases", "count")
    df_I$threshold <- threshold[thr_counter]
    df_I$day <- dayz
    df_I$type <- "proportion"
    df_I$beta <- setBeta
    df_I$cmpt <-halfDayz
    results_infected[[thr_counter]] <- df_I
    flag<- as.integer(thr_counter < length(threshold))
    thr_counter <- thr_counter + flag
  }
  if((sum(states!=Sstate)>=Ifinal*log_threshold[log_thr_counter] & log_flag ==1 )| 
     (log_thr_counter == length(log_threshold) & sum(states!=Sstate) > 
      Ifinal*0.99 & log_flag ==1 )){
    df_logI <- as.data.frame(table(caseTally[states !=Sstate]))  
    names(df_logI) <- c("num_cases", "count")
    df_logI$threshold <- log_threshold[log_thr_counter]
    df_logI$day <- dayz
    df_logI$type <- "logproportion"
    df_logI$beta <- setBeta
    df_logI$cmpt <-halfDayz
    results_log_infected[[log_thr_counter]] <- df_logI
    log_flag<- as.integer(log_thr_counter < length(log_threshold))
    log_thr_counter <- log_thr_counter + log_flag
  }
if(tCur>=halfDayz/2){
  df <- as.data.frame(table(caseTally[states !=Sstate]))  
  names(df) <- c("num_cases", "count")
  df$threshold <- halfDayz
  df$day <- dayz
  df$type <- "half-day"
  df$beta <- setBeta
  df$cmpt <- sum(states!=Sstate)/Ifinal
  results[[halfDayz]] <- df
  halfDayz <- halfDayz + 1
}
 if(S == 0 | I == 0){
   cat(paste0("END!  day ", dayz
              , "; contact # ", i
              ,",  I = ", I
              , ", S = " , S
              , ", maxCases = ", max(caseTally)
              , ", beta = ", setBeta
              , "\n")
 )
   finalDay <- data.frame(Values = c(dayz, halfDayz, sum(states!=Sstate)/Ifinal,
                                     setBeta),
                          Variables = c("day", "half-days",
                                        "proportionInfected",
                                        "beta"))
   
   break}
}
case_per_case_overall <- do.call(rbind, results)
case_per_case_over_infected <- do.call(rbind, results_infected)
case_per_case_over_log_infected <- do.call(rbind, results_log_infected)
# questionable move to save storage
rm(list= c("rateFrame", "rateInds", "contactOrder", "contInd", "contTime" ))

write.table(finalDay, file = paste0(setBeta,"finalDay.txt"), sep = "\t", 
            row.names = FALSE)
return(list(setBeta = setBeta, overTime = case_per_case_overall,
            overEpiState = case_per_case_over_infected,
            overEpiStatelog = case_per_case_over_log_infected))
}


saveEnvironment()