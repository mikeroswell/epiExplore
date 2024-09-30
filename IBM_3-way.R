# script to begin dreaming simulation up

library(shellpipes)
manageConflicts()
startGraphics()
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

# note dependency on params and functions in kappas_in_3-class.R
loadEnvironments()



# the idea here is that individuals will differ in their mixing propensity.
# for now I will not include preferential mixing
# (question: what is the bivariate gamma?)
# we will consider recovery time to be an (unrealistic but broad) exponential
# distribution, like in a basic SIR model
# we will also assume that transmission probability, given contact, is constant
# during the infectious stage and equal for all individuals
# we will assume that recovered individuals cannot be reinfected.
# set.seed(4228)

# Initialization
tMax <- 2e2 # measured in days, assume for most things I'll do 1 year is plenty
popSize <- 1e4

R_0 <- 4 # much higher than in current fig 1 but maybe necessarily so to enable
# outbreak with high probability
# generate ODE model parameters or something (shd be SIR)
mod <- cmptMod(0.2, xChoice = "low", R_0 = R_0, scaleRNum = 1) #scaleRNum[2])
# First let's assume recovery times are exponentially distributed,
# to compare to basic model
setGamma <- 1/3 #


# epidemic parameters
meanBeta <- R_0*setGamma

# daily per-person interactions
dailyRate <- 1.6

# probability transmission occurs, given contact between susceptible, infectious
# (may have several probability values if several classes)

tProb <-mod$rNums*setGamma/dailyRate #
if(any(tProb >1)){stop("One or more transmission probabilities > 1, ", tProb)}
if(any(tProb ==1)){stop("One or more transmission probabilities = 1, ", tProb)}
if(any(tProb < 0.3)){stop("some transmission probabilities low -- are you wasting time? ", tProb)}
print(paste("transmission probability = ", tProb))




# generate the contact model
# some shape parameter
mixKap <- 1e-16 ## verify kappa = 1 if here kappa ==0
# mixKap <- 0.1 # I think this should give overall kappa close to 1.2, but with big dynamics in kappa(T, deltaT)

# mixKap <- 1 # based on one run, this makes for kappa(0,Inf) of 2, with high spread in kappa(T, 6)
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
recDelay <- rexp(1:popSize, rate = setGamma)
# initialize infection time with a large number for logical testing
iTime <- rep(tMax, popSize)



# map integers to infection status
Sstate <- 1
Istate <- c(2,3,4)
Rstate <- 5

states <- rep(Sstate, popSize)
# initialize with one random infection
p0 <- sample(1:popSize, 1)
states[p0]<- sample(Istate, 1, prob = mod$fracs)

# start the clock
tCur <- 0
iTime[p0] <- tCur

# keep track of cases per person and counts for each state
caseTally <- rep(0, popSize)

I <- sum(states %in% Istate)
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
  pair <- co[1:2]
  estat <- states[pair]
  if(any(estat %in% Istate)){
        # see if anyone has already recovered
    recVec <- (iTime[pair] + recDelay[pair]) <= tCur
    # count them
    # if(sum(recVec>0)){cat("recovery")}
    I <- I - sum(recVec)
    # remove them
    estat[recVec] <- Rstate
    # if one is infectious AND one is susceptible, lots to do
    if(any(estat== Sstate) & any(estat %in% Istate)){
      # first flip the coin on an infection
     if(rbinom(1,1, prob = tProb[estat[estat != 1] - 1])){
       # cat("infection")
       # update cumulative state counters
        I <- I + 1
        S <- S - 1
        # count the win
        caseTally[pair[estat %in% Istate]] <- caseTally[pair[estat %in% Istate]] + 1

        # record the infection time
        iTime[pair[estat == Sstate]] <- tCur
        # get the right infectious state
        states[pair[estat == Sstate]] <-
          sample(Istate, 1, prob = mod$fracs)

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
  V <- var(secDist)
  kappa_naive = V/mu^2
  return(data.frame(n, kappa_discrete, kappa_naive, mu, V, startT))
}

# hist(iTime[iTime< tMax])
# plot(sapply(seq(0, 40, 1), function(st){
#   ktdt(st, 10)cat
# }))

keff <- purrr::map_dfr(0:33, function(d){
  ktdt(d, 6)
})

# compute the denoised estimator with loess
loessMu <- loess(mu~startT, data = keff)
loessV <- loess(V ~ startT, data  = keff )

keff |> mutate(lmu = predict(loessMu, startT)
               , lV = predict(loessV, startT)
               , kappa_loess = (lV-lmu)/lmu^2
               )
# pdf("kappa_in_intermediate_ratio.pdf")
keff |>
  pivot_longer(cols = c("kappa_discrete", "kappa_naive", "kappa_loess"), names_to = "kappa_approximation", values_to = "Kappa") |>
  ggplot(aes(startT, Kappa, color = kappa_approximation)) +
  geom_point() +
  geom_hline(yintercept = 1, color = "blue") +
  scale_y_log10()+
  theme_classic() +
  labs(x = "day", y = "kappa")
# dev.off()
#confirm kappa
kd(caseTally[states != Sstate])


