# Code to recapitulate Meehan et al. formalism

# load functions and libraries
source("spreadHelpers.R")
library(ggplot2)
library(dplyr)

# R0 <- 2
# iRat <- 0 # ratio of the reproductive number in the first serial compartment(s) 
# # relative to second; when iRat = 0 we have SEIR chains, and when iRat = 1 we 
# # have an Erlang with size = 2; this ignores duration vs. infectiousness 
# # differences
# pRat <- 0 # fraction of individauls assigned to superspread
# sRat <- 1 # ratio of *sub*spreader's to *super*spreader's reproductive number
# 

# solve for R1, R2
subR <- function(R0, sRat, pRat, iRat){
 
    R0/(pRat*sRat + 1- pRat)
}

superR <- function(superR = superR, sRat){
    superR*sRat
}

# get the intrinsic probability of v offspring for one class
getV <- function(R0, sRat, pRat, iRat, myR, v = v){
   (exp(-v/myR)/myR) * (exp(-iRat*v/myR)-exp(-v/(iRat*myR)))
}

pars = list(R0 = 2, pRat = 0, sRat = 1, iRat = 0)

do.call(getV, c(pars, myR = do.call(subR,pars), v = 1 ))
dexp(1, 0.5)

# probability distribution of individual reproductive numbers, v
vProb <- function(R0, pRat, sRat, iRat
                  , v
                  , pars = NULL
                  , ...){
    if(is.null(pars)){pars <- list(R0, pRat, sRat, iRat)}
    if(!0<=pars$sRat & pars$sRat<=1){stop("Model is defined so that second class is the superspreader.\n sRat must be in [0,1]")}
    R1 <- do.call(subR, pars)
    R2 <- superR(R1, pars$sRat)
    pV <- (1+pars$iRat)/(1-pars$iRat) *
        (pars$pRat*do.call(getV, c(pars, R1, v)) + (1-pars$pRat) * do.call(getV, c(pars, R2, v)))
    return(pV)
}

# set a big number at which to truncate

vMax <- 175
vProb(pars = pars, v = vMax) # confirm it's a "Big" number

# generate N random deviates using accept/reject
sampleV <- function(nTarget = 1e3, vMax = 150
                    , pars = list(R0, pRat, sRat, iRat)
                    , ...){
    vVec <- numeric()
    while(length(vVec)<nTarget){
        cand <- runif(1, min = 0, max = vMax)
        kP <- vProb(pars = pars, v = cand)
        keep <- rbinom(1, 1, kP)
        if(!is.na(keep) & keep >0){vVec <- c(vVec, cand)}
    }
    return(vVec)
}

# see if this goes reasonably fast
replicate(10, system.time(sampleV(nTarget = 100, vMax = vMax, pars = pars)))


# Check that it looks geometric when I make this an S[E]IR model where E is 
# irrelevant for the D0 distribution.
n <- 1e3
myExp <- sort(sampleV(nTarget = n, vMax = vMax
                      , pars = list(R0 = 2, pRat = 0, sRat = 1, iRat = 0)))
realExp <- sort(rexp(n= n, rate = 0.5))
myGeo <- sort(rpois(n = n, myExp))
realGeo <- sort(rgeom(n = n, prob = 1/3))

par(mfrow = c(2,1))
hist(myGeo, xlim = c(0, 10), breaks = 10)
hist(realGeo, xlim = c(0, 10), breaks = 10)

hist(myExp, xlim = c(0, 10), breaks = 30)
hist(realExp, xlim = c(0, 10), breaks = 30)



#######
# will want to separate this out better, but just listing some parameter sets for now

# one class but two I stages, where the reproductive number in first is twice 
# that of the second
pars.twostage <- list(R0 = 2, pRat = 0, sRat = 1, iRat = 2)
# one class but two I stages, where the reproductive number in first is half 
# that of the second
pars.twostage <- list(R0 = 2, pRat = 0, sRat = 1, iRat = 0.5)
# two classes but one stage
pars.twoclass <- list(R0 = 2, pRat = 0.5, sRat = 0.5, iRat = 0)
# full fancyness: two classes, each with two stages
pars.fancy <- list(R0 = 2, pRat = 1/5, sRat = .25, iRat = 2 )

pars.heter <- list(R0 = 0, pRat = 0.01, sRat = .05, iRat = 0.1 )

######################


# twostage
makeDistData(sampleV(pars = pars.twostage)) %>% 
    ggplot(aes(q, cFRealiz))+
    geom_point() +
    geom_point(aes(y = cFIdeal), color = "grey") +
    geom_hline(yintercept = 0.8) +
    geom_vline(xintercept = 0.2) + 
    theme_classic()

makeDistData(sampleV(pars = pars.twoclass)) %>% 
    ggplot(aes(q, cFRealiz))+
    geom_point() +
    geom_point(aes(y = cFIdeal), color = "grey") +
    geom_hline(yintercept = 0.8) +
    geom_vline(xintercept = 0.2) + 
    theme_classic()

makeDistData(sampleV(pars = pars.fancy)) %>% 
    ggplot(aes(q, cFRealiz))+
    geom_point() +
    geom_point(aes(y = cFIdeal), color = "grey") +
    geom_hline(yintercept = 0.8) +
    geom_vline(xintercept = 0.2) + 
    theme_classic()

makeDistData(sampleV(pars = pars.heter, nTarget = 1e3)) %>% 
    ggplot(aes(q, cFRealiz))+
    geom_point() +
    geom_point(aes(y = cFIdeal), color = "grey") +
    geom_hline(yintercept = 0.8) +
    geom_vline(xintercept = 0.2) + 
    theme_classic()




