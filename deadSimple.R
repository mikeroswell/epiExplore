# make dead simple activity and realized distributions
library(shellpipes)
rpcall("deadSimple.Rout deadSimple.R spreadHelpers.rda")
loadEnvironments()
startGraphics()
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# parms
beta1 <- 0.15
beta2 <- 0.8
gamm <- 1/10

# # how many slices?
# n <- 151
# # range
# betaT <- c(seq(0, 6, length.out = n), 0.0001, 0.9999, 1.0001, 1.9999, 2.001, 2.9999, 3.0001, 3.9999, 4.0001)

act1 <- beta1/gamm
act2 <- beta2/gamm


actDist <- function(betaT, act){
  d <- dexp(betaT, rate = 1/act)
   # d/sum(d)
  d
}

# deviates to sample
# n <- 15000

actHist <- function(n, act){
  d <- rexp(n = n, rate = 1/act)
  return(d)
}

secDist <- function(betaT, act){
  d <- dgeom(betaT, prob = makeP(1/act))
  # d/sum(d)
  # d*length(betaT)
  d
}

secHist <- function(n, act){
  d <- rgeom(n = n, prob = makeP(1/act))
  return(d)
}

# deadDat <- data.frame( betaT = betaT
#                        , act_1 = actDist(betaT, act1)
#                        , act_2 = actDist(betaT, act2)
#                        , sec_1 = secDist(betaT, act1)
#                        , sec_2 = secDist(betaT, act2)
#                        )
n <- 2e4
histDat <- data.frame(ind = 1:n
                      , activity_1 = actHist(n, act1)
                      , activity_2 = actHist(n, act2)
                      , secondary_1 = secHist(n, act1)
                      , secondary_2 = secHist(n, act2)
                      )


saveEnvironment()