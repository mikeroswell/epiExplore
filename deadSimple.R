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
beta1 <- 1.5
beta2 <- 8
gamm <- 1

# how many slices?
n <- 300
xmax <- 30.5


act1 <- beta1/gamm
act2 <- beta2/gamm


actDist <- function(low = 0, high = 30, n = n, act){
  x <- seq(low, high, length.out = n)
  d <- dexp(x, rate = 1/act)
  inpt_name <- paste0("act_", deparse(substitute(act)))
   # d/sum(d)
  return(data.frame(x, d,  distr = rep(inpt_name, length(x))))
}

# deviates to sample
# n <- 15000

actHist <- function(n, act){
  d <- rexp(n = n, rate = 1/act)
  return(d)
}

secDist <- function(high = 30, act){
  x <- 0:high
  d <- dgeom(x, prob = makeP(1/act))
  inpt_name <- paste0("scnd_", deparse(substitute(act)))
  # d/sum(d)
  # d*length(betaT)
  return(data.frame(x, d, distr = rep(inpt_name, length(x))))
}

secHist <- function(n, act){
  d <- rgeom(n = n, prob = makeP(1/act))
  return(d)
}

deadDat <- bind_rows( actDist(low = 0, high = xmax, n = n, act = act1)
                       , actDist(low = 0, high = xmax, n = n, act = act2)
                       , secDist(high = xmax, act = act1)
                       , secDist(high = xmax, act = act2)
                       )
n <- 2e4
histDat <- data.frame(ind = 1:n
                      , activity_1 = actHist(n, act1)
                      , activity_2 = actHist(n, act2)
                      , secondary_1 = secHist(n, act1)
                      , secondary_2 = secHist(n, act2)
                      )


saveEnvironment()
