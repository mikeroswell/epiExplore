# cases per case with waning Re

# First try a linear decrease in Re with an exponential recovery time
# might want to go to logistic decrease or something similar at some point too.

library(shellpipes)
rpcall("decreasingRe.Rout decreasingRe.R spreadHelpers.rda")
library(dplyr)
library(purrr)
loadEnvironments()
startGraphics()

reps <- 999
n <- 500
rtime <- 5
rTimes <- rexp(n, rate = 1/rtime)
R0 <- 8
Re <- function(x){R0/rtime - 0.05*x}

Kappas <- map_dfr(1:reps, function(rep){
  cCount <- sapply(1:n, function(ind){
    cases <- rpois(1, integrate(Re, 0, rTimes[ind])[[1]])
    return(cases)
    })
  kappa <- discKappa(cCount)
  mu <- mean(cCount)
  v <- var(cCount)
  heavy <- mu>R0
  return(data.frame(kappa, mu, v, heavy))
  })

sum(Kappas$heavy)
hist(Kappas$kappa)
