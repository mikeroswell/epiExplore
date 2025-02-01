# simulate from exponential-lognormal mixture
library(shellpipes)
rpcall("lognormal_exp_sim.Rout lognormal_exp_sim.R spreadHelpers.rda")
library(dplyr)
library(tidyr)

loadEnvironments()
n <- 3e4
deviates <- rexp(n, rate = 1/rlnorm(n, meanlog = log(1)))
ctsKappa(deviates)
stoch <- rpois(n, deviates)
discKappa(stoch)

deviates <- rexp(n, rate = 1/rlnorm(n, meanlog = log(1), sdlog = 0.5))
ctsKappa(deviates)
stoch <- rpois(n, deviates)
discKappa(stoch)

deviates <- rexp(n, rate = 1/rlnorm(n, meanlog = log(1), sdlog = 0.1))
ctsKappa(deviates)
stoch <- rpois(n, deviates)
discKappa(stoch)

deviates <- rexp(n, rate = 1/rlnorm(n, meanlog = log(1), sdlog = 5))
ctsKappa(deviates)
stoch <- rpois(n, deviates)
discKappa(stoch)

plot(seq(0, 4, length.out = 20), dlnorm(seq(0, 4, length.out = 20), meanlog = log(1), sdlog = 0.25))


deviates <- rexp(n, rate = 1/rgamma(n, shape = 9, scale = 1/9))
ctsKappa(deviates)
stoch <- rpois(n, deviates)
discKappa(stoch)


deviates <- rexp(n, rate = 1/rgamma(n, shape = 25, scale = 1/25))
ctsKappa(deviates)
stoch <- rpois(n, deviates)
discKappa(stoch)



plot(seq(0, 4, length.out = 20), dgamma(seq(0, 4, length.out = 20), shape = 25, scale = 1/25))

alph <- 2
R0 <- 8
deviates <- rexp(n, rate = 1/(R0* rbeta(n, alph, alph*(R0-1))))
ctsKappa(deviates)

alph <- 2
R0 <- 2.5
deviates <- rexp(n, rate = 1/(R0* rbeta(n, alph, alph*(R0-1))))
ctsKappa(deviates)

hist(R0* rbeta(n, alph, alph*(R0-1)))


plot(seq(0, 20, 0.1), dlnorm(seq(0, 20, 0.1), meanlog = 0, sdlog = 10))

log(dbeta(runif(50), )
