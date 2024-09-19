library(shellpipes)
manageConflicts()

library(purrr)
library(dplyr)

nreps <- 1e4

mysim <- map_dfr(1:nreps, function(nr){
  n <- 3
  gamm <- 1/3
  rtimes <-rexp(n = n, gamm)

  cd <- rpois(n = n, rtimes)
  mu <- mean(cd)
  V <- var(cd)
  kap <- (V-mu)/mu^2
  return(data.frame(mu, V, kap))
})

summary(mysim)

print(mysim 
	|> summarize(V=mean(V), mu=mean(mu))
	|> mutate(kap = (V-mu)/mu^2)
)

# what about a synthetic kappa, says JD?
kappa_syn <- (mean(mysim$V)-mean(mysim$mu))/mean(mysim$mu)^2
kappa_syn # looks on target
