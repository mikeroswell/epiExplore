library(shellpipes)
rpcall("kappaFns.Rout kappaFns.R")
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}

kapSum <- function(x){
  kappa_discrete <- kd(x)
  mu <-  mean(x)
  v <- var(x)
  kappa_naive <- v/mu^2
  nCases <- sum(x)
  infectors <- length(x)

  return(data.frame(
    kappa_discrete
    , mu
    , v
    , kappa_naive
    , nCases
    , infectors
  ))
}
saveEnvironment()