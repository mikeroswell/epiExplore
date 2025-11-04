library(shellpipes)
rpcall("v1.kappa.Rout v1.kappa.R IBM_for_v1.rda")
loadEnvironments()
manageConflicts()
library(ggplot2)
library(dplyr)
library(purrr)
startGraphics()

kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}

statCohort <- function(halfdeltaT){
  deltaT<- halfdeltaT/2
  startT<-0
  who <- which(startT <= iTime & iTime < startT+deltaT)
  n <- sum(startT <= iTime & iTime < startT+deltaT)
  secDist <- caseTally[who]
  kappa_discrete <- kd(secDist)
  mu <- mean(secDist)
  v <- var(secDist)
  return(data.frame(mu = mu, kappa_discrete=kappa_discrete, variance = v,  n = n
                    ,startT = startT, deltaT = deltaT))
}

stats_cohorts<-case_per_case_overall$halfDayz |> unique() |>map_dfr(statCohort)

v1_k <- (stats_cohorts |> ggplot()
         + aes(x=deltaT, y = kappa_discrete)
         + geom_point()
         + labs(x="time (normalized by infectious duration)"
                ,y=bquote(kappa[discrete])
                ,title=bquote(kappa[discrete]~"over time for"~R[0]~":"~.(setBeta)))
         
         )
print(v1_k)
