library(shellpipes)
rpcall("conjecture.base.Rout conjecture.R IBM.base.rda")
rpcall("conjecture.lowGamma.Rout conjecture.R IBM.lowGamma.rda")
rpcall("conjecture.highGamma.Rout conjecture.R IBM.highGamma.rda")
rpcall("highGamma.conjecture.Rout conjecture.R highGamma.IBM.rda")
rpcall("conjecture.highR.Rout conjecture.R IBM.highR.rda")
rpcall("lowGamma.conjecture.Rout conjecture.R lowGamma.IBM.rda")
rpcall("gamma3.conjecture.Rout conjecture.R gamma3.IBM.rda")
rpcall("const.conjecture.Rout conjecture.R const.IBM.rda")
rpcall("highR.conjecture.Rout conjecture.R highR.IBM.rda")
rpcall("base.conjecture.Rout conjecture.R base.IBM.rda")
rpcall("change_6.conjecture.Rout conjecture.R change_6.IBM.rda")
rpcall("change_1p5.conjecture.Rout conjecture.R change_1p5.IBM.rda")
rpcall("change_3.conjecture.Rout conjecture.R change_3.IBM.rda")
rpcall("change_2.conjecture.Rout conjecture.R change_2.IBM.rda")
rpcall("change_12.conjecture.Rout conjecture.R change_12.IBM.rda")
loadEnvironments()
manageConflicts()
library(ggplot2)
startGraphics()


# check Kappa
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}
# is this the way to think about kappa? or do we only want to focus on recovered individuals during the epidemic? What is kappa_effective?
genSecDist <- function(startT, deltaT){
  who <- which(startT <= iTime & iTime < startT+deltaT)
  n <- sum(startT <= iTime & iTime < startT+deltaT)
  secDist <- caseTally[who]
  return(list(secDist = secDist, who = who, n = n, startT = startT, deltaT = deltaT))
}

ktdt <- function(secDist = NULL, startT = NULL, deltaT = NULL){
  if(is.null(secDist)){ secDist <- genSecDist(startT, deltaT)}
  kappa_discrete = kd(secDist$secDist)
  mu <- mean(secDist$secDist)
  v <- var(secDist$secDist)
  kappa_naive = v/mu^2
  return(data.frame(n = secDist$n
                    , kappa_discrete = kappa_discrete
                    , kappa_naive= kappa_naive
                    , mu= mu
                    , v= v
                    , startT = secDist$startT))
}

histTdT <- function(secDist = NULL, startT = NULL, deltaT = NULL){
  if(is.null(secDist)){ secDist <- genSecDist(startT, deltaT)}
  p <- data.frame(secDist = secDist$secDist) |> ggplot(aes(x = secDist))+
    geom_histogram(binwidth = 1, bins = max(secDist$secDist), color = "blue", fill = "navy") +
    theme_classic()

  return(p)
}

withSim <- function(secDist = NULL, startT = NULL, deltaT = NULL, kest = NULL){
  if(is.null(secDist)){ secDist <- genSecDist(startT, deltaT)}
  mom <- ktdt(secDist)
  p <- histTdT(secDist)
  if(is.null(kest)){kest <- mom$kappa_discrete}
  newdf <- data.frame(simD = rnbinom(n = secDist$n, mu = mom$mu, size = 1/kest))
  p = p + geom_histogram(data = newdf, aes(x = simD), alpha = 0.5, fill = "red", color = "white"
                         , binwidth = 1, bins = max(secDist$secDist))
  return(p)
}

# hist(iTime[iTime< tMax])
# plot(sapply(seq(0, 40, 1), function(st){
#   ktdt(st, 10)
# }))

# keff <- purrr::map_dfr(0:33, function(d){
#   ktdt(d, 6)
# })
#
#
# keff |>
#   pivot_longer(cols = c("kappa_discrete", "kappa_naive"), names_to = "kappa_approximation", values_to = "Kappa") |>
#   ggplot(aes(startT, Kappa, color = kappa_approximation)) +
#   geom_point() +
#   geom_hline(yintercept = 1, color = "blue") +
#   scale_y_log10()+
#   theme_classic() +
#   labs(x = "day", y = "kappa")
#
# #confirm kappa
# kd(caseTally[states != Sstate])

secD <- genSecDist(0, dayz)
ktdt(secD)


print(paste("seed =", seed))
saveEnvironment()

