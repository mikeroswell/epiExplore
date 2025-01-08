library(shellpipes)
rpcall("conjecture.base.Rout conjecture.R IBM.base.rda")
rpcall("conjecture.lowGamma.Rout conjecture.R IBM.lowGamma.rda")
rpcall("conjecture.highGamma.Rout conjecture.R IBM.highGamma.rda")
rpcall("highGamma.conjecture.Rout conjecture.R highGamma.IBM.rda")
rpcall("conjecture.highR.Rout conjecture.R IBM.highR.rda")
rpcall("highR.conjecture.Rout conjecture.R highR.IBM.rda")
rpcall("base.conjecture.Rout conjecture.R base.IBM.rda")
rpcall("lowGamma.conjecture.Rout conjecture.R lowGamma.IBM.rda")
loadEnvironments()
manageConflicts()
# check Kappa
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}
# is this the way to think about kappa? or do we only want to focus on recovered individuals during the epidemic? What is kappa_effective?


ktdt <- function(startT, deltaT){
  who <- which(startT <= iTime & iTime < startT+deltaT)
  n <- sum(startT <= iTime & iTime < startT+deltaT)
  secDist <- caseTally[who]
  kappa_discrete = kd(secDist)
  mu <- mean(secDist)
  v <- var(secDist)
  kappa_naive = v/mu^2
  return(data.frame(n, kappa_discrete, kappa_naive, mu, v, startT))
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

ktdt(0, dayz)
print(paste("seed =", seed))
saveEnvironment()

