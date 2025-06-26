library(shellpipes)
rpcall("change_3.toPeak.Rout toPeak.R tpeak.rda change_3.conjecture.rda nbinom_z.rda")
rpcall("change_12.toPeak.Rout toPeak.R tpeak.rda change_12.conjecture.rda nbinom_z.rda")
rpcall("change_6.toPeak.Rout toPeak.R tpeak.rda change_6.conjecture.rda nbinom_z.rda")
rpcall("change_2.toPeak.Rout toPeak.R tpeak.rda change_2.conjecture.rda nbinom_z.rda")
rpcall("change_1p5.toPeak.Rout toPeak.R tpeak.rda change_1p5.conjecture.rda nbinom_z.rda")

loadEnvironments()
manageConflicts()
startGraphics()
library(bbmle)
library(nloptr)
library(stringr)
library(numDeriv)
library(ggplot2)
# assume pars have gamma, tprob both at 1
peakEst <- tpeak(popSize, setBeta)

# generate case counts from infectors from the epidemic growth phase
dat <- genSecDist(0, 0.8*peakEst)
# extend a bit if this is too few cases still
if(sum(dat$secDist>0)<100){
  print("80% of peak is too few infectors")
  dat <- genSecDist(0, 0.9*peakEst)
}
if(sum(dat$secDist>0)<100){
  print("90% of peak is too few infectors")
  dat <- genSecDist(0, peakEst)
}

# get a kappa estimate or two
naive <- ktdt(dat)
MLE <- fit_kappaNB(data.frame(z = dat$secDist))
print(naive)
print(MLE)
withSim(dat, kest = coef(MLE)[[2]])

saveEnvironment()