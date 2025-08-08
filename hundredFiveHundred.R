library(shellpipes)
rpcall("hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda IBM_3-way.rda myMeehan.rda nbinom_z.rda")

loadEnvironments()
manageConflicts()
startGraphics()
library(bbmle)
library(nloptr)
library(stringr)
library(numDeriv)

if(is.null(get0("setBeta"))){setBeta <- R0}
# assume pars have gamma, tprob both at 1
peakEst <- tpeak(popSize, setBeta)

# generate case counts from infectors from the epidemic growth phase
# get a kappa estimate or two

z <- caseTally[which(rank(iTime)>100 & rank(iTime)<600)]
MLE <- fit_kappaNB(data.frame(z = z))
pf <- iTime[rank(iTime) == 600]/peakEst

earlyStats <- data.frame(R0 = setBeta, kappa = coef(MLE)[[2]], mu = exp(coef(MLE)[[1]]), peakFrac = pf)

print(earlyStats)

saveEnvironment()
