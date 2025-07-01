library(shellpipes)
rpcall("hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda IBM_3-way.rda myMeehan.rda nbinom_z.rda")
rpcall("change_1p5.hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda change_1p5.conjecture.rda nbinom_z.rda")
rpcall("change_2.hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda change_2.conjecture.rda nbinom_z.rda")
rpcall("change_3.hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda change_3.conjecture.rda nbinom_z.rda")
rpcall("change_6.hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda change_6.conjecture.rda nbinom_z.rda")
rpcall("base.hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda base.conjecture.rda nbinom_z.rda")
rpcall("change_12.hundredFiveHundred.Rout hundredFiveHundred.R tpeak.rda change_12.conjecture.rda nbinom_z.rda")

loadEnvironments()
manageConflicts()
startGraphics()
library(bbmle)
library(nloptr)
library(stringr)
library(numDeriv)

if(is.null(setBeta)){setBeta <- R0}
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
