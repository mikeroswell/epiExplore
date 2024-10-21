# sometimes getting NA from Ben's machinery for kappa UCL. What's going on?
library(shellpipes)
library(bbmle, mask.ok = "slice")
library(nloptr)
loadEnvironments()

dtest <- data.frame(z = c(4,0,0,6,0,0))
ftest <- fit_kappaNB(dtest)
ptest <- profile(ftest, std.err = 0.1) # warnings, upper UCL for kappa not found
citest <- confint(ptest)
citest
ptest <- profile(ftest, std.err = 0.1, maxsteps = 1e5) # takes a minute, but we find it
citest <- confint(ptest)
citest
