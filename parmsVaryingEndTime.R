library(shellpipes)
loadEnvironments()
B0 <- c(2,4,8)
steps<- 6e3
cars <- c(1) #number of compartments
t0 <- 0 # initial time
#cutoffTime <- c(1, 2, 3, 5, 10, 30) # cut-off times according to which cohorts are selected
cutoffTime <- c(2, 2.93, 6.84, 20) # cut-off times according to which cohorts are selected
finTime <- 100
temporalFinalTime <- max(cutoffTime)
y0<-1e-9
cohortProp <- 0.6
saveEnvironment()
