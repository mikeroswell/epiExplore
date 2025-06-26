 library(shellpipes)
rpcall("IBM_change_1p5_pars.Rout change_1p5.R IBM_base_pars.rda")
 loadEnvironments()

 setBeta <- 1.5
 dailyRate <- setBeta
 saveEnvironment()