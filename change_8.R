 library(shellpipes)
rpcall("IBM_change_8_pars.Rout change_8.R IBM_base_pars.rda")
 loadEnvironments()

 setBeta <- 8
 dailyRate <- setBeta
 saveEnvironment()