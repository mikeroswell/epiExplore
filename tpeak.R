library(shellpipes)
rpcall("tpeak.Rout tpeak.R")
tpeak <- function(N, R0){
  if(N < 1/(R0-1)^2){warning("Low N; stochastic fluctuations may be rel. large")}
  if(R0 < 1.8){warning(paste0("With R0 of ", R0, ", stochastic effects may substantially erode deterministic peak time approximation") )}
  if(R0 < 1.2){warning("with R0<1.2, stochastic exinction likely and peak time approximation very unreliable")}
  pt <- (1/(R0-1))*log(N/R0)
  return(pt)
  }

saveEnvironment()
