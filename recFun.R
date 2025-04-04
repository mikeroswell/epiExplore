library(shellpipes)
rpcall("recFun.Rout recFun.R")
loadEnvironments()
recFun <- function(x, rPar = "exp"){
  if(rPar == "exp"){
    y <- rexp(x, rate = setGamma)
  }
  if(rPar == "const"){
    y <- rep(setGamma, max(x))
  }
  if(rPar == "gamma3"){
    y <- rgamma(max(x), rate = setGamma/3, shape = 3)
  }
  return(y)
}
saveEnvironment()
