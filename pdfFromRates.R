library(shellpipes)
rpcall("pdfFromRates.Rout pdfFromRates.R")
manageConflicts()
startGraphics()
library(dplyr)
library(purrr)
library(ggplot2)
loadEnvironments()

makePDF <- function(x, pars = pars){
    list2env(pars, envir = environment())
   dens <- sum(sapply(1:length(fracs), function(i){
      fracs[i]*dexp(x, rate = 1/rNums[i], log = FALSE)

    }) )

    return(dens)


}

makePDF <- Vectorize(makePDF, "x")

getHigh <- function(x, pars = pars){
  list2env(pars, envir = environment())
  phigh <- sum(sapply(1:length(fracs), function(i){
    fracs[i]*pexp(x, rate = 1/rNums[i], log = FALSE)

  }) )

  return(phigh)


}


samplePDF <- function(N, xmax, pars, sens = 1e-6){
  ishigh <- getHigh(xmax, pars)
  if(1-ishigh > sens){message(paste0("with xmax = "
                                   , xmax
                                   , ", the probability at the upper limit is "
                                   , ishigh))}
  X <- seq(0, xmax, by = xmax/(N-1))
  dens <- sapply(X, FUN = function(x){makePDF(x, pars)})
  return(data.frame(x = X, density = dens))

}

# m1 <- cmptMod(R_0 = 10, x = 0.6, xChoice = "low", scaleRNum = 3)


# makePDF(1, pars = m1)
# v <- samplePDF(N = 2000, xmax = 450, pars = m1)
getCounts <- function(v, N = length(v$x)){
  y <- rpois(N, sample(v$x, size = N, prob = v$density, replace = TRUE))
  return(y)
}

saveEnvironment()
