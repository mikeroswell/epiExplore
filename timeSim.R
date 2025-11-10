library(shellpipes)
library(deSolve)
loadEnvironments()
boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    y <- sum(yvec)
    ydots <- numeric(cars)
    Bt<- B0*exp(alpha*sin(omega*time))
    xdot <- -Bt*y*x^(kpa+1) + cars*sigma*r
    ydots[[1]] <- Bt*y*x^(kpa+1) - cars*yvec[[1]]
    cumdot <- Bt*y*x^(kpa+1)
    if (cars > 1) {
      ydots[2:cars] <- cars * (yvec[1:(cars - 1)] - yvec[2:cars])
    }
    rdot <- cars*yvec[[cars]] - cars*sigma*r
    out <- c(xdot, ydots, rdot, cumdot)
    names(out) <- c("xdot", paste0("y", 1:cars, "dot"), "rdot", "cumdot")
    return(list(out))
  }
  )
}
sim <- function(kpa = 0, B0=1,  cars = 1, finTime=365,
                timeStep=0.1, dfun=boxcar, alpha=0, omega=0, sigma=0, t0 =0, 
                y0 = 1e-9){
  x0 <- 1-y0
  r0 <- 0
  cum0 <- 0
  infc <- rep(y0/cars,cars) # y0 is distributed over all infectious cars
  names(infc) <- paste0("y", 1:cars)
  y_init <- c(x = x0, infc, r=r0, cum = cum0)
  if(t0 !=0) timePoints<- c(0, seq(from=t0, to=t0 + finTime, by=timeStep))
  else timePoints<- seq(from=t0, to=t0 + finTime, by=timeStep)
  print(paste0("min",min(timePoints),"max",max(timePoints)))
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=timePoints
    , parms=list(omega=omega, B0=B0, alpha = alpha,  cars = cars,
                 kpa = kpa, sigma=sigma)
  ))
  if(t0!=0){sim <- sim[!sim$time==0,]}
  return(within(sim, {
    if (cars>1){
      y <- rowSums(as.data.frame(mget(paste0("y", 1:cars))))
    }
    else{
      y <- y1
    }
    inc <- c(diff(cum),0)
    
  }))
}
saveEnvironment()
