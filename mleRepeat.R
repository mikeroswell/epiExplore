library(purrr)
library(bbmle)

# define NLL for kappa
kapNB <- function(x, kap, M){
  -sum(dnbinom(x, mu = M, size = 1/kap, log = TRUE))

}

set.seed(1905)
# function to compute MLE kappa with CI
kapEst <- function(x){
  mlefit <- bbmle::mle2(kapNB
              , start = list(kap = 0.5, M = 1.5)
              , data = list(x = x)
              # , method = "L-BFGS-B"
              # , optimizer = "nlminb"
              # , lower = c(1e-9, 1e-9)
              )
  est <- as.numeric(coef(mlefit)[1])
  ci <- confint(profile(mlefit))
  return(list(est = est, lcl = ci[1], ucl = ci[2] ))
}

nreps <- 50

mysim <- map_dfr(1:nreps, function(nr){
  n <- 6
  gamm <- 1/3
  rtimes <-rexp(n = n, gamm)
  rtimes

  cd <- rpois(n = n, rtimes)
  mu <- mean(cd)
  V <- var(cd)
  kapNaive <- (V-mu)/mu^2
  kapMLE <- kapEst(cd)
  return(data.frame(mu, V, kapNaive
                    , kapMLE = kapMLE$est, lcl = kapMLE$lcl, ucl = kapMLE$ucl))
})
