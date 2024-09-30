library(shellpipes)
manageConflicts()
startGraphics()
library(checkPlotR)
library(purrr)
library(bbmle, mask.ok = "slice")
library(ggplot2)

# define NLL for kappa
kapNB <- function(x, lkap, lM){
  mu <- exp(lM)
  kap <- exp(lkap)
  -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))

}


kapNB2 <- function(x, kap, lM){
  mu <- exp(lM)
  -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))

}

kapNB3 <- function(x, lkap, M){
  mu <- M
  kap <- exp(lkap)
  -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))

}


set.seed(1905)
# function to compute MLE kappa with CI
kapEst <- function(x){
  mlefit <- bbmle::mle2(kapNB
                        , start = list(lkap = log(0.5), lM = log(1.5))
                        , data = list(x = x)
                        , method = "L-BFGS-B"
                        # , optimizer = "nlminb"
                        , lower = c(lkap = log(1e-9),  lM = log(1e-9))
  )
  est <- as.numeric(coef(mlefit)[1])
  ci <- confint(profile(mlefit))
  return(list(est = exp(est)
              , lower = exp(ci[1])
              , upper = exp(ci[3])) )
}
# kapEst2 <- function(x){
#   mlefit <- bbmle::mle2(kapNB2
#                         , start = list(kap = 0.5, lM = log(1.5))
#                         , data = list(x = x)
#                         , method = "L-BFGS-B"
#                         # , optimizer = "nlminb"
#                          , lower = c(1e-9, log(1e-9))
#   )
#   est <- as.numeric(coef(mlefit)[1])
#   ci <- confint(profile(mlefit))
#   return(list(est = est
#               , lower = ci[1]
#               , upper = ci[3]
#               )
#   )
# }


kapEst3 <- function(x){
  mlefit <- bbmle::mle2(kapNB3
                        , start = list(lkap = log(0.5), M = 1.5)
                        , data = list(x = x)
                        , method = "L-BFGS-B"
                        # , optimizer = "nlminb"
                        , lower = c(lkap = log(1e-9), M = 1e-9)
  )
  est <- as.numeric(coef(mlefit)[1])
  ci <- confint(profile(mlefit))
  return(list(est = exp(est)
              , lower = exp(ci[1])
              , upper = exp(ci[3])) )
}
nreps <- 200

mysim <- map_dfr(1:nreps, function(nr){
  n <- 6
  gamm <- 1/3
  rtimes <-rexp(n = n, gamm)
  rtimes

  cd <- rpois(n = n, rtimes)
  mu <- mean(cd)
  V <- var(cd)
  kapNaive <- (V-mu)/mu^2
  kapMLE <- kapEst3(cd)
  # k2 <- kapEst2(cd)
  return(data.frame(nr
                    , mu
                    , V
                    , kapNaive
                    , est = kapMLE$est
                    , lower = if_else(is.na(kapMLE$lower), 0, kapMLE$lower)
                    , upper = kapMLE$upper
                    # , e2 = k2$est
                    # , l2 = k2$lower
                    # , u2 = k2$upper
                    )
         )
})

mysim |>
  filter(est > upper | est<lower)

mysim

mysim |>
  mutate(upper = if_else(upper>20, Inf, upper)) |>
                  rangePlot(target = 1
                   , opacity = 1
                   , targNum = 200)
