library(shellpipes)
manageConflicts()
startGraphics()
library(checkPlotR) # checkPlots repo (Dushoff owner)
# https://github.com/dushoff/checkPlots.git
library(purrr)
library(bbmle, mask.ok = "slice")
library(ggplot2)
library(RTMB)

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

# seems like this is mostly working but giving wonky CI that seem like numeric
# issues, and at least are sometimes associated with bbmle warning:
# Warning messages:
# 1: In bbmle::mle2(minuslogl = function (x, lkap, M)  :
#                     convergence failure: code=52 (ERROR: ABNORMAL_TERMINATION_IN_LNSRCH)
kapNB3 <- function(x, lkap, M){
  mu <- M
  kap <- exp(lkap)
  -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))

}


kapRTMB <- function(x, lkap, lM){
  log_mu <- lM
  V <- exp(lkap) * exp(lM)^2 + exp(lM)
  log_var_minus_mu <- log(V-exp(lM))
  -sum(RTMB::dnbinom_robust(x, log_mu = log_mu, log_var_minus_mu = log_var_minus_mu, log = TRUE))
}

# kapRTMB(x, log(0.5),log(3))

# ntest <- 50
# x <- rgeom(500, 1/3)
# lk <- log(runif(ntest, 0, 50))
# lm <- log(runif(ntest, 0, 50))
# # this is weirdly imaginary
# all.equal(
#   sapply(1:ntest, function(ind){
#     kapNB(x, lk[ind], lm[ind])}
#     )
#   , sapply(1:ntest, function(ind){
#     kapRTMB(x, lk[ind], lm[ind]) |> as.double()}
#     )
#   )
#
# sapply(1:3, function(j){
#   RTMB::dnbinom_robust(1:50, j, 1, log = TRUE) |> sum() * -1}
#   )


set.seed(1912)
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
  pro <- profile(mlefit)
  ci <- confint(pro)
  return(list(est = exp(est)
              , lower = exp(ci[1])
              , upper = exp(ci[3])) )
}
nreps <- 200

mysim <- map(1:nreps, function(nr){
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
}) |>
  list_rbind()

# mysim |>
#   filter(est > upper | est<lower)

mysim |>
  filter(upper > 20)

mysim |>
  mutate(upper = if_else(upper>20, Inf, upper)) |>
                  rangePlot(target = 1
                   , opacity = 1
                   , targNum = 200)
