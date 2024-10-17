library(shellpipes)
manageConflicts()
startGraphics()
if (!require("checkPlotR")) stop("please install checkPlotR via remotes::install_github('dushoff/checkPlots')")
library(checkPlotR) # checkPlots repo (Dushoff owner)
# https://github.com/dushoff/checkPlots.git
library(purrr)
library(bbmle, mask.ok = "slice")
library(ggplot2)
library(RTMB)
library(nloptr)
rpcall("MLE_start.Rout MLE_start.pipestar MLE_start.R kapWrap.rda")

loadEnvironments()

# # define NLL for kappa
# kapNB <- function(x, lkap, lM){
#   mu <- exp(lM)
#   kap <- exp(lkap)
#   -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))
#
# }
#
#
# kapNB2 <- function(x, kap, lM){
#   mu <- exp(lM)
#   -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))
#
# }

# seems like this is mostly working but giving wonky CI that seem like numeric
# issues, and at least are sometimes associated with bbmle warning:
# Warning messages:
# 1: In bbmle::mle2(minuslogl = function (x, lkap, M)  :
#                     convergence failure: code=52 (ERROR: ABNORMAL_TERMINATION_IN_LNSRCH)
# kapNB3 <- function(x, lkap, M){
#   mu <- M
#   kap <- exp(lkap)
#   -sum(dnbinom(x, mu = mu, size = 1/kap, log = TRUE))
#
# }


# kapRTMB <- function(x, lkap, lM){
#   log_mu <- lM
#   V <- exp(lkap) * exp(lM)^2 + exp(lM)
#   log_var_minus_mu <- log(V-exp(lM))
#   -sum(RTMB::dnbinom_robust(x, log_mu = log_mu, log_var_minus_mu = log_var_minus_mu, log = TRUE))
# }

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
kapEst <- function(dd, z, ste = 0.1){
  mlefit <- fit_kappaNB(dd, z)
  est <- as.numeric(coef(mlefit)[2])
  p0 <- profile(mlefit, method = "uniroot", std.err = ste, maxsteps = 1e5)
  # print(p0)
  ci <- confint(p0)
  # return(list(est = exp(est)
  #             , lower = exp(ci[1])
  #             , upper = exp(ci[3])) )
  return(list(est = est
              , lower = ci[2]
              , upper = ci[4]
              )
         )
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


# kapEst3 <- function(x){
#   mlefit <- bbmle::mle2(kapNB3
#                         , start = list(lkap = log(0.5), M = 1.5)
#                         , data = list(x = x)
#                         , method = "L-BFGS-B"
#                         # , optimizer = "nlminb"
#                         , lower = c(lkap = log(1e-9), M = 1e-9)
#   )
#   est <- as.numeric(coef(mlefit)[1])
#   pro <- profile(mlefit)
#   ci <- confint(pro)
#   return(list(est = exp(est)
#               , lower = exp(ci[1])
#               , upper = exp(ci[3])) )
# }

bsci <- function(x, alpha = 0.05, N){
  s <- replicate(N, sample(x, length(x), replace = TRUE))
  m <- apply(s, 2, mean)
  v <- apply(s, 2, var)
  kn <- ifelse(v + m == 0, 0, (v-m)/m^2)
  M <- mean(x)
  V <- var(x)
  est <- ifelse(M+V == 0, 0, (V-M)/M^2)
  lcl <- quantile(kn, alpha/2)
  ucl <- quantile(kn, 1-alpha/2)
  return(list(est = est, lcl = lcl, ucl = ucl))
}

# N <- 399
nreps <- 200

loopfun <- function(nr) {
  set.seed(1000 + nr)
  if (nr %% 10 == 0) cat(".")
  n <- 6
  gamm <- 1/3
  rtimes <-rexp(n = n, gamm)
  cd <- rpois(n = n, rtimes)
  mu <- mean(cd)
  V <- var(cd)
  tmb_data <<- list(x = cd)
  # kapBS <- bsci(cd, N = N)
  kapNaive <- (V-mu)/mu^2
  kapMLE <- nbEstCI()
  # k2 <- kapEst2(cd)
  # print(fit_kappaNB(cd))
  if(is.na(kapMLE$upper)){
    print(cd)
  }
  return(data.frame(nr
                    , mu
                    , V
                    , kapNaive
                    , est = kapMLE$est
                    , lower = if_else(is.na(kapMLE$lower), 0, kapMLE$lower)
                    , upper = kapMLE$upper
                    # , estbs = kapBS$est
                    # , uclbs = kapBS$ucl
                    # , lclbs = kapBS$lcl
                    # , e2 = k2$est
                    # , l2 = k2$lower
                    # , u2 = k2$upper
                    )
         )
}
cur_rep <- NA
mysim <- map(1:nreps, loopfun) |>
  list_rbind()

## fails in step 3
## debug(loopfun)
## debug(nbEstCI)
##loopfun(3)
dnbinom2(3, mu = 1, var = 1, log = TRUE)  ## NaN
dnbinom_robust(3, log_mu = 0, log_var_minus_mu = -200, log = TRUE) ## OK
dnbinom_robust(3, log_mu = 0, log_var_minus_mu = -700, log = TRUE) ## OK
dnbinom_robust(3, log_mu = 0, log_var_minus_mu = -Inf, log = TRUE) ## NaN

# mysim |>
#   filter(est > upper | est<lower)
mysim
# mysim |>
#   filter(upper > 20)
# mysim |>
#   ggplot(aes(est, kapNaive)) +
#   geom_point() +
#   theme_classic()

# return bias
mysim[mysim$est<0,]
sum(mysim$est<1)/length(mysim$est)
sum(mysim$est>1)/length(mysim$est)
mysim |>
  mutate(lower = if_else(is.na(lower), 0, lower)
  , upper = if_else(upper>20, Inf, upper)
  ) |>
                  rangePlot(target = 1
                   , opacity = 1
                   , targNum = 200) #+
  # ylim(c(-1, 20))

# mysim |>
#   mutate(lower = lclbs
#          , upper = uclbs
#          , est = estbs
#   ) |>
#   rangePlot(target = 1
#             , opacity = 1
#             , targNum = 200) +
#   labs(title = "Bootstrap")
