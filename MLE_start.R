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

set.seed(1905)
# function to compute MLE kappa with CI
kapEst <- function(x){
  mlefit <- bbmle::mle2(kapNB
                        , start = list(lkap = log(0.5), lM = log(1.5))
                        , data = list(x = x)
                        , method = "L-BFGS-B"
                        # , optimizer = "nlminb"
                        , lower = c(log(1e-9), log(1e-9))
  )
  est <- as.numeric(coef(mlefit)[1])
  ci <- confint(profile(mlefit))
  return(list(est = exp(est)
              , lower = exp(ci[1])
              , upper = exp(ci[2])) )
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
  kapMLE <- kapEst(cd)
  return(data.frame(nr
                    , mu
                    , V
                    , kapNaive
                    , est = kapMLE$est
                    , lower = if_else(is.na(kapMLE$lower), 0, kapMLE$lower)
                    , upper = kapMLE$upper
                    )
         )
})

mysim |>
  filter(est > upper | est<lower)

mysim |> rangePlot(target = 1
                   , opacity = 1
                   , targNum = 200)
