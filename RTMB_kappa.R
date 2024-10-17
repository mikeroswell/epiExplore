# install the latest adcomp
# and reinstall RTMB too
# remotes::install_github("https://github.com/kaskr/RTMB", subdir="RTMB")

library(RTMB)
library(bbmle)

x <- rpois(1000, rexp(1000, 1/2))

kapNB2 <- function(pars) {
  getAll(pars, tmb_data)
  mu <- exp(logM)
  -sum(dnbinom2(x, mu = mu, var = mu*(1+kap*mu), log = TRUE))
}

p0 <- list(logM = log(2), kap = 0.5)

tmb_data <- list(x = x)

ff <- MakeADFun(kapNB2, parameters = p0)
# ff$fn()
# ff$gr()

RTMBfit <- with(ff,
     nlminb(start = par
            , objective = fn
            , lower = c(kap = 0)
            , control = list(trace = 1)
     )
)

bbAD <- bbmle::mle2(minuslogl = ff$fn
           , start = unlist(ff$par)
           , vecpar = TRUE
           , gr = ff$gr
           , lower = c(kap = 0)
           , method = "BFGS"
           )

hes <- sqrt(diag(solve(numDeriv::hessian(func = ff$fn, fit$par))))
profile(RTMBfit)
