library(shellpipes)
manageConflicts()
library(RTMB)
library(bbmle)
library(dplyr, mask.ok = "slice")


kapNB2 <- function(pars) {
  getAll(pars, tmb_data)
  mu <- exp(logM)
  -sum(dnbinom2(x, mu = mu, var = mu*(1+kap*mu), log = TRUE))
}

nbEstCI <- function(p0 = list(
                      logM = log(2)
                      , kap = 0.5
                      )
                    ){
  ff <- MakeADFun(kapNB2, parameters = p0, silent = TRUE)
  parnames(ff$fn) <- names(unlist(ff$par))
  bbAD <- bbmle::mle2(minuslogl = ff$fn
                    , start = unlist(ff$par)
                    , vecpar = TRUE
                    , gr = ff$gr
                    , lower = c(kap = 0)
                    , method = "L-BFGS-B"
                    )

  pp2 <- profile(bbAD)
  CI <- confint(pp2)
  return(list(est = as.numeric(coef(bbAD)[2])
              , lower = CI[2]
              , upper = CI[4]
  )
  )
}

saveEnvironment()
