library(RTMB)
library(dplyr)

update_pkg <- function() {
    library(remotes)
    install_github('kaskr/adcomp/TMB')
    install.packages('RTMB', type = 'source')
}

## test extreme value to make sure we have the version of TMB with dnbinom_robust fixed
fixtmb <- function(x) Im(unclass(x))

calc0 <- function(x=1, mu=1, size=1e15) {
    d1 <- dnbinom(x, size = size, mu = mu, log = TRUE)
    ## var-mu = mu^2/size
    d2 <- RTMB::dnbinom_robust(x, log_var_minus_mu = 2*log(mu)-log(size), log_mu = log(mu), log = TRUE) |> fixtmb()
    return(c(d1,d2))
}

print(res0 <- abs(diff(calc0(x=3, mu=3))))

if (res0 > 0.1) {
    stop("consider \"library(remotes); install_github('kaskr/adcomp/TMB'); install.packages('RTMB', type = 'source')\"")
}

# install the latest adcomp
# and reinstall RTMB too
# remotes::install_github("https://github.com/kaskr/RTMB", subdir="RTMB")

library(bbmle)

x <- rpois(1000, rexp(1000, 1/2))

kapNB2 <- function(pars) {
  getAll(pars, tmb_data)
  mu <- exp(logM)
  -sum(dnbinom2(x, mu = mu, var = mu*(1+kap*mu), log = TRUE))
}

p0 <- list(logM = log(2), kap = 0.5)

tmb_data <- list(x = x)

ff <- MakeADFun(kapNB2, parameters = p0, silent = TRUE)
# ff$fn()
# ff$gr()

RTMBfit <- with(ff,
     nlminb(start = par
            , objective = fn
            , lower = c(kap = 0)
            , control = list(trace = 1)
     )
     )

## tmbprofile only does one parameter profile at a time
pp <- TMB::tmbprofile(ff, name = "kap")
dd <- pp |> as_tibble() |> mutate(value = value - min(value))
par(las=1)
plot(value ~ kap, data = dd)
confint(pp)

## this is a bit weird in 1D
parnames(ff$fn) <- names(unlist(ff$par))
bbAD <- bbmle::mle2(minuslogl = ff$fn
           , start = unlist(ff$par)
           , vecpar = TRUE
           , gr = ff$gr
           , lower = c(kap = 0)
           , method = "L-BFGS-B"
           )

## difference between default scale of the two profiles
## mle profile returns signed square root of deviance
## see mutate() below
pp2 <- profile(bbAD)
dd2 <- (as.data.frame(pp2)
    |> filter(param == "kap")
    |> mutate(value = (z^2/2))
    |> select(focal, value)
)


## by design, mle profile evaluates/returns fewer values
## (intent is to use spline interpolation to find CIs more
##  efficiently)
plot(value ~ kap, data = dd)
with(dd2, lines(focal, value, col = 2, type = "b", pch=16, cex = 2))
abline(h=1.96, lty = 2)
abline(v= confint(pp), lty = 2)


confint(pp)
confint(pp2)
