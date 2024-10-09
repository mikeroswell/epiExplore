library(shellpipes)
manageConflicts()
startGraphics()
library(bbmle)
library(nloptr)
library(stringr)
library(RTMB)
library(tinyplot)  ## felt like playing with this

if (packageVersion("bbmle") <= "1.0.25.1") {
    stop("please install later version of bbmle via remotes::install_github('bbolker/bbmle')")
}

rpcall("nbinom_z.Rout nbinom_z.pipestar nbinom_z.R")

def_control <-
    list(algorithm = "NLOPT_LN_BOBYQA", xtol_abs = 1e-08,
         ftol_abs = 1e-08,
         maxeval = 1e+05)

repl <- function(vals, default) {
    for (n in names(default)) {
        vals[[n]] <- vals[[n]] %||%  default[[n]]
    }
    return(vals)
}

## wrapper function to mimic optim
## modified from lme4::nloptwrap
nloptwrap <- function (par, fn, gr = NULL,
                       method,  ## ignored ... for compatibility with optim
                       ...,
                       lower = -Inf,
                       upper = Inf, control = list(), hessian = FALSE) {
    if (hessian) stop("hessian not supported")
    control <- repl(control, def_control)
    res <- nloptr(x0 = par
                  , eval_f = fn
                  , eval_grad_f = gr
                  , lb = lower
                  , ub = upper
                  , opts = control
                  , ...)
    with(res, list(par = solution, value = objective,
                   counts = c(iterations, NA),
                   convergence = if (status < 0 || status == 5) status else 0,
        message = message))
}

set.seed(101)
dd <- data.frame(z = rpois(100, 2))
dnbinom_kappa <- function(x, mu, kappa, ...) {
    return(dnbinom(x, mu = mu, size = 1/kappa, ...))
}

# make the fit a fuction
fit_kappaNB <- function(dd, z = z){
  mle2(z ~ dnbinom_kappa(exp(logmu), kappa),
     data = dd,
     skip.hessian = TRUE,
     lower = c(logmu = -Inf, kappa = 0),
     upper = c(logmu = Inf, kappa = Inf),
     start = list(logmu = 0, kappa = 1),
     optimizer = "user",
     optimfun = nloptwrap,
     control = list(algorithm = "NLOPT_LN_BOBYQA"))
}

fit <- fit_kappaNB(dd, z)

# fit <- mle2(z ~ dnbinom_kappa(exp(logmu), kappa),
#             data = dd,
#             skip.hessian = TRUE,
#             lower = c(logmu = -Inf, kappa = 0),
#             upper = c(logmu = Inf, kappa = Inf),
#             start = list(logmu = 0, kappa = 1),
#             optimizer = "user",
#             optimfun = nloptwrap,
#             control = list(algorithm = "NLOPT_LN_BOBYQA"))

coef(fit)

########################################
# MR attempt to get CI with issues:

# but can I get CI for the fit?
# I can't get a profile using `profile`
# profile(fit) # issue with no Hessian
p0 <- profile(fit, std.err = c(0.01, 0.01))
p0
# but why do we get NA for the kappa lcl?
confint(p0, method = "uniroot")
########################################
# BB reply drafted during push/pull
## options for getting CIs


## ugh, debugging S4 methods ...
## trace("confint", sig="mle2", browser)

## confint is set to NA when we don't achieve the specified confidence level at
## the bounds; it's up to the user to decide whether to set these NA values equal
## to the bounds or not

## when Hessian is ill-behaved, need to specify a standard error (or vector
## of standard errors); availability for 'uniroot' is new
c1 <- confint(fit, method = "uniroot", std.err=0.1)
c1
## deliberately set std.err smaller to get high-resolution profile for plotting
p0 <- profile(fit, std.err = 0.01)
c2 <- confint(p0)  ## default profile methods
c2
all.equal(c1, c2, tolerance = 2e-5)

dd <- as.data.frame(p0)
## should use ggplot or tinyplot?
print(
    lattice::xyplot(z^2 ~ focal|param, data = dd,
                    scales = list(x = list(relation = "free")))
)

# confint(fit)
## can't use gradient-based methods in nloptr without explicitly
## specifying the gradient function ...

## fit2 <- update(fit, control = list(algorithm = "NLOPT_LD_LBFGS"))

## FIXME/TODO:
# * document 'optimizer = "user"' in mle2 (!)
## * make nloptwrap nicer: set lower/upper if the other is set
##   (nloptr requires both to be set if either is)
## * 'method' -> control$algorithm ?
## * translate method = "BFGS" to NLOPT_LD_LBFGS (closest equivalent),
##   with warning?
## * create an RTMB wrapper for R's dnbinom (more robust)? Not necessarily
##   easy because we don't have a similarly robust implementation of the
##   gradient handy ...

## https://github.com/astamm/nloptr/blob/6d4943aff5a47bd3b3914f86acaa5d6eeeccaa77/R/is.nloptr.R#L65-L79
list_algorithms <- c(
  "NLOPT_GN_DIRECT", "NLOPT_GN_DIRECT_L", "NLOPT_GN_DIRECT_L_RAND",
  "NLOPT_GN_DIRECT_NOSCAL", "NLOPT_GN_DIRECT_L_NOSCAL",
  "NLOPT_GN_DIRECT_L_RAND_NOSCAL", "NLOPT_GN_ORIG_DIRECT",
  "NLOPT_GN_ORIG_DIRECT_L", "NLOPT_GD_STOGO", "NLOPT_GD_STOGO_RAND",
  "NLOPT_LD_SLSQP", "NLOPT_LD_LBFGS_NOCEDAL", "NLOPT_LD_LBFGS",
  "NLOPT_LN_PRAXIS", "NLOPT_LD_VAR1", "NLOPT_LD_VAR2", "NLOPT_LD_TNEWTON",
  "NLOPT_LD_TNEWTON_RESTART", "NLOPT_LD_TNEWTON_PRECOND",
  "NLOPT_LD_TNEWTON_PRECOND_RESTART", "NLOPT_GN_CRS2_LM", "NLOPT_GN_MLSL",
  "NLOPT_GD_MLSL", "NLOPT_GN_MLSL_LDS", "NLOPT_GD_MLSL_LDS", "NLOPT_LD_MMA",
  "NLOPT_LD_CCSAQ", "NLOPT_LN_COBYLA", "NLOPT_LN_NEWUOA",
  "NLOPT_LN_NEWUOA_BOUND", "NLOPT_LN_NELDERMEAD", "NLOPT_LN_SBPLX",
  "NLOPT_LN_AUGLAG", "NLOPT_LD_AUGLAG", "NLOPT_LN_AUGLAG_EQ",
  "NLOPT_LD_AUGLAG_EQ", "NLOPT_LN_BOBYQA", "NLOPT_GN_ISRES", "NLOPT_GN_ESCH"
)
stringr::str_extract(list_algorithms, "(?<=_)[^_]+(?=_)") |> table()

## can't get this to work with gsub ...
## gsub("[^_]*((?<=_)[^_]+(?=_).*$)", "\\1", list_algorithms, perl = TRUE) |> table()

## L = local, G = global,
## N= derivative-free, D = derivative-dependent




### part 2. Testing RTMB's functions as theta→ infty

## MR saw some numerical instability in nbinom2
# # underdispersed NB likelihood
# n <- 50
# x <- rep(3, 50)
#
# RTMB::dnbinom2(3, 3, 3+1e-9, log = TRUE)
# RTMB::dnbinom2(3, 3, 3+1e-11, log = TRUE)
# RTMB::dnbinom2(3, 3, 3+1e-12, log = TRUE)
# RTMB::dnbinom2(3, 3, 3+1e-13, log = TRUE)
# RTMB::dnbinom2(3, 3, 3+1e-14, log = TRUE)
# RTMB::dnbinom2(3, 3, 3, log = TRUE)

x <- 1; mu <- 1
var_minus_mu <- 10^seq(-20, 0, length = 101)
## var = mu*(1+mu/theta) -> var_minus_mu = mu^2/theta -> theta = mu^2/var_minus_mu
thetavec <- mu^2/var_minus_mu
varvec <- mu + var_minus_mu ## will underflow ...

fixtmb <- function(x) Im(unclass(x))
v1 <- dnbinom(x = x, mu = mu, size = thetavec, log = TRUE)
v2 <- RTMB::dnbinom2(x = x, mu = mu, var = varvec, log = TRUE) |> fixtmb()
v3 <- RTMB::dnbinom_robust(x = x, log_mu = log(mu),
                           log_var_minus_mu = log(var_minus_mu),
                           log = TRUE) |> fixtmb()

par(las = 1, bty = "l")
matplot(thetavec, cbind(v1, v2, v3), type = "l", log = "x",
        ylim = c(-2, 0), lwd = 2, col = c(1,2,4),
        ylab = "log LL")
abline(h = dpois(1, 1, log = TRUE), lty = 2)

comb <- Map(\(nll, nm) data.frame(nm, theta = thetavec, nll),
    list(v1, v2, v3),
    c("dnbinom", "RTMB::dnbinom2", "RTMB::dnbinom_robust")) |>
    do.call(what=rbind)

## not sure why color spec isn't working?
# Are those colorblind-friendly colors?
tinyplot(nll ~ theta | nm, data = comb, type = "l",
         log = "x", lwd = 2,
         col = c(1,2,4),
         ylim = c(-2, 0))

# rather than cutting out the functions here just use this script in a makerule:x
saveEnvironment()

