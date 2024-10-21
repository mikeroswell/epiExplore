library(shellpipes)
rpcall("nbinom_z.Rout nbinom_z.R")
manageConflicts()
startGraphics()
library(bbmle)
library(nloptr)
library(stringr)
library(numDeriv)

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

dnbinom_cv <- function(x, mu, cv, ...) {
    return(dnbinom(x, mu = mu, size = 1/cv^2, ...))
}

# make the fit a function
fit_kappaNB <- function(dd, z = "z"){
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

coef(fit)

######################################################################

## This could be deleted if it's in anyone's way, see quadraticNotes.md

fit_cv <- function(dd, z = z){
  mle2(z ~ dnbinom_cv(exp(logmu), cv),
     data = dd,
     start = list(logmu = 0, cv = 0)
)}



fit <- fit_cv(dd, z)

## estimated std error?
sqrt(diag(solve(h1 <- fit@details$hessian)))
ml <- function(p) do.call(fit@minuslogl, as.list(p))
ml(coef(fit))
h2 <- optimHess(coef(fit), ml)
print(se2 <- sqrt(diag(solve(h2))))  ## much more reasonable

## internal Hessian calculation uses
## numDeriv::hessian [Richardson extrapolation] -- much worse in this case ... ???
h3 <- hessian(ml, coef(fit))
all.equal(h1, h3, tolerance = 0)

## str(fit)
p1 <- profile(fit)
p2 <- profile(fit, std.err = se2)
confint(fit)
confint(p2)
print(as.data.frame(profile(fit)))

## Hessian-based profile for some reason too narrow
print(
    lattice::xyplot(z^2 ~ focal|param
	 , data = as.data.frame(p1),
	 , scales = list(x = list(relation = "free")))
)

## Manual std.err
print(
    lattice::xyplot(z^2 ~ focal|param
	 , data = as.data.frame(p2),
	 , scales = list(x = list(relation = "free")))
)

## so what went wrong with the internal Hessian calculation ... ???

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



# rather than cutting out the functions here just use this script in a makerule:x
saveEnvironment()

