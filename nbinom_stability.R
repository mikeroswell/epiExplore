library(RTMB)
library(DPQ) ## for logspace.add

## test extreme value to make sure we have the version of TMB with dnbinom_robust fixed
fixtmb <- function(x) Im(unclass(x))
## **not** robust with x=3, mu=3
x <- 1; mu <- 1; size <- 1e15

calc0 <- function(x=1, mu=1, size=1e15) {
    d1 <- dnbinom(x, size = size, mu = mu, log = TRUE)
    ## var-mu = mu^2/size
    d2 <- RTMB::dnbinom_robust(x, log_var_minus_mu = 2*log(mu)-log(size), log_mu = log(mu), log = TRUE) |> fixtmb()
    return(c(d1,d2))
}

print(res0 <- abs(diff(calc0())))

if (res0 > 0.1) {
    stop("consider \"library(remotes); install_github('kaskr/adcomp/TMB', ref = github_pull(402)); install.packages('RTMB', type = 'source')\"")
}

## uh-oh
calc0(x=3, mu = 3)

## why/where do we run into trouble with x=3, mu=3 while x=1, mu=1 is OK?
## x=1, mu=1, size=1e15 → log_var_minus_mu = -34.53878, log_mu = 0
## x=3, mu=3, size=1e15 → log_var_minus_mu = -32.34155, log_mu = 1.098612

do_calc <- function(x, mu, size) {
    log_mu <- log(mu)
    log_var_minus_mu <- 2*log_mu - log(size)
    cat(sprintf("log_mu = %1.5g, log_var_minus_mu = %1.5g\n", log_mu, log_var_minus_mu))
    log_var <- DPQ::logspace.add( log_mu, log_var_minus_mu )
    log_p   <-     log_mu - log_var
    log_n   <- 2 * log_mu - log_var_minus_mu
    cat(sprintf("log_var=%1.5g, log_p=%1.5g, log_n=%1.5g\n",
                log_var, log_p, log_n))
    n <- exp(log_n)
    ## vv large * vv small value; can we avoid this/take a limit?
    logres <- n * log_p
    log_1mp <- log_var_minus_mu - log_var;
    cat(sprintf("n=%1.5g, logres0=%1.5g, log_1mp=%1.5g\n",
                n, logres, log_1mp))
    logres <- logres - lbeta(n, x) - log(x) + x * log_1mp
    return(logres)
}

## n*log_p = exp(2*log_mu-log_var_minus_mu)*(log_mu-log_var)
##  = exp(2*log_mu-log_var_minus_mu + log(log_mu-log_var))
do_calc(1, 1, 1e15)
dnbinom(x=1, mu=1, size=1e15, log=TRUE)
do_calc(3, 3, 1e15)
dnbinom(x=3, mu=3, size=1e15, log=TRUE)

### Testing RTMB's functions as theta→ infty

calc_fun <- function(x = 1, mu = 1) {
    var_minus_mu <- 10^seq(-20, 0, length = 101)
    ## var = mu*(1+mu/theta) -> var_minus_mu = mu^2/theta -> theta = mu^2/var_minus_mu
    thetavec <- mu^2/var_minus_mu
    varvec <- mu + var_minus_mu ## will underflow ...


    v1 <- dnbinom(x = x, mu = mu, size = thetavec, log = TRUE)
    v2 <- RTMB::dnbinom2(x = x, mu = mu, var = varvec, log = TRUE) |> fixtmb()
    v3 <- RTMB::dnbinom_robust(x = x, log_mu = log(mu),
                               log_var_minus_mu = log(var_minus_mu),
                               log = TRUE) |> fixtmb()
    res <- list(theta = thetavec, dnbinom = v1, dnbinom2 = v2,
                dnbinom_robust = v3, dpois = dpois(x, mu, log = TRUE))
    return(res)
}

plot_fun <- function(res) {
    par(las = 1, bty = "l", mfrow = c(1,2))
    matplot(res$theta, do.call(cbind, res[2:4]),
            type = "l", log = "x",
            ylim = c(-2, 0), lwd = 2, col = c(1,2,4),
            xlab = "theta",
            ylab = "log LL")
    abline(h = res$dpois, lty = 2)
    with(res, matplot(theta, cbind(abs(dnbinom2-dnbinom), abs(dnbinom_robust-dnbinom)),
                        type = "l", log = "xy",
                        lwd = 2, col = c(1,2),
                        xlab = "theta",
                        ylab = "diff from stats::dnbinom"))
    invisible(NULL)
}

c1 <- calc_fun(mu = 1, x = 1)
c3 <- calc_fun(mu = 3, x = 3)

plot_fun(c1)
plot_fun(c3)

