# we need to write down a likelihood for kappa given x.
library(shellpipes)
manageConflicts()
library(bbmle)
# ?dgamma
# Let's write down dnbinom and dgamma in terms of mean, variance
dgammaMV <- function(x, M, V, ...){
  gamScale <- V/M
  gamShape <- M/gamScale
  dgamma(x, shape = gamShape, scale = gamScale, ...)
}

x <- rpois(1000, rexp(1000, 1/2)) # M = 2, V = 6

nbNLL <- function(x, M, V){
  -sum(dnbinomMV(x, M, V, log = TRUE))
}

dnbinomMV <- function(x, M, V, ...){
  k <- M^2/(V-M)
  dnbinom(x, mu = M, size = k, ...)
}

binomMLE <- bbmle::mle2(nbNLL, start = list(M =1, V = 3), data = list(x = x))
summary(binomMLE) # looks fine


kapNB <- function(x, kap, M){
  -sum(dnbinom(x, mu = M, size = 1/kap, log = TRUE))

}


kapMLE <- bbmle::mle2(kapNB
                      , start = list(kap = 0.5, M = 2)
                      # , lower = c(kap = 0, M = 1e-4)
                      , data = list(x = x)
                       # , method = "SANN"
                      )


summary(kapMLE) # keeps estimating M = x, when I specify lower(M=x)

profile(kapMLE)

# y <- rnbinom(1e3, size =1, mu = 2)
# hist(x)
# plot(y[order(y)], x[order(x)])
