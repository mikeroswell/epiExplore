# we need to write down a likelihood for kappa given x.
library(bbmle)
?dgamma
# Let's write down dnbinom and dgamma in terms of mean, variance
dgammaMV <- function(x, M, V, ...){
  gamScale <- V/M
  gamShape <- M/gamScale
  dgamma(x, shape = gamShape, scale = gamScale, ...)
}

dnbinomMV <- function(x, M, V, ...){
  k <- M^2/(V-M)
  dnbinom(x, mu = M, size = k, ...)
}

x <- rpois(1000, rexp(1000, 1/2)) # M = 2, V = 6

nbNLL <- function(x, M, V){
  -sum(dnbinomMV(x, M, V, log = TRUE))
}

binomMLE <- bbmle::mle2(nbNLL, start = list(M =1, V = 3), data = list(x = x))
summary(binomMLE)
profile(binomMLE)

# ok, but I want to estimate kappa, which is actually very closely related to
# size param

kapNB <- function(x, kap, M){
  -sum(dnbinom(x, mu = M, size = 1/kap))

}
kapMLE <- bbmle::mle2(kapNB, start = list(M = 2, kap = 0.5), data = list(x = x), method = "Nelder-Mead")

summary(kapMLE)

profile(kapMLE)

y <- rnbinom(1e3, size =1, mu = 2)
hist(x)
plot(y[order(y)], x[order(x)])
