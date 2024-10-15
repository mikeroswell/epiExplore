library(dplyr)
library(ggplot2); theme_set(theme_classic())

nSamp <- 1e4
nl <- 51
lmax <- 40

p <- ((1:nSamp)-1/2)/nSamp
lam <- seq(0, lmax, length.out=nl)

cvLam0 <- function(q, lam){
	wt <- exp(-lam*q)
	c0 <- mean(wt)
	c1 <- mean(q*wt)
	c2 <- mean(q*q*wt)
	m = c1/c0
	v <- c2/c0 - m^2
	return(c(
		n = c0
		, m = m
		, v = v
		, kap = v/m^2
	))
}

cvLam <- Vectorize(cvLam0, "lam")

cvPlot <- function(q, lam){
	f <- (as.data.frame(t(cvLam(q, lam)))
		|> mutate(effHaz  = -log(n))
	)
	print(f)
	print(ggplot(f)
		+ aes(effHaz, kap)
		+ geom_line()
	)
}

mu <- 2
kap <- 0.4

cvPlot(qgamma(p, scale=kap*mu, shape=1/kap), lam)
cvPlot(qbeta(p, shape1=2, shape2=2), lam)
cvPlot(qbeta(p, shape1=0.5, shape2=0.5), lam)
cvPlot(qlnorm(p, meanlog=log(0.4), sdlog=1), lam)
cvPlot(qlnorm(p, meanlog=log(0.4), sdlog=0.5), lam)
cvPlot(qnorm(p, mean=0.4, sd=0.04), lam)
cvPlot(qnorm(p, mean=4, sd=0.04), lam)
