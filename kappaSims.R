
ctsKappa <- function(v){
	mu <- mean(v)
	V <- mean((v-mu)^2)
	return(V/mu^2)
}

discKappa <- function(v){
	mu <- mean(v)
	V <- mean((v-mu)^2)
	return((V-mu)/mu^2)
}

kComp <- function(a){
	r <- rpois(length(a), lambda=a)
	return(c(cκ = ctsKappa(a), unadj=ctsKappa(r), dκ = discKappa(r)))
}

n <- 1e5
set.seed(1410)
print(kComp(rlnorm(n, meanlog=3, sdlog=0.5)))
print(kComp(rlnorm(n, meanlog=3, sdlog=1)))
print(kComp(rlnorm(n, meanlog=3, sdlog=2)))

print(kComp(rgamma(n, shape=0.5)))
print(kComp(rgamma(n, shape=1.0)))
print(kComp(rgamma(n, shape=2.0)))
