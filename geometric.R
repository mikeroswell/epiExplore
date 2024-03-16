
n <- 1e3
p <- 0.2

nrgeom <- function(n, p){
	lam <- rexp(n, p/(1-p))
	return(rpois(n, lam))
}

g <- rgeom(n, p)
ng <- nrgeom(n, p)

print(g)

plot(sort(g), sort(ng)
	, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1)
	, pch = 16, cex = 2
)
abline(0,1)
