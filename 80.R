set.seed(1000)

n <- 1000

big <- sort(rgeom(n, 0.1), decreasing=TRUE)
small <- sort(rgeom(n, 0.7), decreasing=TRUE)

cbig <- cumsum(big)/sum(big)
csmall <- cumsum(small)/sum(small)

q <- (1:n)/n

plot(q, cbig, type="l")
lines(q, csmall, col="blue")

