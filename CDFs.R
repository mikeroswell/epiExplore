# muck around with the r p q d functions

?pgeom
plot((0:40)/40, pgeom(0.2, (0:40)/40, lower.tail = TRUE))
plot((0:40)/40, pnorm(0.2, mean = 1, sd = (0:40)/40))

abline(a = 0, b = 1)
dev.off()
purrr::map((1:10)/10, function(p){
    hist(rgeom(100, p), xlim = c(0, 50), main = p)
    
})

plot(seq(0, 1, 0.01), pgeom(seq(0, 1, 0.01), 0.2))
plot(seq(0, 1, 0.01), dgeom(seq(0, 1, 0.01), 0.9))

maxK <- 1
curve(pexp, from = maxK, to = 0)
plot(ecdf(rgeom(500, prob = 0.2)))


