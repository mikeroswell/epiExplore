popSize <- 5
mixProp <- qgamma(p = ((1:popSize) - 1/2)/popSize, shape = 2.2)

mat <- outer(mixProp, mixProp)

print(mat)

contactProb <- 2*mat/sum(mat)

contactProb[lower.tri(contactProb, diag = TRUE)] <- 0

print(contactProb)


