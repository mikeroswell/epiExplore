
a <- 5

V <- seq(0, 2, by=0.01)
V_adj <- V/(1+V^a)^(1/a)

plot(V, V_adj)
lines(V,V)

