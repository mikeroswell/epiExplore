## Learning distributions
# Goals: 1) generate geometric as a Poisson - exponential mixture
# 2) generate gamma (or Erlang, what is the special case here? ) as sum or mean
# of exponential distributions 
# 3) generate negative binomial as a Poisson - gamma (or Poisson - Erlang?)
# mixture


### here, lambda is the rate (i.e., mean and variance)
### k is resulting count
## probability density of X = k
poiFun <- function(lambda, k){
    lambda^k*exp(-lambda)/factorial(k)
}
# Check that I did it right
all.equal(poiFun(2, 5), dpois(5, 2))


#### lambda is the rate (sometimes people use 1/lambda called "scale")
### x is a non-negative real value
# probability density of X = x
expFun <- function(lambda, x){
    ifelse(x==0, 0, lambda * exp(-lambda * x))
}
#test
all.equal(expFun(lambda = 5, x = 2), dexp(2, 5))


## make a geometric
# probability density X = k
geomFun <- function(p, k){
    (1-p)^k*p
}

# verify

all.equal(geomFun(0.2, 5), dgeom(5, 0.2))

# make a geometric as a mixture of poisson and exponential
# maybe it will help to have the same parameters!
# make the exponential rate w.r.t. a probability p
# is this correct?
makeRate <- function(p){
    p/(1-p)
}


# ok, test that random deviates from my mixture are geometrically distributed
# focus on a probability p
# make a q-q plot

p <- 0.2
reps <- 5000
my_geom_devs <- rpois(reps
                      , lambda = rexp(reps
                                      , rate = makeRate(p)) )



r_geom_devs <- rgeom(reps, p)
# what if I just have my p and 1-p backwards?
rgd2 <- rgeom(reps, 1-p)

summary(r_geom_devs)
summary(my_geom_devs)
summary(rgd2)

# no, I didn't get the probability backwards (but watch out, scale = 1 /rate)

# visualize with a q-q plot
my_quantl <- function(x){quantile(log(x), probs = seq(0.001,0.999,0.001))}
plot(my_quantl(my_geom_devs), my_quantl(r_geom_devs))
abline(0,1)
# after exploration, decided they are nearly the same but something technical 
# means not entirely. 
# https://github.com/wch/r-source/blob/f46b4cd47b4f4b2bb5ef40137a7f48f91b66469e/src/nmath/rgeom.c#L45

# Ok, now on to gamma from exponential (easy to go the other way)
# maybe go by way of the Erlang (gamma with integer shape parameter)
myErlang <- function(n, p, r){
    apply(replicate(r, rexp(n = reps, makeRate(p))), 1, sum)
}
Erlang <- rgamma(reps, rate = makeRate(p), shape = 2)

plot(sort(myErlang(reps, p, 2)), sort(Erlang))

# how to assemble a gamma more generally?
### Skipping that for now...

# Sample the Erlang to make a negative binomial with an integer shape parameter
# (a "Pascal" distribution).

myPascal <- function(n, p, r){
    rpois(n, lambda = 
              myErlang(n, p, r))
}


# test

# pdf("propagates_to_nbinom.pdf")
plot(sort(myPascal(reps, p, 2)), sort(rnbinom(reps, size = r, prob = p))
     , col = rgb(red = 0, green = 0, blue = 0, alpha = 0.01)
     , pch = 16, cex =0.5)
# dev.off()

# at this point I feel fine about the code, noting the buggy-seeming deviations 
# in the q-q plots. 
