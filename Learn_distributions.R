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

# no, I didn't get the probability backwards, but also the distributions that I 
# thought would be the same have drastically different means.

# visualize with a q-q plot
my_quantl <- function(x){quantile(log(x), probs = seq(0.001,0.999,0.001))}
plot(my_quantl(my_geom_devs), my_quantl(r_geom_devs))
abline(0,1)
# oh no