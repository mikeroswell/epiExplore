# a few little functions to make superspreading stuff easier

# normalize
nrmlz <- function(x){x/sum(x, na.rm = TRUE)}

# cumulative fraction
cumFrac <- function(dist){
    cumsum(sort(dist, decreasing = TRUE))/sum(dist, na.rm = TRUE)
}

# odds to probability, i.e. the way to think about the probability parameters 
# for a geometric distribution and its relationship to the rate parameter of an
# exponential (or, more generally, a gamma) 
# W.R.T. ODE SIR models, this function converts from rate = R0 to an infected's 
# probability of recovery, Beta/(Beta + Gamma) -- where Beta is the transmission  
# rate and Gamma the recovery rate. )

makeP <- function(rate){rate/(1+rate)} 

# and the inverse:
makeRate <- function(p){
    p/(1-p)
}
