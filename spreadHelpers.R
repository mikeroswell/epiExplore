# a few little functions to make superspreading stuff easier

library(shellpipes)
manageConflicts()


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


makeDistData <- function(v){
    n <- length(v)
    realiz <- rpois(n, lambda = v)
    cFIdeal <- cumFrac(v)
    cFRealiz <- cumFrac(realiz)
    q  <- (1:n)/n
    qp <- (1:n)/sum(realiz>0)
    return(data.frame(q, qp, cFIdeal, cFRealiz))
}



# get t20 or tX
tX <- function(DD, X, q = "q", cF = "cFIdeal"){
    DD[which(DD[, eval(q)]>=X)[1], eval(cF)]
}


# compute Gini
Gini <- function(v){
    dst <- c(abs(outer(v,v, "-")))
    nrm <- 2*length(v)*sum(v)
    return(sum(dst)/nrm)
}


# Dushoff Kappas (note one for continuous, the other for poissonified)
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


# Wrapper to provide summary statistics based on data
ehm <- function(dat){
    kapCont <- ctsKappa(dat$ideal)
    kapDisc <- discKappa(dat$real)
    tx20Cont <- tX(dat, 0.2)
    tx20Disc <- tX(dat, 0.2, cF = "cFRealiz")
    R0Ideal <- mean(dat$ideal)
    R0Real <- mean(dat$real)
    return(data.frame(kapCont, kapDisc, tx20Cont, tx20Disc, R0Ideal, R0Real))
}

saveEnvironment()