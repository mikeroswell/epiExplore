# do some lloyd-smith stuff

# load libraries
library(dplyr)
library(purrr)
library(ggplot2)

# set some integer population size
n <- 1e4

# set the dispersion parameter JD-style such that the limiting case of 0 gets us 
# to poisson
JDDisp <- c(exp(seq(-4, 4, 1)))


R0 <- seq(0.5, 3, 0.25) # this is the rate parameter for an exponential 
# aka Beta over Gamma where Beta and Gamma are the SIR rates:
# dI/dt = (Beta*I*S)/N - Gamma*I
# dR/dt = Gamma*I

makeP <- function(rate){rate/(1+rate)} # this is the probability for a geometric
# when rate = R0, equal to Beta/(Beta + Gamma), an infected's probability of 
# recovery


cfracs <- map_dfr(R0, function(brn){
    map_dfr(JDDisp, function(ishape){
        # First, parameterize an underlying thing (gamma, where 
        # ishape = shape = 1 is exponential)
        contin <- rgamma(n, shape = 1/ishape, scale = brn*ishape) # this latent 
        #distribution is the idealized/expected individual-level reproductive
        #rate. The base case (ishape = 1) is based on imagining individuals
        #falling into the the system described by the canonical ODE SIR model:
        #constant per-capita recovery rate & no individual-level variation in
        #transmission rate either.
        # Lloyd-Smith calls this the "individual reproductive number, *v*"
        realiz <- rpois(n, lambda = contin) # But these expected, real-numbered 
        #reproductive rate is unrealistic. Instead, the actual (integer) number
        #of new infections is assumed to be a random(i.e., Poisson) sample from
        #that idealized rate
        
        # verify my parameterization
        alt <-  rnbinom(n, prob = (1/ishape)/((1/ishape)+brn), size = 1/ishape)
        print(paste0(
            "R0_alt = "
            , mean(alt, na.rm = TRUE)
            , ", R0_sim = "
            , mean(realiz, na.rm = TRUE)
            , ", R0 = "
            , brn ))
        # Count in the "80/20 rule paradigm"
        cumFrac = cumsum(sort(realiz, decreasing = TRUE))/sum(realiz)
        q = (1:n)/n
        return(data.frame(q, cumFrac, brn, ishape))
    })
})

cfracs %>% 
    ggplot(aes(q, cumFrac, color = log(brn))) + 
    facet_wrap(~round(ishape,2)) +
    geom_point(size = 0.1) + 
    # geom_line(aes(linewidth = 0.05)) + 
    theme_classic() +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    scale_color_viridis_c() +
    labs(x = "fraction contributing"
         , y = "fraction of new infections"
         , color = "log(R0)"
         , title = paste0("panels by dispersion (inverse shape parameter):\n1 ",
         "is exponential/geometric\n0 is  Dirac/Poisson\nInf is way dispersed"))

# Comments: When R0 is low, a small fraction of the population transmits a
# majority of the infections. This is highly expected: For example, if 60% of
# the population infects nobody, the other 40% has to do all of it.

# With Higher R0, there is a flattening @ low dispersion and a sharpening only 
# as dispersion gets larger.

# In some sense, the assumption of the SIR model is that transmission occurs at
# a constant rate for all individuals, and thus the intrinsic tendency towards
# superspreading is nonexistent regardless of R0. But the implied exponential
# distribution of recovery times means that some individuals end up doing more
# or less transmission... in the continuous ODE model these individuals aren't
# really "there" though.

# I think to generalize, we could estimate different recovery time distributions
# from more complex models? Are there other model features/parameters we'd be 
# keen to dig into? 


    
    
    

