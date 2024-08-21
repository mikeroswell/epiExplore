###########################################################
# Set some params, this will eventually get moved to another script when we makify
midRNum <- 3
scaleRNum <- c(2, 3, 10)
xChoice <- c("low", "mid")

##############################################################
library(shellpipes)
manageConflicts()
startGraphics()

# load some packages
library(dplyr)
library(purrr)
library(ggplot2)

#####################################################
# create some functions to help out here
cmptMod <- function(x, xChoice = c("low", "mid"), midRNum, scaleRNum){
    # define probabilities for each of three compartments
    if(xChoice == "mid"){
        eps <- max((-sqrt((x-1)^2-4*x^2)+(1-x))/(2*x)
                  , (sqrt((x-1)^2-4*x^2)+(1-x))/(2*x))
        low <- x/eps
        mid <- x
        high <- x*eps
    }
    if(xChoice == "low"){
        eps <- max((-sqrt(-3*(x)^2+4*x)-x)/(2*x)
                   , (sqrt(-3*(x)^2+4*x)-x)/(2*x) )
        low <- x
        mid <- x*eps
        high <- x*eps^2
    }
    # compute the reproductive numbers for the three compartments
    lowR <- midRNum/scaleRNum
    midR <- midRNum
    highR <- midRNum * scaleRNum
    
    return(list(fracs = c(low, mid, high), pars = c(eps
                      , xChoice,  scaleRNum), rNums = c(lowR, midR, highR)))
}

# Note to self: Right now, this is not at all set up to have fixed R0, but might 
# be able to do a small amt of math to get there (i.e., we rescale)

# compute emergent heterogeneity at driver level
# assuming here no serial compartments, meanR = R for each group
R0 <- function(dat){
    with(dat, sum(fracs*rNums))
         }

# dat1 <- cmptMod(0.1, xChoice = "low", midRNum = midRNum, scaleRNum = scaleRNum[1])
# dat1 <- list(fracs = 1, rNums = 3)
# dat1 <- list(fracs = c(1/3, 1/3, 1/3), rNums = c(3,3,3))
# 
# R0(dat1)

compVar <- function(dat){
    (with(dat, sum(fracs*2*rNums^2)) # This is only true if there is only one 
     # infectious compartment per group and no exposed class(es)
    - with(dat, sum(fracs*rNums)^2)
    )
}

# compVar (dat1)

#########################################
# do a pile of simulations

emerge <- map_dfr(scaleRNum, function(scaleR){
    map_dfr(seq(0.05, 1, 0.05), function(x){
        dat <- cmptMod(
            x = x, xChoice = "low", midRNum = midRNum, scaleRNum = scaleR)
        R_0 <- R0(dat)
        kappa <- compVar(dat)/R_0^2
        return(data.frame(eps = as.numeric(dat$pars[1]), epsMethod = dat$pars[2]
                          , transmissionScaler = as.numeric(dat$pars[3])
                          , lowFrac = dat$fracs[1]
                          , midFrac = dat$fracs[2]
                          , highFrac = dat$fracs[3]
                          , R_0, kappa ))
    })
})

kappaPlot <- emerge %>% 
    ggplot(aes(highFrac, kappa, color = R_0))+
    geom_point()+
    facet_grid(~transmissionScaler) +
    theme_classic() +
    labs(x = "fraction of population in high-transmission group", y ="kappa \n")

fractionPlot <- emerge %>%
    ggplot(aes(highFrac, lowFrac)) +
    geom_point() +
    geom_point(aes(y = midFrac), color = "red") + 
    theme_classic() +
    facet_grid(~transmissionScaler) +
    labs(x = "fraction of population in high-transmission group"
         , y = "fraction in low-transmission group (black)\nmid-transmission (red)")

R0Plot <- emerge %>%
    ggplot(aes(highFrac, R_0)) +
    geom_point() +
    theme_classic() +
    facet_grid(~transmissionScaler) +
    labs(x = "fraction of population in high-transmission group"
         , y = "R0\n")

library(patchwork)

kappaPlot/R0Plot/fractionPlot
