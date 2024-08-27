############################################################
# notes:
# is there a singularity to deal with in the fractions so I get the ends?

###########################################################
# Set some params, this will eventually get moved to another script when we makify
R_0 <- 3
scaleRNum <- c(2, 3, 5)
xChoice <- c("low", "mid")
gamm <- 1/10
popSizee <- 1e4
tff <- 1e3


##############################################################
# load some packages
library(shellpipes)
manageConflicts()
startGraphics()
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

############################
# simulation stuff, probably not important
library(GillespieSSA) # I started writing simulation code but I think we don't want this approach.
# library(tictoc) # this was for timing simulation, I think we don't explicitly care rn tho.

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
    # solve for one reproductive number based on R_0
    lowR <- R_0/(low + mid*scaleRNum + high*scaleRNum^2)
    midR <- lowR * scaleRNum
    highR <- midR * scaleRNum

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

dat1 <- cmptMod(0.1, xChoice = "low", midRNum = midRNum, scaleRNum = scaleRNum[1])
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
    map_dfr(seq(0., 1, 0.02), function(x){
        dat <- cmptMod(
            x = x, xChoice = "low", midRNum = midRNum, scaleRNum = scaleR)
        R_0 <- R0(dat) # simple verification that I got R_0
        kappa <- compVar(dat)/R_0^2
        return(data.frame(eps = as.numeric(dat$pars[1])
                          , epsMethod = dat$pars[2]
                          , transmissionScaler = as.numeric(dat$pars[3])
                          , lowFrac = dat$fracs[1]
                          , midFrac = dat$fracs[2]
                          , highFrac = dat$fracs[3]
                          , R_0
                          , kappa ))
    })
})

kappaPlot <- emerge %>%
    ggplot(aes(highFrac, kappa, color = as.factor(transmissionScaler)))+
    geom_point()+
    theme_classic() +
    labs(x = "fraction of population in high-transmission group"
         , y ="kappa"
         , color = "ratio of\ntransmission\nbetween groups" )+
    scale_color_brewer(palette = "Dark2")

fractionPlot <- emerge %>%
    mutate(hf = highFrac) %>%
    pivot_longer(cols = ends_with("Frac"), names_to = "transLevel", values_to = "frac") %>%
    ggplot(aes(hf, frac
               , color = factor(transLevel
                                , levels = c("lowFrac", "midFrac", "highFrac")
                                , labels = c("low", "mid", "high")))) +
    geom_point() +
    theme_classic() +
    scale_color_manual(values = RColorBrewer::brewer.pal(9, "YlOrRd")[c(9, 8, 6)]) +
    # facet_grid(~transmissionScaler) +
    labs(x = "fraction of population in high-transmission group"
         , y = "fraction of population"
         , color = "transmission\nlevel")

# R0Plot <- emerge %>%
#     ggplot(aes(highFrac, R_0)) +
#     geom_point() +
#     theme_classic() +
#     facet_grid(~transmissionScaler) +
#     labs(x = "fraction of population in high-transmission group"
#          , y = "R0\n")

# pdf("kappas_in_3-class.pdf")
kappaPlot/fractionPlot
# dev.off()
#########################################################
vecMaker <- function(vec, name){
    x <- vec
    names(x) <- unlist(lapply(seq_len(length(vec)), function(i){paste0(name, i)}))
    return(x)
}

ssaMaker <- function(mod, gam = gamm, popSize= popSizee, tf = tff){
    U <- length(mod$fracs)
    # set parms
    betas <- mod$rNums*gam/popSize

    parms <- c(
        gamma = gam
        , vecMaker(mod$fracs, "p")
        , vecMaker(betas, "beta")


    )
    # set initial states
    # randomly seed one infection
    I0 <- rep(0, U)
    I0[sample(1:U,1)]<-1

    x0 <- c(S = popSize -1, vecMaker(I0, "I"), R = 0)

    a <- unlist(lapply(seq_len(U), function(grp){
        c(paste0("p",grp, " * S * ("
                 , paste0("beta1*I1"
                          , paste0(c(unlist(lapply(seq_len(U-1), function(ind){
                     paste0(" + beta", ind+1, " * I", ind+1)
                     }))), collapse =""), ")")
                 ) # infection
        , paste0("I", grp, " * gamma") # recovery
        )

    }))

    # define state change matrix for each S, I, and R (rows) for each reaction
    # (columns)

    # This is super ugly, make it programmatic later
    nu <- matrix(c(-1,  0,  -1, 0, -1, 0  #changes in S
                   , +1, -1, rep(0,4) # I1
                 , 0, 0, +1, -1, 0, 0 # I2
                 , rep(0, 4), +1, -1 # I3
                 , rep(c(0, +1), 3)
                 )
                 , nrow = 5
                 , byrow=TRUE)
    out <- list(x0 = x0, a = a, nu = nu, parms = parms, tf = tf)
    return(out)
}

firstSSAList <- ssaMaker(mod = dat1)
# tictoc::tic()
firstSim <- do.call(ssa, firstSSAList)
# tictoc::toc()

# ssa.plot(firstSim)

firstSim$data %>%
    data.frame() %>%
    pivot_longer(
    cols = 2:6, names_to = "pop", values_to = "individuals"
) %>%
    ggplot(aes(t, individuals, color = pop)) +
    geom_point() +
    theme_classic()
