############################################################
# notes:
# is there a singularity to deal with in the fractions so I get the ends?



##############################################################
# load some packages
library(shellpipes)
rpcall("kappas_in_3-class.Rout kappas_in_3-class.R")
manageConflicts()
# startGraphics()
library(dplyr)
library(purrr)
library(tidyr)
# library(ggplot2)
# library(patchwork)

############################

###########################################################
# Set some params, this will eventually get moved to another script when we makify
R_0 <- 3
scaleRNum <- c(2, 3, 5) # we pre-specify the ratio of infectiousness between mid/low/high
xChoice <- c("low", "mid") # this is a way to specify different rules for how the fractions of low, mid, and high transmitters are determined

xChoice <- "meehan"
gamm <- 1/10 # recovery rate
gamm <- 1

tff <- 1e3

# create some functions to help out here
cmptMod <- function(x, xChoice = c("low", "mid"), R_0 = R_0, scaleRNum){
    # define probabilities for each of three compartments
    if(xChoice == "meehan"){
        # low <- subR(R0=R_0, sRat = 1/3, pRat = 0.07)
    #     low <- subR(R0=1.63, sRat = 0.07/0.93, pRat = 0.03)
    #     return(list(fracs = c(0.07, 0.93, 0)
    #                 , rNums = c( low
    #                             , superR(low, 0.07/0.93)
    #                             , 0
    #                 )))
    # }
     low <- subR(R0=3.19, sRat = 0.07, pRat = 0.25)
        return(list(fracs = c(0.25, 0.75, 0)
                    , rNums = c( low
                                 , superR(low, 0.07)
                                 , 0
                    )))
    }
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

dat1 <- cmptMod(0.1, xChoice = "low", scaleRNum = scaleRNum[1], R_0 = R_0)
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

#########################################################
# vecMaker <- function(vec, name){
#     x <- vec
#     names(x) <- unlist(lapply(seq_len(length(vec)), function(i){paste0(name, i)}))
#     return(x)
# }

# ssaMaker <- function(mod, gam = gamm, popSize= popSizee, tf = tff){
#     U <- length(mod$fracs)
#     # set parms
#     betas <- mod$rNums*gam/popSize
#
#     parms <- c(
#         gamma = gam
#         , vecMaker(mod$fracs, "p")
#         , vecMaker(betas, "beta")
#
#
#     )
#     # set initial states
#     # randomly seed one infection
#     I0 <- rep(0, U)
#     I0[sample(1:U,1)]<-1
#
#     x0 <- c(S = popSize -1, vecMaker(I0, "I"), R = 0)
#
#     a <- unlist(lapply(seq_len(U), function(grp){
#         c(paste0("p",grp, " * S * ("
#                  , paste0("beta1*I1"
#                           , paste0(c(unlist(lapply(seq_len(U-1), function(ind){
#                      paste0(" + beta", ind+1, " * I", ind+1)
#                      }))), collapse =""), ")")
#                  ) # infection
#         , paste0("I", grp, " * gamma") # recovery
#         )
#
#     }))
#
#     # define state change matrix for each S, I, and R (rows) for each reaction
#     # (columns)
#
#     # This is super ugly, make it programmatic later
#     nu <- matrix(c(-1,  0,  -1, 0, -1, 0  #changes in S
#                    , +1, -1, rep(0,4) # I1
#                  , 0, 0, +1, -1, 0, 0 # I2
#                  , rep(0, 4), +1, -1 # I3
#                  , rep(c(0, +1), 3)
#                  )
#                  , nrow = 5
#                  , byrow=TRUE)
#     out <- list(x0 = x0, a = a, nu = nu, parms = parms, tf = tf)
#     return(out)
# }
#
# firstSSAList <- ssaMaker(mod = dat1)
# # tictoc::tic()
# firstSim <- do.call(ssa, firstSSAList)
# # tictoc::toc()
#
# # ssa.plot(firstSim)
#
# firstSim$data %>%
#     data.frame() %>%
#     pivot_longer(
#     cols = 2:6, names_to = "pop", values_to = "individuals"
# ) %>%
#     ggplot(aes(t, individuals, color = pop)) +
#     geom_point() +
#     theme_classic()
saveEnvironment()
