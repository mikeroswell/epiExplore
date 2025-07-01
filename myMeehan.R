# Code to recapitulate Meehan et al. models and look at inequalities

# load functions and libraries
library(shellpipes)
rpcall("myMeehan.Rout myMeehan.R spreadHelpers.rda secondaryDistributionPlots.rda")
library(ggplot2)
library(patchwork)
library(dplyr)
library(purrr)
loadEnvironments()
startGraphics()

# R0 <- 2
# iRat <- 0 # ratio of the reproductive number in the first serial compartment(s)
# relative to second; when iRat = 0 we have SEIR chains, and when iRat = 1 we
# have an Erlang with size = 2; this ignores duration vs. infectiveness
# differences
# pRat <- 0 # fraction of individuals assigned to "superspread" (c in Meehan)
# sRat <- 1 # ratio of *sub*spreader's to *super*spreader's reproductive number (rho in Meehan)
#

# solve for R1, R2
subR <- function(R0, sRat, pRat, iRat){

    sRat*R0/(pRat+ sRat*(1- pRat))
}

superR <- function(subR = subR, sRat){
    subR/sRat
}

# get the intrinsic probability of v offspring for one class
getV <- function(R0, sRat, pRat, iRat, myR, v = v){
   (exp(-v/myR)/myR) * (exp(-iRat*v/myR)-exp(-v/(iRat*myR)))
}

pars = list(R0 = 2, pRat = 0, sRat = 1, iRat = 0)

# do.call(getV, c(pars, myR = do.call(subR,pars), v = 1 ))
# dexp(1, 0.5)


# compute R1, R2
probSet <- function(R0, pRat, sRat, iRat
                  , pars = NULL
                  , ...){
    if(is.null(pars)){pars <- list(R0 = R0, pRat = pRat, sRat = sRat, iRat = iRat)}
    list2env(pars, envir = environment())
    if(!0<=sRat & sRat<=1){stop("Model is defined so that second class is the superspreader.\n sRat must be in [0,1]")}
    if(!0<=iRat & sRat<1){stop("Model is defined so that second serial infectious compartment is the more infective.\n iRat must be in [0,1)")}
    R1 <- do.call(subR, pars)
    R2 <- superR(R1, sRat)
    return(list(pars = pars, R1 = R1, R2 = R2))
}

# probSet(R0 =1, pRat = 0.2, sRat = 0.2, iRat = 0.2)

# probability distribution of individual reproductive numbers, v
vProb <- function(pSet, v){
    list2env(pSet, envir = environment())
    list2env(pars, envir = environment())
    # pV <- (1+iRat)/(1-iRat) *
    #     ((1-pRat)*do.call(getV, c(pars, R1, v)) + (pRat) * do.call(getV, c(pars, R2, v)))
    #
    #

    # pV <- exp(log((1+iRat)/(1-iRat)) +
    #     log((1-pRat)*do.call(getV, c(pars, R1, v)) + (pRat) * do.call(getV, c(pars, R2, v))))

    pV <- exp(log((1+iRat)/(1-iRat)) +
                  log(
                      exp(log(1-pRat) + log(do.call(getV, c(pars, R1, v)))) +
                          exp(log(pRat) +log( do.call(getV, c(pars, R2, v))))
                      )
              )


    return(pV)
}


# vProb(probSet(pars = pars), 1)

# set a big number at which to truncate

vMax <- 40
# vProb(pars = pars, v = vMax) # confirm it's a "Big" number

# generate N random deviates using accept/reject
sampleV <- function(nTarget = 1e3, vMax = 150
                    , R0, pRat, sRat, iRat
                    , pars = NULL
                    , ...){

    if(is.null(pars)){pars <- list(R0 = R0, pRat = pRat, sRat = sRat, iRat = iRat)}
    list2env(pars, envir = environment())

    vVec <- numeric()
    pSet <- probSet(pars = pars)
    testMax <- vProb(pSet, vMax)
    if(testMax > 1e-12){warning(paste0("p(vMax) = ", testMax, ", consider increasing vMax to avoid truncating distribution"))}
    while(length(vVec)<nTarget){

        cand <- runif(1, min = 0, max = vMax)
        kP <- vProb(pSet, v = cand)

        if(isTRUE(as.logical(rbinom(1, 1, kP)))){vVec <- c(vVec, cand)}
    }
    return(vVec)
}

# see if this goes reasonably fast
# replicate(10, system.time(sampleV(nTarget = 100, vMax = vMax, pars = pars)))

# But I know the PDF, so just get a profile, truncating somehow

vDens <- function(vMax
                  , dType = "density"
                  , nPoints = 200
                  , R0, pRat, sRat, iRat
                  , pars = NULL
                  , ...){
    if(is.null(pars)){pars <- list(R0 = R0, pRat = pRat, sRat = sRat, iRat = iRat)}
    list2env(pars, envir = environment())

    pSet <- probSet(pars = pars)
    newFun <- function(x){vProb(pSet, x)}
    testMax <- newFun(vMax)
    if(testMax > 1e-12){warning(paste0("p(vMax) = ", testMax, ", consider increasing vMax to avoid truncating distribution"))}



    if(dType == "density"){
        vVec <- seq(0, vMax, length.out = nPoints)
        pX = sapply(vVec, function(x){newFun(x)})
    }
    if(dType == "mass"){
        vVec <- seq(0, vMax, 1)
        pX = sapply(vVec, function(x){
            massFun(x, newFun)
        })
    }

    return(data.frame(X = vVec, dType = dType, pX = pX ))

}


vDens(vMax = vMax, dType = "density", pars = pars)
vDens(vMax = vMax, dType = "mass", pars = pars)
# Check that it looks geometric when I make this an S[E]IR model where E is
# irrelevant for the D0 distribution.
# n <- 1e3
# myExp <- sort(sampleV(nTarget = n, vMax = vMax
#                       , pars = list(R0 = 2, pRat = 0, sRat = 1, iRat = 0)))
# realExp <- sort(rexp(n= n, rate = 0.5))
# myGeo <- sort(rpois(n = n, myExp))
# realGeo <- sort(rgeom(n = n, prob = 1/3))
#
# par(mfrow = c(2,1))
# hist(myGeo, xlim = c(0, 10), breaks = 10)
# hist(realGeo, xlim = c(0, 10), breaks = 10)
#
# hist(myExp, xlim = c(0, 10), breaks = 30)
# hist(realExp, xlim = c(0, 10), breaks = 30)
#
#
# # Check that R0 actually works
# map((1:10)/10, function(p){
#     map((1:10)/10, function(s){
#         map((1:9)/10, function(i){
#             R0est <- mean(sampleV(R0 = 2, pRat = p, sRat = s, iRat = i))
#             print(paste(p, s, i))
#             print(paste0("R0 = ", 2, " R0est = ", R0est ))
#         })
#     })
# })



#######
# will want to separate this out better, but just listing some parameter sets for now

# one class but two I stages, where the reproductive number in first is half
# that of the second
# pars.twostage <- list(R0 = 2, pRat = 0, sRat = 1, iRat = 0.5)
# two classes but one stage
# pars.twoclass <- list(R0 = 2, pRat = 0.5, sRat = 0.5, iRat = 0)
# full fancyness: two classes, each with two stages
# pars.fancy <- list(R0 = 5, pRat = 1/5, sRat = .25, iRat = 0.1 )
# a guess about what might produce high inequality
# pars.heter <- list(R0 = 9, pRat = 0.35, sRat = 0.35, iRat = 0.0 )


pars.homo <- list(R0 = 8, pRat = 1, sRat = 1, iRat = 0.0)

# a guess about what might produce high inequality
pars.medi <- list(R0 = 8, pRat = 0.3, sRat = 0.15, iRat = 0.0)

# 80/20
pars.heter <- list(R0 = 8, pRat = 0.1, sRat = 0.1, iRat = 0.0)

#################################################


densDat <- map_dfr(c("pars.homo", "pars.medi", "pars.heter"), function(parSet){
    data.frame(parSet = parSet
               , bind_rows(vDens(vMax = 90, pars = get(parSet)
                       , nPoints = 400)
                       , vDens(vMax = 90, pars = get(parSet)
                               , dType = "mass"
                             )
                       )
    )
})


######################
secondaryCases <- function(pars, nTarget = 1e4, vMax = 1e3){
    ideal <- sampleV(pars = pars, nTarget = nTarget, vMax = vMax)
    real <- rpois(nTarget, ideal)
    return(data.frame(ideal = ideal, real = real))
}



data.homo <- secondaryCases(pars.homo)
data.medi <- secondaryCases(pars.medi)

data.heter <- secondaryCases(pars.heter)



cFrac.homo <- makeDistData(data.homo$ideal) %>% bind_cols(data.homo)
cFrac.medi <- makeDistData(data.medi$ideal) %>% bind_cols(data.medi)
cFrac.heter <- makeDistData(data.heter$ideal) %>% bind_cols(data.heter)

# will want to make histograms of these





ehm.homo <- ehm(cFrac.homo)
ehm.medi <- ehm(cFrac.medi)
ehm.heter <- ehm(cFrac.heter)





cFracs <- bind_rows(cFrac.homo, cFrac.medi, cFrac.heter
                    , .id = "model_description" )

saveEnvironment()

