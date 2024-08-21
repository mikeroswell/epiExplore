# Code to recapitulate Meehan et al. models and look at inequalities

# load functions and libraries
# computing
source("spreadHelpers.R")
# plotting
source("secondaryDistributionPlots.R")
library(ggplot2)
library(patchwork)
library(dplyr)
library(purrr)

# R0 <- 2
# iRat <- 0 # ratio of the reproductive number in the first serial compartment(s) 
# relative to second; when iRat = 0 we have SEIR chains, and when iRat = 1 we 
# have an Erlang with size = 2; this ignores duration vs. infectiveness 
# differences
# pRat <- 0 # fraction of individuals assigned to "superspread"
# sRat <- 1 # ratio of *sub*spreader's to *super*spreader's reproductive number
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

# pars = list(R0 = 2, pRat = 0, sRat = 1, iRat = 0)

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
    pV <- (1+iRat)/(1-iRat) *
        ((1-pRat)*do.call(getV, c(pars, R1, v)) + (pRat) * do.call(getV, c(pars, R2, v)))
    return(pV)
}

# vProb(probSet(pars = pars), 1)

# set a big number at which to truncate

vMax <- 999
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
                  , nPoints = 200
                  , R0, pRat, sRat, iRat
                  , pars = NULL
                  , ...){
    if(is.null(pars)){pars <- list(R0 = R0, pRat = pRat, sRat = sRat, iRat = iRat)}
    list2env(pars, envir = environment())
    
    pSet <- probSet(pars = pars)
    testMax <- vProb(pSet, vMax)
    if(testMax > 1e-12){warning(paste0("p(vMax) = ", testMax, ", consider increasing vMax to avoid truncating distribution"))}
    vVec <- seq(0, vMax, length.out = nPoints)
    return(data.frame(X = vVec, pX = sapply(vVec, function(x){vProb(pSet, x)})))
    
}


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


pars.homo <- list(R0 = 10, pRat = 1, sRat = 1, iRat = 0.0)

# a guess about what might produce high inequality
pars.medi <- list(R0 = 10, pRat = 0.3, sRat = 0.15, iRat = 0.0)

# 80/20
pars.heter <- list(R0 = 10, pRat = 0.1, sRat = 0.1, iRat = 0.0)

#################################################


densDat <- map_dfr(c("pars.homo", "pars.medi", "pars.heter"), function(parSet){
    data.frame(parSet = parSet
               , vDens(vMax = 900, pars = get(parSet)
                       , nPoints = 5e3))
})

densPlot <- 
    densDat %>% 
    mutate(parSet = factor(parSet
                           , levels = c("pars.homo", "pars.medi", "pars.heter")
                           , labels = c(bquote(kappa ==  1)
                                        , bquote(kappa == 2.9) 
                                        , bquote(kappa == 4.9))
                           )
           ) %>% 
    ggplot(aes(X, pX, color = parSet, fill = parSet)) + 
    geom_ribbon( aes(ymax = pX)
                 , linewidth = 0
                 , ymin = -Inf
                 # , alpha = 0.2
                 , outline.type = "upper") + 
    geom_vline(xintercept = 10, color = "red") +
    annotate("text", x = 60, y = 2e-2
             , label = "bolditalic(R)[0] == 10"
             , parse = TRUE
             , color = "red"
             , size = 2) +
    geom_text(
               aes(x = 200, y = 2e-3, label = parSet)
              , stat = "unique"
              
              , parse = TRUE
              , color = "black"
              , size = 3) +
        # annotate("text",aes( x = 200, y = 2e-2, label = parSet), size = 2)+
    theme_classic(base_size = 6) +
    scale_y_log10(n.breaks = 6, limits = c(5e-6, NA), expand = c(0,0)) +
    # scale_y_sqrt(expand = c(0,0), limits = c(0,NA)) + 
    xlim(c(0, 375)) +
    scale_color_viridis_d()+
    scale_fill_viridis_d() +
    # guides(color = guide_legend(position = "inside")
    #        , fill = guide_legend(position = "inside")) +
        theme(strip.background = element_blank()
              # , strip.text = element_blank()
              # , panel.spacing.y = unit(1.1, "lines")
              # , strip.placement = "inside"
              # , strip.text = element_text(vjust = 0, size =14)
              , strip.text = element_blank()
              # , legend.position.inside = c(0.7, 0.7)
              , legend.position = "none"
              ) +
        labs(x = "expected new cases per infected individual", y = "density") +
    facet_wrap(~parSet, nrow = 3
               #, labeller = label_parsed
               ) 
# dev.off()









######################
secondaryCases <- function(pars, nTarget = 5e3, vMax = 1e3){
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





ehm.homo <- ehm(cFrac.homo)
ehm.medi <- ehm(cFrac.medi)
ehm.heter <- ehm(cFrac.heter)

ehm.homo
ehm.medi
ehm.heter


cFracs <- bind_rows(cFrac.homo, cFrac.medi, cFrac.heter
                    , .id = "model_description" )

ineq <- cFracs %>% 
    mutate(model_description = 
               factor(model_description
                      , labels = c("homogeneous"
                               # , "10 of duration a : 3 of duration 20/3a"
                               #     , "10 of duration b : 1 of duration 10b"
                               , "3 slow:10 fast; slow is 15% as fast"
                               , "1 slow:10 fast; slow is 10% as fast")
                      )
           ) %>% 
                      cFPlot(showRealized = FALSE) + 
 aes(color = model_description) +
    scale_color_viridis_d()  +
    geom_point(x = 0.2, y = 0.8, shape = 18, color = "black", size = 3) +
    labs(color = "Distribution of infections"
         , x = "fraction of most infectious individuals"
         , y = "fraction of new infections") +
    guides(color = guide_legend(position = "inside")) +
    theme_classic(base_size = 6) +
    theme(legend.position.inside = c(0.6, 0.3)
          , legend.text = element_text(size = 4.5)
          , legend.title = element_text(size = 6)
          , legend.key.size = unit(0.3, "lines")
          , legend.key.spacing = unit(0.3, "lines")
          # , legend.key
          # , base_size = 6
          )



# hists <- cFracs %>% 
#     mutate(model_description = 
#                factor(model_description
#                       , labels = c("kappa = 1"
#                                    , "kappa = 2.7"
#                                    , "kappa = 5"))
#            ) %>% 
#     secDist(caseCol = "ideal", xMax = 300
#             , breaks = c(0.0005, 0.005, 0.05,0.5)) +
#     aes(group = model_description, fill = model_description) +
#     scale_fill_viridis_d() +
#     facet_wrap(~model_description, ncol = 1) 
    

pdf(file = "MPOPHC_emergent.pdf", width = 4, height = 2)
# quartz(width = 4, height = 2)
ineq  + densPlot 
dev.off()

# twostage
# makeDistData(sampleV(pars = pars.twostage)) %>% 
#     ggplot(aes(q, cFRealiz))+
#     geom_point() +
#     geom_point(aes(y = cFIdeal), color = "grey") +
#     geom_hline(yintercept = 0.8) +
#     geom_vline(xintercept = 0.2) + 
#     theme_classic()
# 
# makeDistData(sampleV(pars = pars.twoclass)) %>% 
#     ggplot(aes(q, cFRealiz))+
#     geom_point() +
#     geom_point(aes(y = cFIdeal), color = "grey") +
#     geom_hline(yintercept = 0.8) +
#     geom_vline(xintercept = 0.2) + 
#     theme_classic()

#################################
# # make this a function to avoid repetition
################################

# data.frame(exNewCases = sampleV(pars = pars.medi)) %>% 
#     ggplot(aes(exNewCases))+
#     geom_histogram() + 
#     geom_vline(aes(xintercept = mean(exNewCases)), color = "red")+
#     theme_classic() + 
#     labs(x = "expected new cases per infectious individual", y = "fraction of infected individuals") + 
#     scale_y_continuous(labels = function(x){x/1000})



# data.frame(exNewCases = sampleV(pars = pars.heter, nTarget = 5e3)) %>% 
#     ggplot(aes(exNewCases))+
#     geom_histogram() + 
#     geom_vline(aes(xintercept = mean(exNewCases)), color = "red") +
#     theme_classic() + 
#     labs(x = "expected new cases per infectious individual", y = "fraction of infected individuals") + 
#     scale_y_continuous(labels = function(x){x/1000})





# data.frame(exNewCases = sampleV(pars = pars.fancy)) %>% 
#     ggplot(aes(exNewCases))+
#     geom_histogram() + 
#     geom_vline(aes(xintercept = mean(exNewCases)), color = "red") +
#     theme_classic() + 
#     labs(x = "expected new cases per infectious individual", y = "fraction of infected individuals") + 
#     scale_y_continuous(labels = function(x){x/1000})
# 
# 
# 
# makeDistData(sampleV(pars = pars.fancy)) %>% 
#     ggplot(aes(q, cFRealiz))+
#     geom_point() +
#     geom_point(aes(y = cFIdeal), color = "grey") +
#     geom_hline(yintercept = 0.8) +
#     geom_vline(xintercept = 0.2) + 
#     theme_classic() + 
#     labs(x = "fraction of infectious individuals, ranked by n(offspring)", y = "fraction of new infections")
# 





