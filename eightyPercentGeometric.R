# Confirm... 80% of infections from the same fraction of infecteds in all SIR
# models?
library(dplyr)
library(purrr)
library(ggplot2)

bv <- 10^(1:4) 
popSize <- c(bv, 2*bv, 5*bv) # population sizes

pRec <- seq(0.05, 0.3, 0.05) # Beta over Beta + Gamma


geom_Study <-map_dfr(popSize, function(pS){
    map_dfr(pRec, function(pR){
        myGeom <- pgeom(1:pS, pR) * 1:pS # expected infections per person
        totCase <- sum(myGeom) # total number of infections (should check against analytical estimate of total epidemic size)
        cumFrac <- cumsum(rev(myGeom))/totCase # start at the top and compute the cumulative fraction of cases attributable to that cumulative chunk of the population
        eighty <- which(cumFrac >= 0.8)[1]/ pS # what size is the chunk for 80%
        return(data.frame(popSize = pS
                          , betaOverBetaPlusGamma = pRec
                          , threshEighty = eighty)
               )
       })
})

# It's always the same @ 55%,, where I'm seeing tie-breaking in the dark points
geom_Study %>% 
    ggplot(aes(betaOverBetaPlusGamma, threshEighty, color = popSize)) + 
    geom_point() +
    theme_classic() 
  
