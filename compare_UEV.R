# Gini (or diversity-based unevenness) notice the difference between sharply
# vs. gradually unequal distributions, I think, and will tend to say that a 
# sharply uneven distribution is less even than a gradually uneven one, whereas 
# the moment-based dispersions cosider total variation but ignore how variation 
# gets distributed
source("spreadHelpers.R")
library(dplyr)
library(tidyr)
library(ggplot2)

nMeans <- 2
meanDiff <- 1:5
varDiff <- seq(0.2, 4, 0.2)

shapeRate <- c(2,2)
nDev <- 1500

uevInds <- map_dfr(meanDiff, function(muDiff){
    map_dfr(varDiff, function(ssDiff){
        boring <- rgamma(nDev, shapeRate[1], shapeRate[2])
        Beta <- muDiff/ ssDiff
        a <- muDiff*Beta
        interesting <- rgamma(nDev, shape = Beta, rate = a)
        both <- c(boring, interesting)
        return(data.frame(
            kap = ctsKappa(both)
            , gini = Gini(both)
            , t20 = tX(DD = makeDistData(both), X = 0.2)
            , mu1 = mean(boring)
            , mu2 = mean(interesting)
            , var1 = var(boring)
            , var2 = var(interesting)
            , mu = mean(both)
            , var = var(both)
            , muDiff
            , ssDiff
            , meanDifference = muDiff - 1
            , meanVar = (1 + ssDiff)/2)
        )
    })
})


uevInds %>% 
    pivot_longer(cols = 1:3, names_to = "index", values_to = "value" ) %>% 
    ggplot(aes(x = meanVar, y = value, color = meanDifference)) +
    geom_point() + 
    facet_wrap(~index, scales = "free") + 
    scale_color_viridis_c() +
    theme_classic()

