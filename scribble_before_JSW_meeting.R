# Try to compute an "80, X" fraction for a compartment model with classes
# say age.

library(dplyr)
library(purrr)
library(ggplot2)

#### Not sure if this will be code or pseudo code

source("spreadHelpers.R")

# we start with 4 separate classes
nClasses <- 4

# and set the sizes as a fraction of the whole
classFracs <- nrmlz(1:4)

# We define class-specific recovery rates
Gamma <- (1:4)/20

# and we have transmission rates that are only non-zero along the diagonal? 
# This is like having nClasses separate populations

Beta <- matrix(rep(0, nClasses^2), ncol = nClasses, nrow = nClasses)

selfRates <- c(4:1)
diag(Beta) <- selfRates
Beta

# Now I think I can think about this in v. simple terms like in `myLloydSmith.R`
# get the whole population size
fullPop <- 1e5
SS <- 1e3

classSize <- classFracs*fullPop

# I think JSW suggests I can think about the number infected per individual
# already as a geometric with rate Beta/(Beta + Gamma). This seems reasonable,
# but let's go a bit slower for now.

# To make it easy, we assume integer increases in sample sizes between classes

simplest <- map_dfr(1:nClasses, function(clss){
    intrinsicOffspring <- rexp(n = SS * 10 * classFracs[clss]
          , rate = selfRates[clss]/Gamma[clss])
    realizedOffspring <-
        rpois(n = SS * 10 * classFracs[clss], lambda = intrinsicOffspring)
    return(data.frame(clss, intrinsicOffspring, realizedOffspring))
          
}
)

combined <- data.frame(q = (1:length(simplest[,1]))/length(simplest[,1])
                       , iSort <- sort(simplest$intrinsicOffspring
                                       , decreasing = TRUE)
                       , rSort <- sort(simplest$realizedOffspring
                                       , decreasing = TRUE)
                       , cFIdeal = cumFrac(simplest$intrinsicOffspring)
                       , cFRealiz = cumFrac(simplest$realizedOffspring)
)



combined %>% ggplot(aes(q, cFRealiz)) +
    geom_point() + 
    geom_point(aes(y = cFIdeal), color = "grey") +
    theme_classic() +
    labs(x = "total fraction contributing")    

simplest %>% 
    group_by(clss) %>% 
    summarize(over4 = sum(realizedOffspring > 4)
              , over3 = sum(realizedOffspring > 3)
              , over2 = sum(realizedOffspring > 2)
              , over1 = sum(realizedOffspring > 1)
              , over0 = sum(realizedOffspring > 0))

NGS <- matrix(rep(0, nClasses^2), nrow = nClasses)
diag(NGS) <- selfRates*classFracs/Gamma
eigen(NGS)
