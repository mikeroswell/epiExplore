# three body problem

# goal: we want to have a single parameter to describe three values. This means
# we need to live with a few constraints. The goal is to find a set of
# constraints that let us cover parameter space broadly. We'll start with values
# on the simplex.

# we'll call a the biggest, b the middle, and c the smallest

library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Additive offset kinda stinks, b always 1/3

# Multiplicative offset 
# multOff <- map_dfr(seq(0.001, 0.33, 0.01), function(b){
#    eps <- uniroot(function(eps){ b + b*eps + b/eps - 1}
#                       , interval = c(0,1)
#                   , extendInt = "upX")$root
#    return(data.frame(eps = eps, high = b/eps, b, low = b*eps))
# })
# 

# calculate eps directly using quadratic formula
multOff <- map_dfr(seq(0.0003, 0.3333, 0.003), function(b){
    eps <-max((-sqrt((b-1)^2-4*b^2)+(1-b))/(2*b)
              , (sqrt((b-1)^2-4*b^2)+(1-b))/(2*b))
    data.frame( mid = b, eps = 1/eps, low = b*eps, high = b/eps)
}) %>% 
    pivot_longer(cols = c("low","mid", "high")
                 , names_to = "frac.name", values_to = "frac.value")

# seems really fine. Main drawback: can't have two values similar and one v. different
multOff_fig <- multOff %>% ggplot(aes(x = eps, frac.value
                                      , color = factor(
                                          frac.name
                                          , levels = c("low"
                                                       , "mid"
                                                       , "high"))))+
    geom_point()+
    theme_classic()  +
    ylim(c(0,1)) +
    labs(x = "eps"
         , y = "fractions"
         , color = "transmission rate"
         , title = "fractions: x/eps, x, x*eps") +
    scale_color_viridis_d()

geomOff <- map_dfr(seq(0, 1, 0.01), function(b){
    eps <-max((-sqrt(-3*(b)^2+4*b)-b)/(2*b)
              , (sqrt(-3*(b)^2+4*b)-b)/(2*b)
              )
    data.frame( high = b*eps^2, eps, mid = b*eps, low = b)
}) %>% 
    pivot_longer(cols = c("low","mid", "high")
                 , names_to = "frac.name", values_to = "frac.value")

geomOff_fig <- geomOff %>% ggplot(aes(x = eps, frac.value
                                      , color = factor(
                                          frac.name
                                          , levels = c("low"
                                                       , "mid"
                                                       , "high"))))+
    geom_point()+
    theme_classic()  +
    ylim(c(0,1)) +
    labs(x = "eps"
         , y = "fractions"
         , color = "transmission rate"
         , title = "fractions: x, eps*x, eps^2*x") +
    scale_color_brewer(palette = 11)



pdf("fractionRule_fig.pdf", height = 4, width = 8)
multOff_fig + geomOff_fig
dev.off()
# tol < 1e-9
# # inner angle in triangle inscribed in a circle
# angles <- map_dfr(seq(0, pi/2, pi/199), function(theta){
#     sa <- sin(theta/2)
#     sb <- sin((pi*2-theta)/2)
#     sc <- cos(theta/2)
#     sides <- c(sa, sb, sc)
#     sums <- sum(sides)
#     low <- min(sides)/sums
#     high <- max(sides)/sums
#     if(abs(min(diff(sides)))>tol){
#         mid <-  setdiff(sides/sums, c(low, high))}
#     if(abs(min(diff(sides)))<tol){
#         print(paste("theta = ", theta, "... just 2"))
#         mid <- sides[which.min(abs(diff(sides)))] }
#     return(data.frame(theta,low, mid, high ))
# })
# 
# anglePlot <- angles %>% pivot_longer(cols = c("low", 
#                                  "mid", "high")
#                         , names_to = "frac.name", values_to = "frac.value") %>% 
# ggplot(aes(theta, frac.value, color = frac.name )) +
#     geom_point()+
#     theme_classic()+
#     labs(x = "theta", y = "fractions", color = "", title = "inscribed triangle")
# 
# offsetPlot +anglePlot
# 
#                          
