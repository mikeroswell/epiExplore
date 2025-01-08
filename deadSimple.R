# make dead simple activity and realized distributions
library(shellpipes)
rpcall("deadSimple.Rout deadSimple.R spreadHelpers.rda")
loadEnvironments()
startGraphics()
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# parms
beta1 <- 20
beta2 <- 200
gamm <- 1/10

# range
betaT <- seq(0, 6, 0.001)

act1 <- beta1*gamm
act2 <- beta2*gamm


actDist <- function(betaT, act){
  d <- dexp(betaT, rate = act)
  d/sum(d)
}

secDist <- function(betaT, act){
  d <- dgeom(betaT, prob = makeP(act))
  d/sum(d)
}

deadDat <- data.frame(betaT = betaT
           , act_1 = actDist(betaT, act1)
           , act_2 = actDist(betaT, act2)
           , sec_1 = secDist(betaT, act1)
           , sec_2 = secDist(betaT, act2))

deadDat |>
  pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
  mutate(val = if_else(val==0, NA, val)) |>
  ggplot(aes(betaT, val, shape = distType, color = distParms, size = (as.numeric(as.factor(distType)))^2))+
  geom_point()+
  guides(size = "none")+
  scale_size_area(max_size = 3) +
  # geom_line(linewidth = 0.2)+
  theme_classic()
