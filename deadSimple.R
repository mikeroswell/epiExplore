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

# how many slices?
n <- 151
# range
betaT <- c(seq(0, 6, length.out = n), 0.0001, 0.9999, 1.0001, 1.9999, 2.001, 2.9999, 3.0001, 3.9999, 4.0001)

act1 <- beta1*gamm
act2 <- beta2*gamm


actDist <- function(betaT, act){
  d <- dexp(betaT, rate = act)
   # d/sum(d)
  d
}

# deviates to sample
# n <- 15000

# actDist <- function(n, act){
#   d <- rexp(n = n, rate = act)
#   return(d)
# }

secDist <- function(betaT, act){
  d <- dgeom(betaT, prob = makeP(act))
  # d/sum(d)
  # d*length(betaT)
  d
}

# secDist <- function(n, act){
#   d <- rgeom(n = n, prob = makeP(act))
#   return(d)
# }

deadDat <- data.frame( betaT = betaT
                       , act_1 = actDist(betaT, act1)
                       , act_2 = actDist(betaT, act2)
                       , sec_1 = secDist(betaT, act1)
                       , sec_2 = secDist(betaT, act2))


deadDat |>
  # ggplot()+
  # geom_freqpoly(aes(x= act_1),  color = "red", bins = 250, stat = "bin") +
  # geom_point(aes(x = sec_2), color = "maroon", stat = "count") +
  # geom_freqpoly(aes(x = act_2), color = "maroon", bins = 250, stat= "bin", closed = "left") +
  # geom_point(aes(x = sec_1), color = "red", stat= "count")+# , geom = "point")+
  #
  pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
  mutate(val = if_else(val==0, NA, val)
         , val = if_else(distType == "sec", val*length(betaT), val)
         ) |>
  ggplot(aes(betaT, val,  color = distParms, fill = distParms) )+ # , color = distParms,
  xlim(c(NA,4)) +
  # scale_fill_brewer() +
  # geom_histogram(bins = 50, position = position_identity(), alpha = 0.5)+
  # geom_line(linewidth = 0.2)+

  # scale_shape_manual(values = c(20, 16 )) +
  geom_area(position= "identity", aes(alpha = 0.8*(as.numeric(as.factor(distType)))), na.rm = TRUE, linewidth = 0.1)+ #data = deadDat[deadDat$distType == "act",]
  geom_point( aes(size = 0.5* (as.numeric(as.factor(distType))-1)
                  , alpha =  (as.numeric(as.factor(distType))-1)
                  )) +
  # # geom_line() +
  # geom_col(position = "identity", alpha = 0.7, linewidth = 0.2) +
  scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  # scale_fill_manual(values = c(0, 0, "red", "blue")) +
  theme_classic() +
  scale_size(range = c(0,2)) +
  scale_alpha_identity() +
  # scale_shape_manual(values = 16) +
  labs(x = "cases per case", y = "something akin to density, area preserved?") +
  guides(size = "none"
         # , alpha = "none"

         )
# v1
deadDat |>
  # ggplot()+
  # geom_freqpoly(aes(x= act_1),  color = "red", bins = 250, stat = "bin") +
  # geom_point(aes(x = sec_2), color = "maroon", stat = "count") +
  # geom_freqpoly(aes(x = act_2), color = "maroon", bins = 250, stat= "bin", closed = "left") +
  # geom_point(aes(x = sec_1), color = "red", stat= "count")+# , geom = "point")+
  #
  pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
  group_by(distType, distParms) |>
  mutate(  val = val/sum(val)
           , val = if_else(val==0, NA, val)
           ) |>
  ggplot(aes(betaT, val,  color = distParms, fill = distParms) )+ # , color = distParms,
  xlim(c(NA,4)) +
  # scale_fill_brewer() +
  # geom_histogram(bins = 50, position = position_identity(), alpha = 0.5)+
  # geom_line(linewidth = 0.2)+

  # scale_shape_manual(values = c(20, 16 )) +
  geom_area(position= "identity", aes(alpha = 0.8*(as.numeric(as.factor(distType)))), na.rm = TRUE, linewidth = 0.1)+ #data = deadDat[deadDat$distType == "act",]
  geom_point( aes(size = 0.5* (as.numeric(as.factor(distType))-1)
                  , alpha =  (as.numeric(as.factor(distType))-1)
  )) +
  # # geom_line() +
  # geom_col(position = "identity", alpha = 0.7, linewidth = 0.2) +
  scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  # scale_fill_manual(values = c(0, 0, "red", "blue")) +
  theme_classic() +
  scale_size(range = c(0,2)) +
  scale_alpha_identity() +
  # scale_shape_manual(values = 16) +
  labs(x = "cases per case", y = "something akin to relative frequency, intuitive?") +
  guides(size = "none"
         # , alpha = "none"

  )

