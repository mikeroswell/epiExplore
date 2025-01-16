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
beta1 <- 0.15
beta2 <- 0.8
gamm <- 1/10

# # how many slices?
# n <- 151
# # range
# betaT <- c(seq(0, 6, length.out = n), 0.0001, 0.9999, 1.0001, 1.9999, 2.001, 2.9999, 3.0001, 3.9999, 4.0001)

act1 <- beta1/gamm
act2 <- beta2/gamm


actDist <- function(betaT, act){
  d <- dexp(betaT, rate = 1/act)
   # d/sum(d)
  d
}

# deviates to sample
# n <- 15000

actHist <- function(n, act){
  d <- rexp(n = n, rate = 1/act)
  return(d)
}

secDist <- function(betaT, act){
  d <- dgeom(betaT, prob = makeP(1/act))
  # d/sum(d)
  # d*length(betaT)
  d
}

secHist <- function(n, act){
  d <- rgeom(n = n, prob = makeP(1/act))
  return(d)
}

# deadDat <- data.frame( betaT = betaT
#                        , act_1 = actDist(betaT, act1)
#                        , act_2 = actDist(betaT, act2)
#                        , sec_1 = secDist(betaT, act1)
#                        , sec_2 = secDist(betaT, act2)
#                        )
n <- 5e4
histDat <- data.frame(ind = 1:n
                      , activity_1 = actHist(n, act1)
                      , activity_2 = actHist(n, act2)
                      , secondary_1 = secHist(n, act1)
                      , secondary_2 = secHist(n, act2)
                      )

# Custom StatBinNoZeros implementation
# Adapted with assistance from OpenAI's ChatGPT for filtering zero-count bins in ggplot2
# Conversation date: January 16, 2025
{
StatBinNoZeros <- ggproto(
  "StatBinNoZeros", StatBin,
  compute_group = function(data, scales, binwidth = NULL, bins = 30, breaks = NULL, self = NULL, ...) {
    # Check and set default for bins
    if (is.null(bins)) bins <- 30

    # Call the parent's compute_group explicitly
    bin_data <- ggproto_parent(StatBin, StatBinNoZeros)$compute_group(
      data = data, scales = scales, binwidth = binwidth, bins = bins, breaks = breaks, ...
    )

    # Filter out bins with zero count
    if (!"count" %in% names(bin_data)) {
      stop("The computed data does not contain a 'count' column. Ensure proper mapping.")
    }
    bin_data <- bin_data[bin_data$count > 0, ]
    return(bin_data)
  }
)

stat_bin_no_zeros <- function(mapping = NULL, data = NULL, geom = "bar",
                              position = "identity", ..., binwidth = NULL,
                              bins = 30, breaks = NULL, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(
    stat = StatBinNoZeros, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(binwidth = binwidth, bins = bins, breaks = breaks, na.rm = na.rm, ...)
  )
}
}



# histDat |>
#   pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
#   # mutate(fillgrp = paste(distType, distParms)) |>
#   ggplot(aes(val, fill = distParms, color = distParms, shape = distParms, group = distType)) + # , color = fillgrp, color = distParms,
#   geom_histogram(aes(alpha =  0.7*as.numeric(as.factor(distType))), position = "identity", bins = 100, linewidth = 0) + #, color = "black"
#   stat_count(geom = "point", aes(alpha =  as.numeric(as.factor(distType))-1, size = 5*(as.numeric(as.factor(distType))-1))) +
#   scale_alpha_identity() +
#   xlim(c(NA, 5)) +
#   scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
#   scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
#   # scale_color_manual(values = c("orange","darkorange", "blue", "navy")) +
#   # scale_fill_manual(values = c( "orange","darkorange", "blue", "navy")) +
#   labs(color = "distribution", fill = "distribution", x = "cases per case", y = "infectors (of 5000)",  ) +
#   scale_size(range = c(0,2)) +
#   scale_y_continuous(sec.axis = sec_axis(name = "% infectors", transform = function(x){100*x/max(x)})) +
#   theme_classic()


histDat |>
  pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
  mutate(higherGroup = paste(distType, distParms, sep = "_") )|>
  # mutate(fillgrp = paste(distType, distParms)) |>
  ggplot(aes(val, fill = distParms, color = distParms, shape = distParms)) + # , color = fillgrp, color = distParms,
  # geom_histogram(alpha = 0, position = "identity", bins = 100, linewidth = 0, aes(group = higherGroup)) + #, color = "black"
  stat_bin(geom = "bar"
             , aes(alpha =  0.7*(as.numeric(as.factor(distType))-1)
                   , group = higherGroup)
           , color = scales::alpha("white", alpha = 0)
             , position = "identity"
           , bins = 100
               ) +
  stat_bin(geom = "bar"
           , aes(alpha =  0.3*(2-as.numeric(as.factor(distType)))
                 , group = higherGroup)
           , color = scales::alpha("white", alpha = 0)
           , position = "identity"
           , bins = 100
  ) +
  stat_bin_no_zeros(geom = "point"
           , aes(alpha =  (as.numeric(as.factor(distType))-1)
                  , group = higherGroup
                , y = after_stat(count)
                 )
           # , color = "white"
           , position = "identity"
           # , label = "-"
           , bins = 100
  ) +
  stat_bin(geom = "line"
             , aes(alpha = 0.7*(2-as.numeric(as.factor(distType)))
                 , group = higherGroup)
             ,  position = "identity"
           , bins = 100) +
  scale_alpha_identity() +
  xlim(c(NA, 30)) +
  scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  scale_shape_discrete(name = "beta", labels = c(beta1, beta2)) +
  # scale_color_manual(values = c("orange","darkorange", "blue", "navy")) +
  # scale_fill_manual(values = c( "orange","darkorange", "blue", "navy")) +
  labs(color = "distribution", fill = "distribution", x = "cases per case", y = "infectors (of 5000)",  ) +
  scale_size(range = c(0,2)) +
  scale_y_continuous(sec.axis = sec_axis(name = "% infectors", transform = function(x){100*x/max(x)})) +
  theme_classic()

# scalor <- 2
#
# deadDat |>
#   # ggplot()+
#   # geom_freqpoly(aes(x= act_1),  color = "red", bins = 250, stat = "bin") +
#   # geom_point(aes(x = sec_2), color = "maroon", stat = "count") +
#   # geom_freqpoly(aes(x = act_2), color = "maroon", bins = 250, stat= "bin", closed = "left") +
#   # geom_point(aes(x = sec_1), color = "red", stat= "count")+# , geom = "point")+
#   #
#   pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
#   mutate(val = if_else(val ==0, NA, val)) |>
#   ggplot(aes(betaT, val,  color = distParms, fill = distParms, shape = distParms) )+ # , color = distParms,
#   xlim(c(NA,4)) +
#   # scale_fill_brewer() +
#   # geom_histogram(bins = 50, position = position_identity(), alpha = 0.5)+
#   # geom_line(linewidth = 0.2)+
#
#   # scale_shape_manual(values = c(20, 16 )) +
#   geom_area(position= "identity", aes(alpha = 0.8*(as.numeric(as.factor(distType)))), na.rm = TRUE, linewidth = 0)+ #data = deadDat[deadDat$distType == "act",]
#   geom_point( aes(y =  val * max(val, na.rm = TRUE) * scalor
#         , size = 0.5* (as.numeric(as.factor(distType))-1)
#         , alpha =  (as.numeric(as.factor(distType))-1)
#                   )
#   ) +
#   # # geom_line() +
#   # geom_col(position = "identity", alpha = 0.7, linewidth = 0.2) +
#   scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
#   scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
#   # scale_fill_manual(values = c(0, 0, "red", "blue")) +
#   theme_classic() +
#   scale_size(range = c(0,3)) +
#   scale_alpha_identity() +
#   # scale_shape_manual(values = 16) +
#   labs(x = "cases per case", y = "density of infectors (curves)") +
#   guides(size = "none"
#          # , alpha = "none"
#
#   ) +
#
#   scale_y_continuous(limits = c(NA, 40)
#                      , sec.axis = sec_axis(name = "fraction infectors (points)", transform = function(x){x/max(x)}))
#
#   # v1
# deadDat |>
#   # ggplot()+
#   # geom_freqpoly(aes(x= act_1),  color = "red", bins = 250, stat = "bin") +
#   # geom_point(aes(x = sec_2), color = "maroon", stat = "count") +
#   # geom_freqpoly(aes(x = act_2), color = "maroon", bins = 250, stat= "bin", closed = "left") +
#   # geom_point(aes(x = sec_1), color = "red", stat= "count")+# , geom = "point")+
#   #
#   pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
#   group_by(distType, distParms) |>
#   mutate(  val = val/sum(val),
#             val = if_else(val==0, NA, val)
#            ) |>
#   ggplot(aes(betaT, val,  color = distParms, fill = distParms) )+ # , color = distParms,
#   xlim(c(NA,4)) +
#   # scale_fill_brewer() +
#   # geom_histogram(bins = 50, position = position_identity(), alpha = 0.5)+
#   # geom_line(linewidth = 0.2)+
#
#   # scale_shape_manual(values = c(20, 16 )) +
#   geom_area(position= "identity", aes(alpha = 0.8*(as.numeric(as.factor(distType)))), na.rm = TRUE, linewidth = 0)+ #data = deadDat[deadDat$distType == "act",]
#   geom_point( aes(size = 0.5* (as.numeric(as.factor(distType))-1)
#                   , alpha =  (as.numeric(as.factor(distType))-1)
#   )) +
#   # # geom_line() +
#   # geom_col(position = "identity", alpha = 0.7, linewidth = 0.2) +
#   scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
#   scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
#   # scale_fill_manual(values = c(0, 0, "red", "blue")) +
#   theme_classic() +
#   scale_size(range = c(0,2)) +
#   scale_alpha_identity() +
#   # scale_shape_manual(values = 16) +
#   labs(x = "cases per case", y = "Fraction of infectors (points)") +
#   guides(size = "none"
#          # , alpha = "none"
#
#   ) +
#   scale_y_continuous(sec.axis = sec_axis(name = "% infectors", transform = function(x){100*x/max(x)}))
#
