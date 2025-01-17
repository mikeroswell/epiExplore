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
n <- 2e4
histDat <- data.frame(ind = 1:n
                      , activity_1 = actHist(n, act1)
                      , activity_2 = actHist(n, act2)
                      , secondary_1 = secHist(n, act1)
                      , secondary_2 = secHist(n, act2)
                      )


bins <- 100
xupper <- 30

probDist <- histDat |>
  pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") ) |>
  mutate(higherGroup = paste(distType, distParms, sep = "_") )|>
  filter(val <= xupper+xupper/bins) |>
  ggplot(aes(val, fill = distParms, color = distParms, shape = distParms)) +
  # # highlight the tops of the bars with points
  stat_count(geom = "point"
             , aes(alpha =  (as.numeric(as.factor(distType))-1)
                   , group = higherGroup
                   ,  y = after_stat(count / tapply(count, group, sum)[group])
             )
             # , key_glyph = 'point'
             , position = "identity"

  ) +
   # plot PMFs as bars with "unit" width and normalized counts
  stat_count(geom = "bar"
             , aes(alpha =  0.7*(as.numeric(as.factor(distType))-1)
                   , group = higherGroup
                   ,  y = after_stat(count / tapply(count, group, sum)[group])
                   )
           # , color = scales::alpha("white", alpha = 0)
           , linewidth = 0.3
           , position = "identity"
           , width = (xupper+1) / bins
           # , key_glyph = 'rect'
               ) +

  # create a shaded region for the PDFs using geom_bar
  stat_bin(geom = "bar"
           , aes(alpha =  0.3*(2-as.numeric(as.factor(distType)))
                 , group = higherGroup
                 ,  y = after_stat(count / tapply(count, group, sum)[group])
           )
           , color = scales::alpha("white", alpha = 0)
           , breaks = seq(0, xupper, length.out = bins)
           , position = "identity"
           , bins = bins
           , key_glyph = 'polygon'
  ) +
# provide an outline for the PDFs
  stat_bin(geom = "line"
             , aes(alpha = 0.85*(2-as.numeric(as.factor(distType)))
                 , group = higherGroup
                 ,  y = after_stat(count / tapply(count, group, sum)[group])
             )
           ,  position = "identity"
           , key_glyph = "blank"
           , bins = bins
           , breaks = seq(0, xupper, length.out = bins)
           , linewidth = 0.7
           ) +

  scale_alpha_identity(
    guide = "legend"
    , name = "distribution type"
    , breaks = c(0.3, 0.7)
    , labels = c("activity (shaded areas with lines)", "secondary case (bars with points)")
    ) + # clear is clear
  xlim(c(NA, xupper)) + # truncate the PDFs (this may lead to slight inaccuracies in area integrations)
  scale_fill_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  scale_shape_discrete(name = "beta", labels = c(beta1, beta2)) +
  labs(color = "none", fill = "distribution", x = "cases per case", y = "fraction of infectors"  ) +
  guides(
    alpha = guide_legend(
      override.aes = list(
     fill = c(scales::alpha("grey", 30), scales::alpha("grey", 70) )
     , color = "black"
     , linewidth = c(0.7, 0)
     , size = c(0,2)
     , shape = 17

      )

    )


  ) +
  scale_size(range = c(0,2)) + # for whatever reason, controlling the point size here
  theme_classic()


##############

# now we want to build the inequality plots
R0ineq<- histDat |>
  mutate(across(.cols = 2:5, .fns = cumFrac, .names ="{.col}_{.fn}")) |>
  slice(which(row_number() %% 500 == 1)) |>
  mutate(frac = row_number(ind)/length(ind)) |>
  pivot_longer(cols = 6:9, names_sep = "_", names_to = c("distType", "distParms", "valType"), values_to = "val") |>

  ggplot(aes(frac, val, color = distParms, linetype = distType)) +
  geom_hline(yintercept = 0.8, linewidth = 0.5, color = "grey") +
  geom_vline(xintercept = 0.2, linewidth = 0.5, color = "grey") +
  geom_line(linewidth = 1
            #, alpha = 0.8
            ) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2", name = "beta", labels = c(beta1, beta2)) +
  labs(x = "fraction of infectors (ranked)", y = "fraction of new infections attributable to x", linetype = "distribution type")

probDist / R0ineq + plot_annotation(tag_levels = 'a')


