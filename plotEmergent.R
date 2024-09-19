# load libraries
library(shellpipes)
manageConflicts()
startGraphics()
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

# note dependency on params and functions in kappas_in_3-class.R
loadEnvironments()

####
# kluging from mpophc cartoon

mod <- list(fracs = inputs$p, rNums = inputs$groupR)
compVar(mod)/R0(mod)

R0(mod)

emerge <- map_dfr(scaleRNum, function(scaleR){
  map_dfr(seq(0., 1, 0.02), function(x){
    dat <- cmptMod(R_0 = R_0
                   , x = x
                   , xChoice = "low"
                   , scaleRNum = scaleR)
    # R_0 <- R0(dat) # simple verification that I got R_0
    kappa <- compVar(dat)/R_0^2
    return(data.frame(eps = as.numeric(dat$pars[1])
                      , epsMethod = dat$pars[2]
                      , transmissionScaler = as.numeric(dat$pars[3])
                      , lowFrac = dat$fracs[1]
                      , midFrac = dat$fracs[2]
                      , highFrac = dat$fracs[3]
                      , R_0
                      , kappa ))
  })
})

kappaPlot <- emerge %>%
  ggplot(aes(highFrac, kappa, color = as.factor(transmissionScaler)))+
  geom_point()+
  theme_classic() +
  labs(x = "fraction of population in high-transmission group"
       , y ="kappa"
       , color = "ratio of\ntransmission\nbetween groups" )+
  scale_color_brewer(palette = "Dark2")

fractionPlot <- emerge %>%
  mutate(hf = highFrac) %>%
  pivot_longer(cols = ends_with("Frac"), names_to = "transLevel", values_to = "frac") %>%
  ggplot(aes(hf, frac
             , color = factor(transLevel
                              , levels = c("lowFrac", "midFrac", "highFrac")
                              , labels = c("low", "mid", "high")))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = RColorBrewer::brewer.pal(9, "YlOrRd")[c(9, 8, 6)]) +
  # facet_grid(~transmissionScaler) +
  labs(x = "fraction of population in high-transmission group"
       , y = "fraction of population"
       , color = "transmission\nlevel")

# R0Plot <- emerge %>%
#     ggplot(aes(highFrac, R_0)) +
#     geom_point() +
#     theme_classic() +
#     facet_grid(~transmissionScaler) +
#     labs(x = "fraction of population in high-transmission group"
#          , y = "R0\n")

# pdf("kappas_in_3-class.pdf")
kappaPlot/fractionPlot
# dev.off()