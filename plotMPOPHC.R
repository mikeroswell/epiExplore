library(shellpipes)
rpcall("plotMPOPHC.Rout plotMPOPHC.R myMeehan.rda")
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
startGraphics()
loadEnvironments()

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



# make plot for MPOPHC
ineq <- cFracs %>%
  mutate(model_description =
           factor(model_description
                  , labels = c("homogeneous"
                               # , "10 of duration a : 3 of duration 20/3a"
                               #     , "10 of duration b : 1 of duration 10b"
                               , "p = (0.23, 0.77); R = (6.7, 1)"
                               , "p = (0.1, 0.9); R = (10, 1)")
           )
  ) %>%
  cFPlot(showRealized = FALSE) +
  aes(color = model_description) +
  scale_color_viridis_d()  +
  geom_point(x = 0.2, y = 0.8, shape = 18, color = "black", size = 3) +
  labs(color = "p's and R's"
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


# pdf(file = "MPOPHC_emergent.pdf", width = 4, height = 2)
# quartz(width = 4, height = 2)
ineq  + densPlot
# dev.off()

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





