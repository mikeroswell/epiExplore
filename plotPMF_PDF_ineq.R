library(shellpipes)
rpcall("plotPMF_PDF_ineq.Rout plotPMF_PDF_ineq.R myMeehan.rda deadSimple.rda densHist.rda")
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
loadEnvironments()
startGraphics(height = 6, width = 9)



# # try again using density, as originally planned
#
# deadDat |>
#   separate(distr, into = c("distType", "distParms"), sep = "_") |>
#   ggplot(aes(x = x, y = d, color = distParms, shape = distParms)) +
#   geom_line(aes(alpha = distType == "act")) +
#   geom_point(aes(alpha = distType != "act"), size = 2) +
#   scale_alpha_discrete(breaks = c(0,1)
#                        , range = c(0,1)
#                        , labels = c("activity", "secondary case")
#                        , guide = guide_legend(title = "distribution type"
#                                               , override.aes = list(
#                                                 linewidth = c(0.7, 0)
#                                                 , size = c(0,2)
#                                                 , alpha = c(1, 1)
#                                               )
#                        )
#   ) +
#   labs(x = "new cases per infector", y = "density", title = "incommensurate, but densities") +
#   theme_classic()
#
# deadDat |>
#   separate(distr, into = c("distType", "distParms"), sep = "_", remove = FALSE) |>
#   # create a bar widths layer that has a narrower width at x = 0
#   mutate(barWidth = 0.5 + 0.5*(x>0)) |>
#   mutate(d = if_else(distType == "scnd", if_else(x == 0, 2*d, d), d)
#          ) |>
#   ggplot(aes(x = x, y = d, color = distParms, shape = distParms, fill = distParms)) +
#   geom_line(aes(alpha = as.numeric(distType == "act"), group = distr)
#             # , key_glyph = c("path", "shape")
#             ) +
#   # geom_ribbon(aes(ymax = d, ymin = 0, alpha = 0.2*as.numeric(distType == "act"), group = distr), linewidth = 0) +
#   geom_bar(aes(alpha = 0.5* as.numeric(distType != "act"), width = barWidth, group = distr)
#            , stat = "identity"
#            , position = "identity"
#            , color = scales::alpha("white", alpha = 0)
#
#            ) +
#   scale_alpha_identity(breaks = c(0,1)
#                         # , range = c(0,1)
#                        , labels = c("activity", "secondary case")
#                        , guide = guide_legend(title = "distribution type"
#                                               , override.aes = list(
#                                                 linewidth = c(0.8, 0)
#                                                 , shape = c(15, 21)
#                                                 # , color = c("black", "white")
#                                                 , linetype = c("solid", "blank")
#                                                 , fill = c(NA, "grey")
#                                                 , size = c(0,2)
#                                                 , alpha = c(1)
#                                               )
#                        )
#   ) +
#   scale_linewidth(range = c(0,1)) +
#   labs(x = "new cases per infector", y = "density", title = "half_width_zero") +
#   theme_classic()
#
#



fancyHist <- deadDat |>
  separate(distr, into = c("distType", "distParms"), sep = "_", remove = FALSE) |>
  # create a bar widths layer that has a narrower width at x = 0
  mutate(barWidth = 0.5 + 0.5*(x>0)) |>
  # double the density for halved bar
  mutate(d = if_else(distType == "scnd", if_else(x == 0, 2*d, d), d)
         # and shift it right to fill the space in the domain
         # , x = if_else(distType == "scnd", if_else(x == 0, 0.25, x), x)
         )


# bins <- 100
#
#
# simpleDat <- histDat |>
#   pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") )
#

# gonna need to think through this one; it's right for a histogram but not for
# the density!



twoClass <- cFracs |>
  pivot_longer(cols = c("ideal", "real")
               , names_to = "distType"
               , values_to = "val") |>
  mutate(distParms = model_description) |>
  mutate(distr = paste(distType, distParms))
#
# probDist <- function(dat
#                      , xupper = 30
#                      , colorVar = "beta"
#                      , colorVals = c(beta1, beta2)) {
#   dat |>
#   mutate(higherGroup = paste(distType, distParms, sep = "_") ) |>
#   filter(val <= xupper+xupper/bins) |>
#   ggplot(aes(val, fill = distParms, color = distParms, shape = distParms)) +
#   # # highlight the tops of the bars with points
#   stat_count(geom = "point"
#              , aes(alpha =  (as.numeric(as.factor(distType))-1)
#                    , group = higherGroup
#                    ,  y = after_stat(count / tapply(count, group, sum)[group])
#              )
#              # , key_glyph = 'point'
#              , position = "identity"
#
#   ) +
#   # plot PMFs as bars with "unit" width and normalized counts
#   stat_count(geom = "bar"
#              , aes(alpha =  0.7*(as.numeric(as.factor(distType))-1)
#                    , group = higherGroup
#                    ,  y = after_stat(count / tapply(count, group, sum)[group])
#              )
#              # , color = scales::alpha("white", alpha = 0)
#              , linewidth = 0.3
#              , position = "identity"
#              , width = (xupper+1) / bins
#              # , key_glyph = 'rect'
#   ) +
#
#   # create a shaded region for the PDFs using geom_bar
#   stat_bin(geom = "bar"
#            , aes(alpha =  0.3*(2-as.numeric(as.factor(distType)))
#                  , group = higherGroup
#                  ,  y = after_stat(count / tapply(count, group, sum)[group])
#            )
#            , color = scales::alpha("white", alpha = 0)
#            , breaks = seq(0, xupper, length.out = bins)
#            , position = "identity"
#            , bins = bins
#            , key_glyph = 'polygon'
#   ) +
#   # provide an outline for the PDFs
#   stat_bin(geom = "line"
#            , aes(alpha = 0.85*(2-as.numeric(as.factor(distType)))
#                  , group = higherGroup
#                  ,  y = after_stat(count / tapply(count, group, sum)[group])
#            )
#            ,  position = "identity"
#            , key_glyph = "blank"
#            , bins = bins
#            , breaks = seq(0, xupper, length.out = bins)
#            , linewidth = 0.7
#   ) +
#
#   scale_alpha_identity(
#     guide = "legend"
#     , name = "distribution type"
#     , breaks = c(0.3, 0.7)
#     , labels = c("activity (shaded areas with lines)", "secondary case (bars with points)")
#   ) + # clear is clear
#   xlim(c(NA, xupper)) + # truncate the PDFs (this may lead to slight inaccuracies in area integrations)
#   scale_fill_brewer(palette = "Dark2", name = colorVar, labels = colorVals) +
#   scale_color_brewer(palette = "Dark2", name = colorVar, labels = colorVals) +
#   scale_shape_discrete(name = colorVar, labels = colorVals) +
#   labs(color = "none", fill = "distribution", x = "cases per case", y = "fraction of infectors"  ) +
#   guides(
#     alpha = guide_legend(
#       override.aes = list(
#         fill = c(scales::alpha("grey", 30), scales::alpha("grey", 70) )
#         , color = "black"
#         , linewidth = c(0.7, 0)
#         , size = c(0,2)
#         , shape = 17
#
#       )
#
#     )
#
#
#   ) +
#   scale_size(range = c(0,2)) + # for whatever reason, controlling the point size here
#   theme_classic()
# }

deadDistPlot <- fancyHist |> densHist(colorVar = "distParms"
                                      , colorVals = c(1.5, 8)
                                      , colorLab = "R_0"
                                      , groupVar = "distr"
                                      )

deadDistPlot2 <- fancyHist |> densHist(colorVar = "distParms"
                                      , colorVals = c(1.5, 8)
                                      , colorLab = "R_0"
                                      , groupVar = "distr"
                                      , clearFill = TRUE
)



twoDistPlot <- densDat |>
  filter(parSet != "pars.heter") |>
  mutate(distr = paste(parSet, dType, sep = "_")
         , d = pX
         , distType = if_else(dType == "density", "act", "scnd")
         , x = X) |>
  mutate(barWidth = 0.5 + 0.5*(x>0)) |>
  # double the density for halved bar
  mutate(d = if_else(distType == "scnd", if_else(x == 0, 2*d, d), d)
         # and shift it right to fill the space in the domain
         # , x = if_else(distType == "scnd", if_else(x == 0, 0.25, x), x)
  ) |>
  densHist(colorLab = "model structure"
           , colorVar = "parSet"
           , groupVar = "distr"
           , colorVals = c("homogeneous", "23% are\n6.67x more transmissive")) +
  scale_color_manual(name = "model structure"
                        , labels = c("homogeneous"
                                     , "23% are 6.67x\nmore transmissive")
                    , values = c("#d95f02", "#7570b3")
                    ) +
  scale_fill_manual(name = "model structure"
                       , labels = c("homogeneous"
                                    , "23% are 6.67x\nmore transmissive")
                       , values = c("#d95f02", "#7570b3")
  )



##############
#ineq summary for deadSimple
deadIneq <- histDat |>
  mutate(across(.cols = 2:5, .fns = cumFrac, .names ="{.col}_{.fn}")) |>
  slice(which(row_number() %% 500 == 1)) |>
  mutate(frac = row_number(ind)/length(ind)) |>
  pivot_longer(cols = 6:9, names_sep = "_", names_to = c("distType", "distParms", "valType"), values_to = "val")

twoClassIneq <- cFracs |>
  slice(which(row_number() %% 500 == 1)) |>
  pivot_longer(cols = c("cFIdeal", "cFRealiz"), names_to = "distType", values_to = "val") |>
  mutate(distParms = model_description
         , frac = q)



ineqDeadPlot <- deadIneq |> ineq()

ineqTwoPlot <- twoClassIneq |>
  filter(distParms !=3) |>
  ineq(colorVar = "model structure"
       , colorVals = c("homogeneous"
                       , "23% are 6.67x\nmore transmissive")
       ) +
  scale_linetype_discrete(labels =c("expected infectiousness", "secondary cases")) +
  scale_color_manual(name = "model structure"
                        , labels = c("homogeneous"
                                     , "23% are 6.67x\nmore transmissive")
                     , values = c("#d95f02", "#7570b3"))+
  scale_fill_manual(name = "model structure"
                       , labels = c("homogeneous"
                                    , "23% are 6.67x\nmore transmissive")
                    , values = c("#d95f02", "#7570b3")       )



# making some sub-plot pieces for a presentation

deadDistPlot+ theme(legend.position.inside = c(0.6, 0.6)
                    , legend.justification = "left"
                    #,
                    # legend.position.inside = c(0.5, 0.1)
) +
  scale_fill_manual(values = c(alpha("white", 1), alpha("white", 1)), name = "R_0", )
# pdf("draftFig1.pdf", width = 11, height =6)

(deadDistPlot+ theme(legend.position.inside = c(0.6, 0.6)
                     , legend.justification = "left"
  #,
  # legend.position.inside = c(0.5, 0.1)
                     ) +
    ineqDeadPlot + theme(legend.position = "none")) /(
      twoDistPlot + guides(alpha = "none") + theme(legend.position = c(0.6, 0.6)
                                                   , legend.justification = "left") +
        ineqTwoPlot + theme(legend.position = "none")) +
  plot_annotation(tag_levels = 'a', tag_suffix = ") ")
# dev.off()


