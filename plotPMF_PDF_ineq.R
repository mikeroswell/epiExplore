library(shellpipes)
rpcall("plotPMF_PDF_ineq.Rout plotPMF_PDF_ineq.R deadSimple.rda myMeehan.rda")
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
loadEnvironments()
startGraphics()

bins <- 100


simpleDat <- histDat |>
  pivot_longer(cols = 2:5, values_to = "val", names_sep = "_", names_to = c("distType", "distParms") )

twoClass <- cFracs |>
  pivot_longer(cols = c("ideal", "real"), names_to = "distType", values_to = "val") |>
  mutate(distParms = model_description)

probDist <- function(dat
                     , xupper = 30
                     , colorVar = "beta"
                     , colorVals = c(beta1, beta2)) {
  dat |>
  mutate(higherGroup = paste(distType, distParms, sep = "_") ) |>
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
  scale_fill_brewer(palette = "Dark2", name = colorVar, labels = colorVals) +
  scale_color_brewer(palette = "Dark2", name = colorVar, labels = colorVals) +
  scale_shape_discrete(name = colorVar, labels = colorVals) +
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
}

deadDistPlot <- simpleDat |> probDist()

twoDistPlot <- twoClass |> filter(distParms!=3) |>
  probDist(xupper = 75
           , colorVar = "model structure"
           , colorVals = c("homogeneous", "23% are 6.67x more transmissive")) +
  scale_color_manual(name = "model structure"
                        , labels = c("homogeneous"
                                     , "23% are 6.67x more transmissive")
                    , values = c("#d95f02", "#7570b3")
                    ) +
  scale_fill_manual(name = "model structure"
                       , labels = c("homogeneous"
                                    , "23% are 6.67x more transmissive")
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

# now we want to build the inequality plots
ineq <- function(dat, colorVar = "beta", colorVals = c(beta1, beta2)){
  dat |>
  ggplot(aes(frac, val, color = distParms, linetype = distType)) +
  geom_hline(yintercept = 0.8, linewidth = 0.5, color = "grey") +
  geom_vline(xintercept = 0.2, linewidth = 0.5, color = "grey") +
  geom_line(linewidth = 0.6
            #, alpha = 0.8
  ) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2", name = colorVar, labels = colorVals) +
  labs(x = "fraction of infectors (ranked)", y = "fraction of new infections attributable to x", linetype = "distribution type")
}

ineqDeadPlot <- deadIneq |> ineq()

ineqTwoPlot <- twoClassIneq |>
  filter(distParms !=3) |>
  ineq(colorVar = "model structure"
       , colorVals = c("homogeneous"
                       , "23% are 6.67x more transmissive")
       ) +
  scale_linetype_discrete(labels =c("activity", "secondary cases")) +
  scale_color_manual(name = "model structure"
                        , labels = c("homogeneous"
                                     , "23% are 6.67x more transmissive")
                     , values = c("#d95f02", "#7570b3"))+
  scale_fill_manual(name = "model structure"
                       , labels = c("homogeneous"
                                    , "23% are 6.67x more transmissive")
                    , values = c("#d95f02", "#7570b3")       )


# pdf("draftFig1.pdf", width = 11, height =6)

(deadDistPlot + ineqDeadPlot)/(twoDistPlot+ineqTwoPlot) + plot_annotation(tag_levels = 'a')
# dev.off()

