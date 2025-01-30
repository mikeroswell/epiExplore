library(shellpipes)
rpcall("densHist.Rout densHist.R")
densHist <- function(.data
                     , xlab = "cases per case"
                     , ylab = "density"
                     , colorVar = "distParms"
                     , colorVals = NULL
                     , colorLab = NULL
                     , groupVar = NULL
){
  if(is.null(colorLab)){colorLab <- colorVar}
  ggplot(.data, aes(x = x
                    , y = d
                    , color = get(eval(colorVar))
                    , fill = get(eval(colorVar))
  )
  ) +
    geom_point(alpha = 0, aes(group = get(eval(groupVar)))
               # , key_glyph = "point"
    )+
    geom_line(aes(alpha = as.numeric(distType == "act"), group = get(eval(groupVar)))
              # , key_glyph = "path"
    ) +
    geom_bar(aes(alpha = 0.5* as.numeric(distType != "act"), width = barWidth, group = get(eval(groupVar)))
             , stat = "identity"
             , position = "identity"
             , color = scales::alpha("white", alpha = 0)

    ) +
    # set up a guide for the plotting type, using the alpha scale that renders
    # outside type invisible
    scale_alpha_identity(breaks = c(0,1)
                         # , range = c(0,1)
                         , labels = c("activity", "secondary case")
                         , guide = guide_legend(title = "distribution type"
                                                , override.aes = list(
                                                  linewidth = c(0.8, 0)
                                                  , shape = c(15, 21)
                                                  , linetype = c("solid", "blank")
                                                  , fill = c(NA, "grey")
                                                  , size = c(0,2)
                                                  , alpha = c(1)
                                                  # , order = 1
                                                )
                         )
    ) +
    scale_fill_brewer(palette = "Dark2", name = colorLab, labels = colorVals
                      # , guide = guide_legend(order = 2)
                      ) +
    scale_color_brewer(palette = "Dark2", name = colorLab, labels = colorVals
                       # , guide = guide_legend(order = 2)
                       ) +
    theme_classic() +
    labs(x = xlab
         , y = ylab
         , color = colorLab
         , fill = colorLab
    )
}

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
    labs(x = "fraction of infectors (ranked)"
         , y = "cumulative fraction of new infections"
         , linetype = "distribution type")
}

saveEnvironment()