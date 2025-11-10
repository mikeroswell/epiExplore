library(shellpipes)
library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(deSolve)
loadEnvironments()
colorval<- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
setBetas <- c(2,4,8)
finTime <- 40
steps <- 2e3
xlimits <- finTime
straightSim <- map_dfr(setBetas, function(B0){
  return(data.frame(sim( B0=B0,
                        timeStep=finTime/steps,
                        finTime=finTime,
                         y0 = 1e-9
  ), B0 = B0))
}
)

SusPlot  <- (straightSim |>
  ggplot(aes(time, x, color = as.factor(B0)))
  + geom_line(linewidth = 1)
  + scale_color_manual(values=colorval[seq_len(nlevels(factor(straightSim$B0)))])
  +  labs(x = "time"
       , y = "population proportion \n of susptibles"
       , color = bquote(beta))
  +
  xlim(c(0, xlimits))
  +  guides(color="none")
  +  theme_bw()
  )

IPlot  <- (straightSim |>
  ggplot(aes(time, y, color = as.factor(B0)))
  + geom_line(linewidth = 1)
  + scale_color_manual(values=colorval[seq_len(nlevels(factor(straightSim$B0)))])
  + labs(x = "noramlized time"
       , y = "population proportion \n of infectious"
       , color = bquote(beta))
  + xlim(c(0, xlimits)) 
  + theme_bw() 
  + theme(legend.position = "inside"
        , legend.position.inside = c(0.7, 0.9))
)

(IPlot
      + SusPlot 
      + plot_annotation(tag_levels = "a"))
