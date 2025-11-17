library(ggplot2); theme_set(theme_bw() + theme(
  plot.background = element_blank(),
  panel.grid = element_blank(),
) )
library(dplyr)
library(patchwork)
library(tidyr)
library(deSolve)
library(purrr)
library(shellpipes)

loadEnvironments()
startGraphics(width=10, height=5)

lineType <- "dotted"
interceptLineWidth <- 0.5 #intercept's line width
LineWidth <-1 #lines' line width
pointSize <- 1.5
############### Time Plot ########################
res_mat_mutated_2 <- res_mat |> mutate(
                            B0 = as.factor(B0) )
########### Rc and kappa_c over time #########
colorVec<- c("#F8766D", "#00BFC4", "#7B3294", "#E69F00", "#56B4E9"
             ,"#009E73", "#0072B2", "#D55E00", "#CC79A7")
cohortXlabel <- "infection time cutoff"
mu_Rc <- (res_mat_mutated_2 |> ggplot(aes(cutoffTime, muRc
                                          , color = as.factor(B0))) 
  + geom_point(size = pointSize)
  + geom_line(linewidth = LineWidth) 
  + geom_hline(yintercept = 1, linetype=lineType
               , linewidth=interceptLineWidth)
  + scale_color_manual(values = colorVec)
  + guides(color = "none") 
  + xlim(c(0, temporalFinalTime)) 
  + labs(x = cohortXlabel
       , y = bquote(mu)
       , color = bquote(beta)))


var_Rc<- (res_mat_mutated_2 |> ggplot(aes(cutoffTime, totalVRc
                                          , color = as.factor(B0))) 
        + geom_point(size = pointSize)
       + geom_line(linewidth = LineWidth) 
       + geom_hline(yintercept = 1, linetype=lineType
                    , linewidth=interceptLineWidth)
       + scale_color_manual(values = colorVec)
       + guides(color = "none") 
       + xlim(c(0, temporalFinalTime)) 
       + labs(x = cohortXlabel
              , y = bquote(sigma^2)
              , color = bquote(beta)))

var_Rc_bet<- (res_mat_mutated_2 |> ggplot(aes(cutoffTime, between
                                          , color = as.factor(B0)))
          + geom_point(size = pointSize)
          + geom_line(linewidth = LineWidth) 
          + geom_hline(yintercept = 1, linetype=lineType
                       , linewidth = interceptLineWidth)
          + scale_color_manual(values = colorVec)
          + guides(color = "none") 
          + xlim(c(0, temporalFinalTime)) 
          + labs(x = cohortXlabel
                 , y = bquote(sigma["bet"]^2)
                 , color = bquote(beta)))

var_Rc_with<- (res_mat_mutated_2 |> ggplot(aes(cutoffTime, within
                                              , color = as.factor(B0)))
              + geom_point(size = pointSize)
              + geom_line(linewidth = LineWidth) 
              + geom_hline(yintercept = 1, linetype=lineType
                           , linewidth = interceptLineWidth)
              + scale_color_manual(values = colorVec)
              + guides(color = "none") 
              + xlim(c(0, temporalFinalTime)) 
              + labs(x = cohortXlabel
                     , y = bquote(sigma["with"]^2)
                     , color = bquote(beta)))

kappa_Rc<- (res_mat_mutated_2 |> ggplot(aes(cutoffTime, totalKRc
                                          , color = as.factor(B0))) 
          + geom_point(size = pointSize)
          + geom_line(linewidth = LineWidth) 
          + geom_hline(yintercept = 1, linetype=lineType
                       , linewidth=interceptLineWidth)
          + scale_color_manual(values = colorVec)
          + xlim(c(0, temporalFinalTime)) 
          + labs(x = cohortXlabel
                 , y = bquote(kappa)
                 , color = bquote(beta)))


cohortFig <- mu_Rc + var_Rc + var_Rc_bet + var_Rc_with + kappa_Rc

############### Final Plot #############
print(cohortFig 
      + plot_annotation(tag_levels ="a")
     # + plot_layout(heights=c(2,1,1) )
 )


#saveEnvironment()
