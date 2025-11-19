library(ggplot2); theme_set(theme_bw() +theme(
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
startGraphics(width=10, height=10)




wrap_f <- function(wrap_level, B0) {
  sapply(wrap_level,function(x){
  paste("Cohorts infected prior to time", x, "are considered")
  })}
lineType <- "dotted"
interceptLineWidth <- 0.5 #intercept's line width
LineWidth <-1 #lines' line width
############### Bar Plot ########################
stackbar_vRc<-( res_mat    |> pivot_longer(cols = c(between, within),
                                       names_to = "source",
                                       values_to = "RcVariance")
            |> mutate(cutoffTime = factor(cutoffTime
                      , levels = sort(unique(cutoffTime)))
                      , B0 = as.factor(B0) )|>
              ggplot()
            + aes(x = B0, y = RcVariance,  fill = source)
            +  geom_bar(stat = "identity", position = "stack")
            + geom_hline (yintercept = 1, linetype=lineType
                          , linewidth=interceptLineWidth)
            + facet_wrap(~ cutoffTime
                         , labeller = labeller(cutoffTime = function(x){
                wrap_f(x)
              })
              )
            + scale_color_viridis_d()
            +  ylab(bquote(sigma^2~"="~sigma["bet"]^2~"+"~sigma["with"]^2))
            + xlab(bquote(beta))
            + scale_fill_discrete(labels=c(bquote("between"~"("~sigma["bet"]^2~")"),
                                           bquote("within"~"("~sigma["with"]^2~")")  ))
            + theme(axis.title.y = element_text(size = 10)
                    ,strip.text.x = element_text(size = 8))
)
stackbar_kRc<-( res_mat |> pivot_longer(cols = c(betweenKRc, withinKRc),
                                        names_to = "source",
                                        values_to = "KRc")
                |> mutate(cutoffTime = factor(cutoffTime
                                              , levels = sort(unique(cutoffTime)))
                          , B0 = as.factor(B0) )|>
                  ggplot()
                + aes(x = B0, y = KRc,  fill = source)
                +  geom_bar(stat = "identity", position = "stack")
                + geom_hline (yintercept = 1, linetype=lineType
                              , linewidth=interceptLineWidth)
                + facet_wrap(~ cutoffTime
                             , labeller = labeller(cutoffTime = function(x){
                               wrap_f(x)
                             })
                )
                + scale_color_viridis_d()
                +  labs(y=bquote(kappa)
                        , x = bquote(beta)
                )
                + scale_fill_discrete(
                  labels=c(bquote("between"~"("~sigma["bet"]^2~")/"~mu^2),
                                                   bquote("within"~"("~sigma["with"]^2~")/"~mu^2)  ))
                + theme(axis.title.y = element_text(size = 10)
                        ,strip.text.x = element_text(size = 8))
)
bar_muRc <-( res_mat  |> mutate(cutoffTime = factor(cutoffTime
                          , levels = sort(unique(cutoffTime)))
                          , B0 = as.factor(B0) )|>
                  ggplot()
                + aes(x = B0, y = muRc)
                +  geom_bar(stat = "identity")
                + geom_hline (yintercept = 1, linetype=lineType
                              , linewidth=interceptLineWidth)
                + facet_wrap(~ cutoffTime
                             , labeller = labeller(cutoffTime = function(x){
                               wrap_f(x)
                             })
                )
                + scale_color_viridis_d()
                +  labs(y=bquote(mu)
                        , x = bquote(beta)
                )
                + theme(axis.title.y = element_text(size = 10)
                        ,strip.text.x = element_text(size = 8))
)

############### Final Plot #############
print(bar_muRc / stackbar_vRc / stackbar_kRc
      + plot_annotation(tag_levels ="a")
      + plot_layout(heights=c(1,1,1) )
 )


#saveEnvironment()
