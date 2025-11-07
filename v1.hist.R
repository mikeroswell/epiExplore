library(shellpipes)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
loadEnvironments()
manageConflicts()
startGraphics()


measure<-"proportion"
label_wrap <-function(x, measure){
  ifelse(measure == "proportion", paste(as.numeric(x)*100
        ,"% of finalSize\n infected so far"), paste(as.numeric(x), 
                                                   "half-days since outbreak onset"))}

colorval<- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
#plotting the histogram
plt<- (IBM_v1_results_rep |> filter(type == measure) |> 
         mutate(threshold = factor(threshold, levels
                                   = sort(unique(threshold)))
                , beta = factor(beta))
       |>
         ggplot(aes(x =num_cases
                    , y=after_stat(density)
                    , fill = beta
         ))
       +
         geom_histogram( binwidth = 1, alpha = 0.8,
                         position="identity") 
       + facet_wrap(~ threshold, scales = "free_y"
                    ,  labeller = labeller(threshold = function(x){label_wrap(x, measure)})
       ) 
       + labs(x = "cases per case", y = "Density", fill = bquote(beta)
              #, title=bquote("case per case distribution for "~R[0]~":"~.(setBeta))
       )
       + theme(strip.text =element_text(size = 7))
       + theme_bw()
       + scale_fill_manual(values=colorval[seq_len(nlevels(factor(IBM_v1_results_rep$beta)))])
)

print(plt)




saveEnvironment()

