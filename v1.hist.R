library(shellpipes)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
loadEnvironments()
manageConflicts()
startGraphics(width=10, height=5)

label_wrap <- function(wrap_level, measure) {
    sapply(wrap_level,function(x){ifelse(measure == "proportion",
           paste(as.numeric(as.character(x)) * 100, "% of finalSize\n infected so far"),
           paste(as.numeric(as.character(x))*10, "% of the \n outbreak duration"))
})}



colorval<- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

IBM_v1_results_rep_mod<-IBM_v1_results_rep |> dplyr::filter(type==measure)
unique_threshold<-sort(unique(IBM_v1_results_rep_mod$threshold))
if(measure=="half-day"){
  IBM_v1_results_rep_modified <-
  IBM_v1_results_rep |> dplyr::filter(type=="half-day",
                                      threshold %in% unique_threshold[c(1,
            median(unique_threshold),length(unique_threshold))])}else{
              IBM_v1_results_rep_modified<- IBM_v1_results_rep |>
                dplyr::filter(type=="proportion"
                              ,threshold %in% unique_threshold[c(1,
                              (length(unique_threshold)+1)/2
                              ,length(unique_threshold))])
              }

plt<- (IBM_v1_results_rep_modified  |> 
         mutate(threshold = factor(threshold, levels
                                   = sort(unique(threshold)))
                , beta = factor(beta))
       |>
         ggplot(aes(x =num_cases
                    , y=after_stat(density)
                    , fill = beta
         ))
       +
         geom_histogram( binwidth = 1, alpha = 1,
                         position="identity") +
         facet_wrap(~ threshold, scales = "free_y"
                   ,labeller = labeller(threshold = function(x){
                     label_wrap(x, measure)}))
                    
       + labs(x = "Cases per case", y = "Frequency", fill = bquote(beta)
              #, title=bquote("case per case distribution for "~R[0]~":"~.(setBeta))
       )
       + theme(strip.text =element_text(size = 7))
       + scale_fill_manual(values=colorval[seq_len(nlevels(factor(IBM_v1_results_rep_modified$beta)))])
       + scale_color_manual(values=colorval[seq_len(nlevels(factor(IBM_v1_results_rep_modified$beta)))])
       + theme_bw()
)

print(plt)




saveEnvironment()

