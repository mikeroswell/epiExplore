library(shellpipes)
rpcall("v1.Rout v1.R IBM_for_v1.rda")
rpcall("v1.hist.Rout v1.hist.R IBM_for_v1.rda")
loadEnvironments()
manageConflicts()
library(ggplot2)
library(dplyr)
library(purrr)
startGraphics()

plt<- (case_per_case_overall |>  mutate(halfDayz = factor(halfDayz, levels
                                                      = sort(unique(halfDayz)))
                                       )
       |>
ggplot(aes(x =as.numeric(num_cases)))
 +
  geom_histogram(aes(weight = count, y=after_stat(density)), binwidth = 1) 
  + facet_wrap(~ halfDayz, labeller = labeller(halfDayz = function(x) {
    paste("normalized half-day # ", x)})  ,scales = "free_y") 
  + labs(x = "cases per case", y = "Frequency"
         , title=bquote("case per case distribution for "~R[0]~":"~.(setBeta)))
  + theme(strip.text =element_text(size = 5))
)

print(plt)



saveEnvironment()

