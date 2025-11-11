library(shellpipes)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
loadEnvironments()
startGraphics()
manageConflicts()
colorval<- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
filtered_data <- IBM_v1_results_rep |> filter(type == "half-day") |> 
  distinct(beta, threshold, .keep_all = TRUE)

plt<- (filtered_data |> mutate(beta = factor(beta)) |>
         ggplot(aes(x = cmpt, y = 10*threshold, color = beta)) 
  + geom_point(size = 3)
  + geom_line()
  + labs(x = "proportion of infected so far to the final epidemic size"
         , y = "percentage of the outbreak duration"
         , color = bquote(beta))
 + scale_color_manual(values=colorval[seq_len(nlevels((factor(filtered_data$beta))))])
)
print(plt)
