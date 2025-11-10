library(shellpipes)
#rpcall("v1.kappa.Rout v1.kappa.R")
loadEnvironments()
manageConflicts()
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
startGraphics()


colorval<- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}

#kd_evolution returns kd over the course of outbreak characterized by 
#either "proportion" (proportion of infected so far relative to the finalSize)
# or "half-day" (ratio of time past since outbreak to the entire outbreak duration)

kd_evolution<-function(data, typex = "proportion", R0 = 2){
  filtered_data<-data|> filter(type== typex, beta == R0)
  a<-map_dfr(unique(filtered_data$threshold), function(x){
    filtered_data_x <-filtered_data  |>
      filter(threshold == x)
    return(data.frame(threshold = x,
                      beta=R0,
                      type = typex,
                      kd=kd(filtered_data_x$num_cases)
    ))
  })
}
aa<-map_dfr(unique(IBM_v1_results_rep$beta),
            function(x){kd_evolution(IBM_v1_results_rep,
                                     typex = measure,
                                     R0 = x)})

xlabel<-function(pairs, type = "proportion"){ifelse(type=="proportion",
      "proportion of cases infected so far relative to the final epidemic size",
      paste(
        "percentage of the outbreak duration",
        "(the outbreak duration: the time from the outbreak onset to",
        " the first time the number of infectious individuals hits zero)",
        "(beta, outbreak duration):",
        paste(pairs, collapse = ", "),
        sep = "\n"
      )
      
      )
}

df<- map_dfr(setBetas, function(x){
             IBM_v1_results |>
               filter(beta == x) |>
               summarise(beta = x, max_day = max(day, na.rm = TRUE))}
)
pairs <- with(df, paste0("(", beta, ", ", max_day, ")"))

plt_kd<- (aa |> 
            mutate(beta = factor(beta))
          |>
            ggplot(aes(x =ifelse(type=="half-day", 10*threshold, threshold)
                       , y=kd
                       , color =  beta
            ))
          +
            geom_point(size = 3) +
            geom_line() +
            scale_color_manual(values=colorval[seq_len(nlevels((factor(aa$beta))))]) + 
            theme_minimal()
          + labs(x =xlabel(pairs, measure) ,
                 y = bquote(kappa[d]),
                 color = bquote(beta)
                 #, title=bquote("case per case distribution for "~R[0]~":"~.(setBeta))
          ) +
            scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
            theme_bw()
)
print(plt_kd)
