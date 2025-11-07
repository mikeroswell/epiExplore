<<<<<<< HEAD
library(shellpipes)
rpcall("v1.kappa.Rout v1.kappa.R")
loadEnvironments()
manageConflicts()
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
startGraphics()

args <- commandArgs(trailingOnly = TRUE)
measure <- args[1]


colorval<- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}

#kd_evolution returns kd over the course of outbreak characterized by 
#either "proportion" (proportion of infected so far relative to the finalSize)
# or "half-day" (number of half-days past the outbreak onset)

kd_evolution<-function(data, typex = "proportion", R0 = 2){
  filtered_data<-data|> filter(type== typex) |> filter(beta == R0)
  ser<-as.array(unique(filtered_data$threshold))
  print(ser)
  a<-map_dfr(ser, function(x){
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

xlabel<-function(type = "proportion"){ifelse(type=="proportion",
      "proportion of cases infected so far relative to the final epidemic size",
      "half-days sinces the outbreak onset")}

plt_kd<- (aa |> 
            mutate(beta = factor(beta))
          |>
            ggplot(aes(x =threshold
                       , y=kd
                       , color =  beta
            ))
          +
            geom_point(size = 3) +
            geom_line() +
            scale_color_manual(values=colorval[seq_len(nlevels((factor(aa$beta))))]) + 
            theme_minimal()
          + labs(x =xlabel(measure) ,
                 y = bquote(kappa[d]),
                 color = bquote(beta)
                 #, title=bquote("case per case distribution for "~R[0]~":"~.(setBeta))
          ) +
            scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
            theme_bw()
)
print(plt_kd)
=======
library(shellpipes)
rpcall("v1.kappa.Rout v1.kappa.R IBM_for_v1.rda")
loadEnvironments()
manageConflicts()
library(ggplot2)
library(dplyr)
library(purrr)
startGraphics()

kd <- function(x){(sd(x)^2-mean(x))/mean(x)^2}

statCohort <- function(halfdeltaT){
  deltaT<- halfdeltaT/2
  startT<-0
  who <- which(startT <= iTime & iTime < startT+deltaT)
  n <- sum(startT <= iTime & iTime < startT+deltaT)
  secDist <- caseTally[who]
  kappa_discrete <- kd(secDist)
  mu <- mean(secDist)
  v <- var(secDist)
  return(data.frame(mu = mu, kappa_discrete=kappa_discrete, variance = v,  n = n
                    ,startT = startT, deltaT = deltaT))
}

stats_cohorts<-case_per_case_overall$halfDayz |> unique() |>map_dfr(statCohort)

v1_k <- (stats_cohorts |> ggplot()
         + aes(x=deltaT, y = kappa_discrete)
         + geom_point()
         + labs(x="time (normalized by infectious duration)"
                ,y=bquote(kappa[discrete])
                ,title=bquote(kappa[discrete]~"over time for"~R[0]~":"~.(setBeta)))
         
         )
print(v1_k)
>>>>>>> refs/remotes/origin/main
