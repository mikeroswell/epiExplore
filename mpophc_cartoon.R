# Little fig for mpophc
library(dplyr)
library(purrr)
library(ggplot2)
# three groups with different R, compiled into one inequality cumsum curve


inputs <- data.frame(infection = c("asymptomatic", "mild", "severe")
                     , groupR = c(29, 5, 1.5)
                     , p = c(0.15, 0.5, 0.35)
)

outFun <- function(n, dat){
    rows <- length(dat[,1])
    map_dfr(1:rows, function(grp){
        grpP <- round(n*dat[grp,"p"])
        bind_cols(dat[grp,]
                   , cases = rgeom(grpP, prob = 1/dat[grp, "groupR"])
                   )
       
    })
}


pltDat <- outFun(5e3, inputs)


pdf("cartoon_8020_v2.pdf", height = 4, width = 4)
pltDat[sample(1:length(pltDat[[1]])),] %>%
    
    arrange(desc(cases)) %>% 
    mutate(cFrac = cumsum(cases)/sum(cases)
           , pFrac = row_number(desc(cases))/length(cases)) %>% 
    ggplot(aes(pFrac, cFrac, color = infection)) +
    scale_color_manual(values = c("red","purple", "blue" )) +
    # geom_hline(yintercept = 0.8, linetype = "dashed") + 
    # geom_vline(xintercept = 0.2, linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_point(alpha = 0.3) + 
    geom_point(x = 0.2, y = 0.8, shape = 18, size = 5, color = "black") +
    annotate("text", x = 0.1, y = 0.9, label = "\"80:20\nrule\"")+
    annotate("text", x = 0.5, y = 0.5, label = "\n1:1 line", angle = 45
             # , vjust = 1
             )+
    theme_classic() +
    labs(x = "fraction of ascending rank\norder of secondary infections", y = "fraction of total infections") +
    theme(legend.position = "none")
dev.off()
