library(shellpipes)
rpcall("secondaryDistributionPlots.Rout secondaryDistributionPlots.R")
# Plot wrappers

# secondary case histograms


fraction_labels <- function(breaks) {
    sapply(breaks, function(b){b/sum(y)})
}

secDist <- function(dat, caseCol = c("real", "ideal"), caseSource = NULL
                    , yMax = 1, xMax = NULL
                    , breaks = NULL){
    xMax <- ifelse(is.null(xMax)
           , max(dat[[caseCol]])*1.01
           , xMax)
    nPoints <- length(dat[,1])
    yMax <- yMax * nPoints
    breaks <- breaks*nPoints
    dat %>%
        ggplot(aes(x = .data[[caseCol]]
                   # , color = ifelse(is.null({{caseSource}})
                   #                   , "grey"
                   #                   , {{caseSource}})
                   ))+
        geom_histogram() +
        geom_vline(aes(xintercept = mean(.data[[caseCol]]), color = "red")) +

        labs(x = "expected new cases per infectious individual", y = "fraction of infected individuals") +
        scale_y_log10(breaks = breaks, labels = function(x){x/nPoints}, limits = c(NA, yMax))+
        theme(legend.position = "none") +
        xlim(NA, xMax)
}

cFPlot <- function(cFDat, showRealized = TRUE){
    p <- cFDat %>%
           ggplot(aes(q, cFIdeal))+
        geom_hline(yintercept = 0.8, linetype = "dashed" ) +
        geom_vline(xintercept = 0.2, linetype = "dashed") +
        geom_point(size = 0.6) +
           labs(x = "fraction of most infectious individuals"
                , y = "fraction of new infections")
    if(showRealized){p <- p + geom_point(aes(y = cFRealiz)
                                         , color = "grey", size = 1) }
    return(p)
}
saveEnvironment()