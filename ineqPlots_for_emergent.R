# do some lloyd-smith stuff

# load libraries
library(shellpipes)
manageConflicts()
startGraphics()

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

loadEnvironments()

# a few utility functions
# source("spreadHelpers.R")

# set some integer population size; keep it small for plotting
n <- 3e3



R0 <- seq(0.5, 10, length.out = 6) # this is the rate parameter for an exponential
# aka Beta over Gamma where Beta and Gamma are the SIR rates:
# dI/dt = (Beta*I*S) - Gamma*I
# dR/dt = Gamma*I

# set kappa to 1
JDDisp <- 1

cFracs <- map_dfr(R0, function(brn){
    map_dfr(JDDisp, function(ishape){
        # First, parameterize an underlying thing (gamma, where
        # ishape = shape = 1 is exponential)
        contin <- rgamma(n, shape = 1/ishape, scale = brn*ishape) # this latent
        # distribution is the idealized/expected individual-level reproductive
        # rate. The base case (ishape = 1) is based on imagining individuals
        # falling into the the system described by the canonical ODE SIR model:
        # constant per-capita recovery rate & no individual-level variation in
        # transmission rate either.
        # Lloyd-Smith calls this the "individual reproductive number, *v*"
        # realiz <- rpois(n, lambda = contin) # But these expected, real-numbered
        # reproductive rate is unrealistic. Instead, the actual (integer) number
        # of new infections is assumed to be a random(i.e., Poisson) sample from
        # that idealized rate
        dat <- makeDistData(contin)
        return(data.frame(dat, brn, ishape))
    })
})

# Even though we imagine an infinite susceptible population, we can still
# imagine that the offspring infectees are discrete individuals. In this case,
# increasing R0 decreases the inequality in D0. By the same token, effects of
# reduced dispersion in the intrinsic infectiousness (red lines) are much muted
# by the poisson sampling--- but when the dispersion is higher, everything looks
# dispersed regardless of R0
cFracs %>%
    ggplot(aes(q, cFRealiz, color = brn)) +
    # facet_wrap(~round(ishape,2)) +
    geom_point(size = 0.1) +
    # geom_point(aes(x = q, y = cFIdeal), color = "red", size = 0.05) +
    theme_classic() +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    scale_color_viridis_c() + #trans = "log") +
    labs(x = "fraction contributing"
         , y = "fraction of new infections"
         , color = "R0"
         , title = paste0("Inequality in \"realized\" infectiousness in single-class SIR models\nwith varying R0"
         #                  , "red is idealized\n\npanels"
         #                  , " by dispersion (inverse shape parameter):\n1 "
         # , "is exponential/geometric\n0 is  Dirac/Poisson\nInf is way dispersed"
         )
         )

# But how much of this is simply because of the 0-counts? And, in epidemic data,
# Do we even get a sense of the frequency of 0s? Or do the datasets typicallly
# undercount them?

# anyways, the punchline is that here, we see that when R0 is larger, there is
# more inequality in D0, and when R0<1, the inequality gets small even if there
# is a good bit of dispersion in the underlying expectation. Check JLS paper,
# They probably said exactly that...

cFracs |>
    ggplot(aes(qp, cFRealiz, color = brn)) +
    # facet_wrap(~round(ishape,2)) +
    geom_point(size = 0.1) +
    # geom_line(aes(linewidth = 0.05)) +
    theme_classic() +
    # geom_point(aes(x = q, y = cFIdeal), color = "red", size = 0.05) +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    scale_color_viridis_c() + #trans= "log") +
    labs(x = "fraction of infectors contributing\n"
         , "(those with no infectees removed)"
         , y = "fraction of new infections"
         , color = "R0"
         , title = paste0("Inequality in \"realized\" infectiousness in single-class SIR models\n with varying R0 (not counting 0s)"
         )
    ) +
    xlim(c(0,1))


scaleR <- 3
R_0 <- 10
N <- 1e4
xmax <- 1000
cfThree <- map_dfr(exp(seq(log(0.95), log(0.05), length.out = 5)), function(x){
    dat <- cmptMod(R_0 = R_0
                   , x = x
                   , xChoice = "low"
                   , scaleRNum = scaleR)

    v <- samplePDF(N = N, xmax = xmax, pars = dat, sens = 1e-06)
    counts <- getCounts(v)
    cf <- cumFrac(counts)
    q <- (1:N)/(N)
    return(data.frame(phigh = dat$fracs[3]
                      , cf
                      , q))
})

cfThree |>
    ggplot(aes(q, cf, color =phigh)) +
    geom_point(size = 0.3) +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    theme_classic()+
    labs(x = "fraction of infectors contributing"
         , y = "fraction of new infections"
         , color = "fraction going\nto most\ninfectious\nclass"
         , title = "Inequality in \"realized\" infectiousness in three-class SIR models\nwith fixed R0 = 10, varying the fraction going to the most infectious class"
    ) +
    scale_color_viridis_c(option = "C"
                          #, direction = -1
                          )



threeClass <- map_dfr(seq(0.025, 0.95, 0.025), function(x){
        dat <- cmptMod(R_0 = R_0
                       , x = x
                       , xChoice = "low"
                       , scaleRNum = scaleR)
        kappa <- compVar(dat)/R_0^2
        v <- samplePDF(N = N, xmax = xmax, pars = dat, sens = 1e-06)
        counts <- getCounts(v)
        print(counts)
        cf <- cumFrac(counts)
        print(cf)
        q <- (1:N)/(N)
        DD <- data.frame(cf, q )
        print(DD)
        t20 <- tX(DD, 0.20, cF = "cf")
        t30 <- tX(DD, 0.30, cF = "cf")
        t50 <- tX(DD, 0.50, cF = "cf")
        t80 <- tX(DD, 0.80, cF = "cf")

        return(data.frame(phigh = dat$fracs[3]
                          , kappa
                          , t20
                          , t30
                          , t50
                          , t80))
})


threeClass

tXPlot <- threeClass |>
    pivot_longer(names_to = "ineqName", values_to = "inequality"
                 , cols = c ("t20", "t30", "t50", "t80")
                 , names_prefix = "t") |>
    ggplot(aes(phigh, inequality, color = ineqName)) +
    geom_point() +
    # geom_line() +
    theme_classic() +
    labs(x = "fraction going to most infectious class"
, y = "fraction of new infections caused by top X% infectious individuals"
, color = "X") +
    scale_color_viridis_d(option = "G")

kapPlot <- threeClass |>
    ggplot(aes(phigh, kappa)) +
    geom_point(color = "red") +
    # geom_line() +
    theme_classic() +
    labs(x = "fraction going to most infectious class"
         , y = "kappa")

tXPlot + kapPlot





