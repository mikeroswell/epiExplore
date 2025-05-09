# do some lloyd-smith stuff

# load libraries
library(shellpipes)
rpcall("myLloyd-Smith.Rout myLloyd-Smith.R spreadHelpers.rda")
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
loadEnvironments()
startGraphics()



# set some integer population size; keep it small for plotting
n <- 3e3




# set the dispersion parameter JD-style such that the limiting case of 0 gets us
# to poisson
JDDisp <- c(exp(seq(-4, 4, length.out = 12)))


R0 <- seq(0.5, 3, length.out = 6) # this is the rate parameter for an exponential
# aka Beta over Gamma where Beta and Gamma are the SIR rates:
# dI/dt = (Beta*I*S) - Gamma*I
# dR/dt = Gamma*I




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


Kappas <- map_dfr(R0, function(brn){
    map_dfr(JDDisp, function(ishape){
        # First, parameterize an underlying thing (gamma, where
        # ishape = shape = 1 is exponential)
        contin <- rgamma(n, shape = 1/ishape, scale = brn*ishape)
        # poissonify (nb)
        realiz <- rpois(n, contin)
        dat <- makeDistData(contin)
        t20_activity <- tX(dat, X = 0.2)
        t20_realized <- tX(dat, X = 0.2, cF = "cFRealiz")
        Kappa <- ishape
        R0 <- brn
        return(data.frame(t20_activity, t20_realized, Kappa, R0))
    })
})


head(Kappas)

Kappas %>%
    pivot_longer(cols = starts_with("t20")
                 , names_to = "heterogeneityType"
                 , values_to = "t20") %>%
    ggplot(aes(Kappa, t20, color = heterogeneityType ))+
    facet_wrap(~R0, labeller = "label_both") +
    geom_point() +
    theme_classic() +
    scale_x_log10() +
    scale_color_manual(values = c("purple3", "lightseagreen")) # +
    # scale_y_log10()


# Plots are for a counter-factual universe where we imagine the offspring
# distribution _at the beginning of an outbreak_ where susceptible are infinite
# This is conceptually analogous to R0, but instead of only considering the
# expected average infections for a typical individual in an entirely
# susceptible population, now we are thinking about the expected *distribution*
# of individuals in that entirely susceptible population. Maybe `D_0`?


# pdf("MyLloydSmith.pdf")
# in an abstract sense, the expectation of D0 doesn't vary with R0
cFracs %>%
    ggplot(aes(q, cFIdeal, color = brn)) +
    facet_wrap(~round(ishape,2)) +
    geom_point(size = 0.1) +
    # geom_line(aes(linewidth = 0.05)) +
    theme_classic() +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    scale_color_viridis_c(trans = "log") +
    labs(x = "fraction contributing"
         , y = "expected fraction of new infections"
         , color = "R0"
         , title = paste0("Inequality in \"intrinsic\" infectiousness\n\npanels"
                          , " by dispersion (inverse shape parameter):\n1"
                          , " is exponential/geometric\n0 is  Dirac/Poisson\n"
                          , "Inf is way dispersed"
         )
    )

# Even though we imagine an infinite susceptible population, we can still
# imagine that the offspring infectees are discrete individuals. In this case,
# increasing R0 decreases the inequality in D0. By the same token, effects of
# reduced dispersion in the intrinsic infectiousness (red lines) are much muted
# by the poisson sampling--- but when the dispersion is higher, everything looks
# dispersed regardless of R0
cFracs %>%
    ggplot(aes(q, cFRealiz, color = brn)) +
    facet_wrap(~round(ishape,2)) +
    geom_point(size = 0.1) +
    geom_point(aes(x = q, y = cFIdeal), color = "red", size = 0.05) +
    theme_classic() +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    scale_color_viridis_c(trans = "log") +
    labs(x = "fraction contributing"
         , y = "fraction of new infections"
         , color = "R0"
         , title = paste0("Inequality in \"realized\" infectiousness\n\n"
                          , "red is idealized\n\npanels"
                          , " by dispersion (inverse shape parameter):\n1 "
         , "is exponential/geometric\n0 is  Dirac/Poisson\nInf is way dispersed"
         )
         )

# But how much of this is simply because of the 0-counts? And, in epidemic data,
# Do we even get a sense of the frequency of 0s? Or do the datasets typicallly
# undercount them?

# anyways, the punchline is that here, we see that when R0 is larger, there is
# more inequality in D0, and when R0<1, the inequality gets small even if there
# is a good bit of dispersion in the underlying expectation. Check JLS paper,
# They probably said exactly that...

cFracs %>%
    ggplot(aes(qp, cFRealiz, color = brn)) +
    facet_wrap(~round(ishape,2)) +
    geom_point(size = 0.1) +
    # geom_line(aes(linewidth = 0.05)) +
    theme_classic() +
    geom_point(aes(x = q, y = cFIdeal), color = "red", size = 0.05) +
    geom_hline(yintercept = 0.8, color = "grey") +
    geom_vline(xintercept = 0.2, color = "grey") +
    scale_color_viridis_c(trans= "log") +
    labs(x = "fraction of infectors contributing\n"
         , "(those with no infectees removed)"
         , y = "fraction of new infections"
         , color = "R0"
         , title = paste0("Inequality in \"realized\" infectiousness\n(counting"
                          , " only those with >0 offspring)\n\nred is idealized"
                          , "\n\npanels by dispersion (inverse shape parameter)"
                          , ":\n1 is exponential/geometric\n0 is  Dirac/Poisson"
                          , "\nInf is way dispersed"
         )
    ) +
    xlim(c(0,1))
# dev.off()






