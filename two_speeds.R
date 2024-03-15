# load libraries
library(dplyr)
library(purrr)
library(ggplot2)

# set parameter values
# epi parameters, beta - transmissibility, gamma - recovery, p are the different
# 'classes'

# First class here has medium contact rates and fast recovery
# Second class has medium contact rates but slow recovery
# Third class looks like trouble... really high contact rates and SLOW recovery; 
# They are, on average, the super-spreaders
pars.beta <- c(0.5, 0.5, 4) 
pars.gamma <- c(0.5, 0.25, 0.1)
pars.p_vals <- c(0.1, 0.8, 0.1)
pars.max_k <- 1000
k <- 0:pars.max_k

# Define p(k)
# I am assuming a sum of geometric distributions, each with rate beta (above) 
# for a duration gamma, and therefore we get the per-class distribution of 
# infectees


qdata <- map_dfr(1:length(pars.p_vals), function(idx){
    q_inf <- pars.p_vals[idx]* # fraction of the total population in class with 
        # this set of infectivity (beta) and recovery rate (gamma)
        (pars.beta[idx]/(pars.beta[idx]+pars.gamma[idx]))^k * # probability k
        # individuals are infected (k failures)
    
        (pars.gamma[idx]/(pars.beta[idx]+pars.gamma[idx]))  # probability of 
    # recovery (a single success)
    data.frame(q_inf, kdex = 0:pars.max_k, idx)
})

qdata <- qdata %>% mutate(csq = cumsum(q_inf))

# plot what I just made
qdata %>% ggplot(aes(q_inf, fill = as.factor(idx))) + 
    geom_histogram() + 
    scale_y_sqrt() + 
    theme_classic() 

qdata %>% ggplot(aes(csq, fill = as.factor(idx))) + 
    geom_histogram() + 
    scale_y_sqrt() + 
    theme_classic() 


# now plot how jsw was thinking about it (cf fig 1b in Lloyd-Smith et al. 2005)
qdata_sum <- qdata %>% 
    group_by(kdex) %>% 
    summarize(q_inf_sum = sum(q_inf)) %>% 
    mutate(revq = rev(q_inf_sum)
           , revk = rev(kdex)
           , q_inf_cumsum = cumsum(revq)
           , q_num = cumsum(revq*revk))

tot_val <- sum(qdata_sum$q_inf_sum*k)
qdata_sum <- qdata_sum %>% mutate(q_prop = q_num/tot_val)

pdf("fast_and_slow_jsw.pdf")
qdata_sum %>% ggplot(aes(q_inf_cumsum, q_prop)) + 
    geom_line() +
    theme_classic() +
    geom_hline(yintercept = 0.8, linetype = 2) +
    geom_vline(xintercept = 0.2, linetype = 2) +
    labs(x = "fraction of population (ordered by transmission rank)", y = "fraction of infections attributable to members of x")
dev.off()
