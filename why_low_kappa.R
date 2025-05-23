# gut check on low kappas
library(shellpipes)
rpcall("why_low_kappa.Rout why_low_kappa.R")
manageConflicts()
startGraphics()
library(purrr)
library(ggplot2)
library(bbmle)
library(RTMB)

# define NLL for kappa
kapNB <- function(x, kap, M){
  -sum(dnbinom(x, mu = M, size = 1/kap, log = TRUE))

}

kapNB2 <- function(x, kap, M) {
  -sum(dnbinom2(x, mu = M, var = M*(1+kap*M), log = TRUE))
}

set.seed(1905)
# function to compute MLE kappa with CI
kapEst <- function(x){
  mlefit <- bbmle::mle2(kapNB2
              , start = list(kap = 0.5, M = 1.5)
              , data = list(x = x)
              , method = "L-BFGS-B"
              # , optimizer = "nlminb"
              , lower = c(1e-9, 1e-9)
              )
  est <- as.numeric(coef(mlefit)[1])
  ci <- confint(profile(mlefit))
  return(list(est = est, lcl = ci[1], ucl = ci[2] ))
}

nreps <- 50

mysim <- map_dfr(1:nreps, function(nr){
  n <- 6
  gamm <- 1/3
  rtimes <-rexp(n = n, gamm)
  rtimes

  cd <- rpois(n = n, rtimes)
  mu <- mean(cd)
  V <- var(cd)
  kapNaive <- (V-mu)/mu^2
  kapMLE <- kapEst(cd)
  return(data.frame(mu, V, kapNaive
                    , kapMLE = kapMLE$est, lcl = kapMLE$lcl, ucl = kapMLE$ucl))
})

mysim |> ggplot(aes(mu, kap)) +
  geom_point(alpha = 0.04) +
  theme_classic() +
  geom_hline( yintercept = mean(mysim$kap, na.rm = TRUE), color = "red") +
  geom_vline(xintercept = mean(mysim$mu, na.rm = TRUE), color = "red") +
  geom_hline(yintercept = 1, color ="blue", linetype = 2)+
  geom_vline(xintercept = 3, color ="blue", linetype = 2)+
  labs(x = "mean cases per case"
       , y = "kappa for 2ary cases from 6 individuals"
       , title = "R_0 = 3\n blue is platonic, red observed")

# not because of a mistake with sampling
# but this stuff might be using a different parameterization than R's

# geomsim <- map_dfr(1:nreps, function(nr){
#   n <- 6
#   gamm <- 0.3
#   cd <-rgeom(n = n, prob = gamm/(1-gamm))
#   mu <- mean(cd)
#   sig <- sd(cd)
#   kap <- (sig^2-mu)/mu^2
#   return(data.frame(mu, sig, kap))
# })
#
# geomsim |> ggplot(aes(mu, kap)) +
#   geom_point(alpha = 0.3) +
#   theme_classic()
# # scale_x_log10()+
# # scale_y_log10()
# mean(geomsim$kap, na.rm = TRUE)
# geom3 <- rgeom(5000, p = 1/4) #
# mean(geom3) # 3 as expected
# # I think this is what Wikipedia box suggests
# mean(geom3)^2-mean(geom3) # ~ 6,
# sd(geom3)^2 # ~12
# # but that matches Jonathan's fix
# mean(geom3)^2+mean(geom3) # ~ 12


# geometric mean and variance
#
# cvGeom <- function(mu){
#   v <- mu^2+mu
#   kappa <- (v-mu)/mu^2
#   CV <- sqrt(v)/mu
#   return(data.frame(v, kappa, CV))
# }
#
# checkGeom<-map_dfr(seq(1,50, 1), function(R){
#   data.frame(R, cvGeom(R))
# })
#
#
# checkGeom |> ggplot(aes(R, kappa)) +
#   geom_point() +
#   geom_point(aes(y=CV^2), color = "blue")+
#   theme_classic() +
#   scale_y_log10() +
#   labs(y = "kappa_discrete (black)\n CV^2 (blue)"
#        , title = "kappa_discrete and CV^2 for geometric with mean R")
#
