library(ggplot2)
library(tidyr)
library(purrr)
library(bbmle)
library(dplyr)

plots <- map(c("1p5", "2", "3", "6", "12"), function(R0){
  fi <- paste0("change_", R0, ".toPeak.rda")
  load(fi)
  df <- data.frame(table(dat$secDist)/length(dat$secDist))
  names(df) <- c("count", "SIR")
  df <- df |> mutate(count = as.numeric(as.character(count)))
  maxc <- max(df$count)
  df <- bind_rows(df, data.frame(count = 1:maxc, SIR = rep(0, maxc))[! 1:maxc %in% df$count,])

  nbd <- dnbinom(0:maxc, size = 1/coef(MLE)[[2]], mu = exp(coef(MLE)[[1]]))
  df <- data.frame(df, nbd = nbd)

  p <- df |>
    pivot_longer(cols = c("SIR", "nbd"), values_to = "density", names_to = "model") |>     ggplot(aes(count, density, color = model)) +
    geom_point() +
    scale_y_log10() +
    theme_classic() +
    labs(title = paste0("R0 = ", R0))

  print(coef(MLE))
  print(ktdt(dat))
  print("---------------")
  return(p)
})

print(plots)

