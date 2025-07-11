library(purrr)
library(dplyr)

early <- map_dfr(c("1p5", "2", "3", "6", "12"), function(R0){
  fi <- paste0("change_", R0, ".hundredFiveHundred.rda")
  load(fi)
  earlyStats <- earlyStats |> select(R0, mu, kappa, peakFrac)
  print(table(states))
  print(I)
  return(earlyStats)
})
print(early)
write.csv( early, "outputs/early.csv", row.names = FALSE)
