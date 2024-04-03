
library(dplyr)

n <- 1e6
shape <- 0.4
prob <- 0.6

ournbinom <- function(n, shape, prob){
	return(rpois(n, lambda = rgamma(n, shape=shape, rate=prob/(1-prob))))
}

t <- (
	tibble(
		rnb = sort(rnbinom(n=n, size=shape, prob=prob))
		, ournb = sort(ournbinom(n=n, shape=shape, prob=prob))
	) |> group_by(rnb, ournb) |>  tally()
)

print(t)


