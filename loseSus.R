library(shellpipes)
rpcall("loseSus.Rout loseSus.R kappaFns.rda")
# check whether {remotes} is installed
# if (!require("remotes")) install.packages("remotes")
# remotes::install_github("epiverse-trace/epichains")
library(epichains)
library(dplyr)
library(tidyr)
# library(ggplot2)
# startGraphics()
loadEnvironments()

set.seed(32)
# Define generation time
generation_time_fn <- function(n) {
  gt <- rep(3, n)
  return(gt)
}

sim_chains_with_pop <- simulate_chains(
  pop = 10000,
  n_chains = 1000,
  percent_immune = 0,
  offspring_dist = rnbinom,
  statistic = "size",
  size = 1, mu = 3,
  generation_time = generation_time_fn
)

head(sim_chains_with_pop)

caseCount <- sim_chains_with_pop |>
  group_by(chain) |>
  mutate(ghost = !(infectee %in% infector)) |>
  (\(data) {
    ghosts <- data |>
      filter(ghost) |>  # Select only ghost rows
      transmute(
        chain = chain,
        infector = infectee,  # Ghosts become the infectors
        infectee = 0,         # Infectee is labeled as 0
        generation = NA,      # Generation is NA for ghosts
        time = NA             # Time is NA for ghosts
      )
    bind_rows(data, ghosts)
  })() |>

# also works, but not with native pipe
# mutate(ghost = !(infectee %in% infector)) %>%
#   {
#     ghosts <- filter(., ghost) %>% # Select only ghost rows
#       transmute(
#         chain = chain,
#         infector = infectee,  # Ghosts become the infectors
#         infectee = 0,         # Infectee is labeled as 0
#         generation = NA,      # Generation is NA for ghosts
#         time = NA             # Time is NA for ghosts
#       )
#   bind_rows(., ghosts)
# } |>
  group_by(chain, infector) |>
  summarize(cases = sum(infectee>0))

# caseCount |>
#   ggplot(aes(cases, fill = as.factor(chain))) +
#   geom_histogram(position = "stack", breaks = seq(-1/2, 20+1/2, 1)) +
#   theme_classic() +
#   scale_color_viridis_d()


 fastSimOut <- caseCount |>
  ungroup() |>
  group_by(chain) |>
  summarize(out = kapSum(cases)) |>
  tidyr::unpack(cols = out)


summary(fastSimOut)
cor(fastSimOut )

