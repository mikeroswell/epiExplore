library(ggplot2)
library(dplyr)

set.seed(22)

# Example data
df <- data.frame(
  x = rep(1:10, 2),
  y = c(runif(10, 1, 3), runif(10, 3, 5)),
  pVar = rep(c("A", "B"), each = 10),
  cVar = rep(c("a", "b"), 10)
) %>%
  mutate(grp = paste(pVar, cVar, sep = "_"))

# set point size
sz <- 4

# Base plot
ggplot(df, aes(x, y, group = grp)) +
  # Path for group A (cVar) with color mapping for group (cVar)
  geom_path(aes(color = cVar
                , alpha = as.numeric(pVar == "A")
                , group = grp)
            , linewidth = 1) +
  # Points for group B (pVar) with fill based on cVar
  geom_point(aes(fill = cVar
                 , alpha = as.numeric(pVar == "B")
                 , group = grp
  )
  , shape = 21
  , size = sz
  , color = scales::alpha("white", alpha = 0)
  ) +

  # set up a guide for the plotting type, using the alpha scale that renders
  # outside type invisible
  scale_alpha_identity(breaks = c(0,1)
                       , labels = c("A", "B")
                       , guide = guide_legend(title = "type"
                                              , order = 1
                                              , override.aes = list(
                                                linewidth = c(0.8, 0)
                                                , shape = c(15, 21)
                                                , linetype = c("solid", "blank")
                                                , fill = c(NA, "grey")
                                                , size = c(0, sz)
                                                , alpha = c(1)

                                              )
                       )
  ) +
  scale_color_discrete(guide = guide_legend(order = 2)) +
  scale_fill_discrete(guide = guide_legend(order = 2)) +
  theme_classic()

