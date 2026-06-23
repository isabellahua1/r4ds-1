#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
#
#
#
#
#| message: false
music <- read_csv("data/music.csv")
music %>%
  select(artist.familiarity, artist.hotttnesss, song.year, song.tempo) %>%
  summarise(
    across(everything(), list(
      min = ~min(., na.rm = TRUE),
      q1 = ~quantile(., 0.25, na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      q3 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ))
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("variable", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value)
#
#
#
#
