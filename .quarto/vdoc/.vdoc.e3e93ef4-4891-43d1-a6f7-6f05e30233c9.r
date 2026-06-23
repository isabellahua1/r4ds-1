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
# Plot distribution of song.year, excluding year = 0
year_data <- music %>%
  filter(song.year != 0)

year_count <- nrow(year_data)

ggplot(year_data, aes(x = song.year)) +
  geom_histogram(binwidth = 5, color = "white", fill = "#2c7fb8") +
  labs(
    title = "Distribution of song release years",
    subtitle = paste("Number of songs:", year_count),
    x = "Year",
    y = "Count"
  ) +
  theme_minimal()
#
#
#
#
