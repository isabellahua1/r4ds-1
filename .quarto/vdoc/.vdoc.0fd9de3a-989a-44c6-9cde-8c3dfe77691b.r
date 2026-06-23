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
# Display human-readable columns
music %>%
  select(
    artist.name,
    artist.location,
    artist.latitude,
    artist.longitude,
    artist.terms,
    artist.familiarity,
    artist.hotttnesss,
    song.title,
    song.year
  ) %>%
  head(20)
#
#
#
# Count rows with placeholder values
music %>%
  summarise(
    location_empty = sum(artist.location == "" | is.na(artist.location)),
    release_empty = sum(release.name == "" | is.na(release.name)),
    title_empty = sum(song.title == "" | is.na(song.title)),
    year_zero = sum(song.year == 0, na.rm = TRUE),
    familiarity_zero = sum(artist.familiarity == 0, na.rm = TRUE),
    hotttnesss_zero = sum(artist.hotttnesss == 0, na.rm = TRUE)
  )
```{r}
# Count artists with usable coordinates vs placeholder coordinates
music %>%
  mutate(
    usable_coords = !(artist.latitude == 0 & artist.longitude == 0),
    placeholder_coords = (artist.latitude == 0 & artist.longitude == 0)
  ) %>%
  summarise(
    usable_coordinates = sum(usable_coords, na.rm = TRUE),
    placeholder_coordinates = sum(placeholder_coords, na.rm = TRUE)
  )
#
#
#
#
#
#
#
#| message: false
# World map of artist locations (excluding placeholder 0,0 coordinates)
library(maps)
# color artist points by song.year (exclude year == 0)
artist_coords <- music %>%
  distinct(artist.name, artist.latitude, artist.longitude, song.year) %>%
  filter(!is.na(song.year), song.year != 0) %>%
  filter(!is.na(artist.latitude), !is.na(artist.longitude)) %>%
  filter(!(artist.latitude == 0 & artist.longitude == 0))

n_artists <- nrow(artist_coords)

# map background (base graphics)
map("world", fill = TRUE, col = "gray95", bg = "white")

# color palette for years
years <- artist_coords$song.year
pal <- colorRampPalette(c("#4575b4", "#91bfdb", "#ffffbf", "#fee090", "#fc8d59", "#d73027"))
cols <- pal(length(unique(years)))[as.integer(factor(years))]

points(artist_coords$artist.longitude, artist_coords$artist.latitude,
       pch = 21, bg = cols, col = "#333333", cex = 0.8)

# legend: show min and max year
yr_min <- min(years, na.rm = TRUE)
yr_max <- max(years, na.rm = TRUE)
legend_text <- paste0("song.year: ", yr_min, " — ", yr_max)
legend("topright", legend = legend_text, bty = "n")

title(main = "Artist Locations (colored by song.year)",
      sub = paste("Plotted artists:", n_artists, "— colors show song release year; rows with song.year = 0 are excluded; coverage limited by available coordinates."))
#
#
#
#
