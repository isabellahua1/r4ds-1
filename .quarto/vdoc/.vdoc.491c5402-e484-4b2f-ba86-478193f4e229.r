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
#| message: false
music <- read_csv("data/music.csv")
music %>%
  select(starts_with("artist.")) %>%
  str()
#
#
#
#
