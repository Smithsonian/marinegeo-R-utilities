library(tidyverse)

seagrass_cover_example <- read_csv(
  "C:/Users/marine/Documents/repositories/marinegeo-seagrass-monitoring/L2-data/seagrass-cover-monitoring-v1/cover_USA-IRL_2025_seagrass.csv"
)

save(seagrass_cover_example, file = "data/seagrass_cover_example.rda")
