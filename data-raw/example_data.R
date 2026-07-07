library(tidyverse)
library(marinegeo.utils)

seagrass_cover_example <- marinegeo.utils::db_arrow_marinegeo("seagrass-cover-monitoring-v1") %>%
  filter(partner_code == "USA-IRL",
         year(sample_collection_date) > 2021) %>%
  collect()

save(seagrass_cover_example, file = "data/seagrass_cover_example.rda")
