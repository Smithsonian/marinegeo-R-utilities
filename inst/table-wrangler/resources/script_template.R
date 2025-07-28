

## MarineGEO Table Wrangler Start ##
df %>%
  
  ### MarineGEO Code Chain Link - DPLYR
  
  mutate(`Date` = substr(Date,1,10)) %>%
  
  ### MarineGEO Code Chain Link - DPLYR
  
  mutate(Date = ymd(Date))
## MarineGEO Table Wrangler End ##