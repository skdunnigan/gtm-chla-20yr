# sjr data

sjr <- read.csv(here('data', 'sjrwmd', 'Data2.csv')) %>% janitor::clean_names()

glimpse(sjr)

sjr_dat <- sjr %>% 
  mutate(datetime = as.POSIXct(sample_collection_date_and_time, format = 
                                 "%m/%d/%Y %H:%M", tz = "EST"),
         date = as.Date(datetime)) %>% 
  select(station, date, measured_value) %>% 
  rename(value = measured_value) %>% 
  filter(station %in% c('JXTR17', 'JXTR21', 'MR312', 'MRT')) %>%
  mutate(yr = lubridate::year(date)) %>% 
  filter(yr > 2002 & yr < 2023) %>% 
  select(-yr)
  
save(sjr_dat, file = here("output", "data", "sjr.RData"))

rm(list = ls())