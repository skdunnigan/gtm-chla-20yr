# prep for WIN data


# read in data ------------------------------------------------------------

win <- readxl::read_xlsx(here('data', 'WIN-STORET',
                              'win_revised.xlsx')) %>% 
  janitor::clean_names()

# inspect
glimpse(win)

# create lat and long data frames
win.sjr <- win %>% 
  filter(organization_id == "21FLSJWM" & activity_type == "Sample") %>% 
  select(monitoring_location_name, dep_latitude, dep_longitude) %>% 
  distinct() %>% 
  rename(lat = dep_latitude,
         lng = dep_longitude) %>% 
  filter(monitoring_location_name %in% c("TOL",
                                         "JXTR17",
                                         "MR312",
                                         "MRT",
                                         "PELFAV",
                                         "PEL"))

# selected sites for SJR
win.sjr %>% 
  leaflet(width = 900) %>% 
    addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions(),
               label = ~htmlEscape(monitoring_location_name),
               labelOptions = labelOptions(textsize = "15px"))

# there are a lot of neroc sites, need to filter based on sampling frequency

win.neroc <- win %>% 
  filter(organization_id == "21FLA" & activity_type == "Sample") %>% 
  select(monitoring_location_name, activity_start_date_time, dep_result_value_number) %>% 
  mutate(cont_year = lubridate::decimal_date(activity_start_date_time)) %>% 
  ggplot(aes(x = cont_year, y = dep_result_value_number)) + 
  geom_line() +
  facet_wrap(~monitoring_location_name, ncol = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(x = "Chlorophyll-a (\U00B5g/L)")

win.neroc %>% 
  leaflet(width = 900) %>% 
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             label = ~htmlEscape(monitoring_location_name),
             labelOptions = labelOptions(textsize = "15px"))