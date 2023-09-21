# load and combine sjr and swmp data
load(file = here('output', 'data', 'swmp.RData'))
load(file = here('output', 'data', 'sjr.RData'))

comb_dat <- bind_rows(sjr_dat, swmp_dat)
save(comb_dat, file = here("output", "data", "comb.RData"))


# check for pine island patterns

bind_rows(sjr_dat, swmp_dat) %>% 
  filter(station %in% c("gtmpinut", "JXTR17")) %>% 
  group_by(station, date) %>% # group by station and date to average duplicates
  summarise(value = mean(value, na.rm = T)) %>% # avg duplicates
  ungroup() %>% 
  mutate(yr = lubridate::year(date)) %>% 
  filter(yr > 2002 & yr < 2023) %>% 
  select(-yr) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~station, ncol = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(x = "Chlorophyll-a (\U00B5g/L)")

# check for SS patterns
bind_rows(sjr_dat, swmp_dat) %>% 
  filter(station %in% c("gtmssnut", "MR312")) %>% 
  group_by(station, date) %>% # group by station and date to average duplicates
  summarise(value = mean(value, na.rm = T)) %>% # avg duplicates
  ungroup() %>% 
  mutate(yr = lubridate::year(date)) %>% 
  filter(yr > 2002 & yr < 2023) %>% 
  select(-yr) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~station, ncol = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(x = "Chlorophyll-a (\U00B5g/L)")

# check for FM patterns
bind_rows(sjr_dat, swmp_dat) %>% 
  filter(station %in% c("gtmfmnut", "JXTR21")) %>% 
  group_by(station, date) %>% # group by station and date to average duplicates
  summarise(value = mean(value, na.rm = T)) %>% # avg duplicates
  ungroup() %>% 
  mutate(yr = lubridate::year(date)) %>% 
  filter(yr > 2002 & yr < 2023) %>% 
  select(-yr) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~station, ncol = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(x = "Chlorophyll-a (\U00B5g/L)")

# check for PC patterns
bind_rows(sjr_dat, swmp_dat) %>% 
  filter(station %in% c("gtmpcnut", "MRT")) %>% 
  group_by(station, date) %>% # group by station and date to average duplicates
  summarise(value = mean(value, na.rm = T)) %>% # avg duplicates
  ungroup() %>% 
  mutate(yr = lubridate::year(date)) %>% 
  filter(yr > 2002 & yr < 2023) %>% 
  select(-yr) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~station, ncol = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(x = "Chlorophyll-a (\U00B5g/L)")

dat <- comb_dat %>% 
  group_by(station, date) %>% # group by station and date to average duplicates
  summarise(value = mean(value, na.rm = T)) %>% # avg duplicates
  ungroup() %>%
  mutate(station = toupper(station), # capitalize all stations
         doy = lubridate::yday(date), # day of the year
         cont_year = lubridate::decimal_date(date), # date in decimal time
         yr = lubridate::year(date), # year
         mo = lubridate::month(date, label = TRUE), # month
         param = "chla") %>% # add param name
  filter(yr > 2002 & yr < 2023) %>% # only keep data from 2003-2022 
  filter(station != "MR312") # remove the SJR site near SS

save(dat, file = here('output', 'data', 'dat.RData'))

rm(list = ls())