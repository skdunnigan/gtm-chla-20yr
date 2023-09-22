# environmental setting ----

## salinity ----
# plot salinity at four sites

WQ %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>% 
    ggplot() +
    geom_histogram(aes(x = sal), binwidth = 1, fill = "lightgray", color = "black") +
  facet_wrap(~station) +
    theme_classic() +
    labs(y = "Number",
         x = "Salinity (psu)")

# combined
WQ %>% 
  ggplot() +
  geom_histogram(aes(x = sal), binwidth = 1, fill = "lightgray", color = "black") +
  theme_classic() +
  labs(y = "Number",
       x = "Salinity (psu)")

## DO ----
# plot do at four sites

WQ %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = do_mgl), binwidth = 0.5, fill = "lightgray", color = "black") +
  # geom_vline(xintercept = 6, color = "red", linetype = "dashed") +
  facet_wrap(~station) +
  theme_classic() +
  labs(y = "Number",
       x = "Dissolved Oxygen (mg/L)")

# combined
WQ %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = do_mgl), binwidth = 0.5, fill = "lightgray", color = "black") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  labs(y = "Number",
       x = "Dissolved Oxygen (mg/L)")

## chlorophyll a ----
# plot chla at four sites
NUT_monthly %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = chla_n), binwidth = 1, fill = "lightgray", color = "black") +
  # geom_vline(xintercept = 20, color = "red", linetype = "dashed") +
  facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "Chlorophyll a (ug/L)")

NUT_monthly %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = chla_n), binwidth = 1, fill = "lightgray", color = "black") +
  theme_classic() +
  labs(y = "Number",
       x = "Chlorophyll a (ug/L)")

# nitrogen and phosphorus

n_dat %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = DINuM), binwidth = 1, fill = "lightgray", color = "black") +
  facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "DIN (uM)")

n_dat %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = DONuM), binwidth = 1, fill = "lightgray", color = "black") +
  # facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "DON (uM)")

n_dat %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = PNuM), binwidth = 1, fill = "lightgray", color = "black") +
  facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "PN (uM)")

n_dat %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = TNuM), binwidth = 1, fill = "lightgray", color = "black") +
  # facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "TN (uM)")

n_dat %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = DIPuM), binwidth = 0.1, fill = "lightgray", color = "black") +
  facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "DIP (uM)")

n_dat %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>%
  ggplot() +
  geom_histogram(aes(x = TPuM), binwidth = 0.1, fill = "lightgray", color = "black") +
  facet_wrap(~station_code) +
  theme_classic() +
  labs(y = "Number",
       x = "TP (uM)")