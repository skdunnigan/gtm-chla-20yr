library(here)
library(wql)
library(tidyverse)

# load pi site data
load(here('output', 'data', 'pi.RData'))

# convert data into a mts object
pi_ts <- mdat.pi %>% 
  mutate(
    mo = lubridate::month(date, label = T)
  ) %>% 
  select(yr, mo, value) %>% 
  pivot_wider(values_from = 'value', names_from = 'mo') %>% 
  arrange(yr) %>% 
  select(-yr) %>% 
  ts(start = 2003, end = 2022, frequency = 1)

# Run empirical orthogonal function analysis
eof(pi_ts, 1)

