library(here)
source(here('R', '00_loadpackages.R'))


# load-wrangle-export-wq-dat ----------------------------------------------
# this is used to make monthly average plots

### import data with `SWMPr::import_local()` and then clean it with `SWMPr::qaqc()` to screen observations
### check what the flags mean used in the `SWMPr::qaqc()` fxn here:  https://cdmo.baruch.sc.edu/data/qaqc.cfm.
### only data starting 2003-01-01


pi <- SWMPr::import_local(path = here::here('data',
                                            'SWMP'),
                          station_code = 'gtmpiwq') %>%
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
  filter(datetimestamp > "2002-12-31")

ss <- SWMPr::import_local(path = here::here('data',
                                            'SWMP'),
                          station_code = 'gtmsswq') %>%
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
  filter(datetimestamp > "2002-12-31")

fm <- SWMPr::import_local(path = here::here('data',
                                            'SWMP'),
                          station_code = 'gtmfmwq') %>%
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
  filter(datetimestamp > "2002-12-31")

pc <- SWMPr::import_local(path = here::here('data',
                                            'SWMP'),
                          station_code = 'gtmpcwq') %>%
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
  filter(datetimestamp > "2002-12-31")

# # monthly avg aggregated data ---------------------------------------------
# ### aggregate to monthly averages for temp and sal
# ### add in station name (for combining)
pi_mean <- pi %>%
  SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
  mutate(station = "Pine Island")

ss_mean <- ss %>%
  SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
  mutate(station = "San Sebastian")

fm_mean <- fm %>%
  SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
  mutate(station = "Fort Matanzas")

pc_mean <- pc %>%
  SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
  mutate(station = "Pellicer Creek")

# bind all stations into one dataframe
wq_mean_dat <- bind_rows(pi_mean, ss_mean, fm_mean, pc_mean) %>%
  mutate(station = factor(station, levels = c('Pine Island',
                                              'San Sebastian',
                                              'Fort Matanzas',
                                              'Pellicer Creek')))


# monthly minimum and maximum aggregated data ---
fun_min <- function(x) min(x, na.rm = TRUE)
fun_max <- function(x) max(x, na.rm = TRUE)

pi_mo_min <- pi %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_min, params = c('temp', 'sal')) %>% # first find daily minimums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily minimums for the month to get an average minimum
  mutate(station = "Pine Island") %>% 
  rename(temp_min = temp,
         sal_min = sal)

pi_mo_max <- pi %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_max, params = c('temp', 'sal')) %>% # first find daily maximums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily maximum for the month to get an average maximum
  mutate(station = "Pine Island") %>% 
  rename(temp_max = temp,
         sal_max = sal)
## 
ss_mo_min <- ss %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_min, params = c('temp', 'sal')) %>% # first find daily minimums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily minimums for the month to get an average minimum
  mutate(station = "San Sebastian") %>% 
  rename(temp_min = temp,
         sal_min = sal)

ss_mo_max <- ss %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_max, params = c('temp', 'sal')) %>% # first find daily maximums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily maximum for the month to get an average maximum
  mutate(station = "San Sebastian") %>% 
  rename(temp_max = temp,
         sal_max = sal)
##
fm_mo_min <- fm %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_min, params = c('temp', 'sal')) %>% # first find daily minimums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily minimums for the month to get an average minimum
  mutate(station = "Fort Matanzas") %>% 
  rename(temp_min = temp,
         sal_min = sal)

fm_mo_max <- fm %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_max, params = c('temp', 'sal')) %>% # first find daily maximums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily maximum for the month to get an average maximum
  mutate(station = "Fort Matanzas") %>% 
  rename(temp_max = temp,
         sal_max = sal)
##
pc_mo_min <- pc %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_min, params = c('temp', 'sal')) %>% # first find daily minimums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily minimums for the month to get an average minimum
  mutate(station = "Pellicer Creek") %>% 
  rename(temp_min = temp,
         sal_min = sal)

pc_mo_max <- pc %>% 
  SWMPr::aggreswmp(by = "days", FUN = fun_max, params = c('temp', 'sal')) %>% # first find daily maximums
  SWMPr::aggreswmp(by = "months") %>% # then average the daily maximum for the month to get an average maximum
  mutate(station = "Pellicer Creek") %>% 
  rename(temp_max = temp,
         sal_max = sal)

pi_mo_min_max <- pi_mo_min %>% left_join(pi_mo_max, by = c('datetimestamp', 'station'))
ss_mo_min_max <- ss_mo_min %>% left_join(ss_mo_max, by = c('datetimestamp', 'station'))
fm_mo_min_max <- fm_mo_min %>% left_join(fm_mo_max, by = c('datetimestamp', 'station'))
pc_mo_min_max <- pc_mo_min %>% left_join(pc_mo_max, by = c('datetimestamp', 'station'))

mo_min_max <- bind_rows(pi_mo_min_max,
                        ss_mo_min_max,
                        fm_mo_min_max,
                        pc_mo_min_max)

wq_mo_min_max_mean <- wq_mean_dat %>% 
                        rename(temp_mean = temp, 
                               sal_mean = sal) %>% 
                        left_join(mo_min_max, by = c('datetimestamp', 'station')) %>% 
                        select(datetimestamp, station, temp_mean, temp_min, temp_max,
                               sal_mean, sal_min, sal_max)

# save(wq_max_dat, file = here('output', 'data', 'wq_max_dat.RData'))
# save(wq_min_dat, file = here('output', 'data', 'wq_min_dat.RData'))
# save(wq_min_max, file = here('output', 'data', 'wq_min_max.RData'))

write.csv(wq_mo_min_max_mean, file = here('output', 'data', 'monthly-min-max-mean.csv'))

rm(list = ls())

#
# yearly minimum aggregated data -----------------------------------------

fun_min <- function(x) min(x, na.rm = TRUE)

pi_min <- pi %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "Pine Island")

ss_min <- ss %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "San Sebastian")

fm_min <- fm %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "Fort Matanzas")

pc_min <- pc %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "Pellicer Creek")

# bind all stations into one dataframe
wq_min_dat <- bind_rows(pi_min, ss_min, fm_min, pc_min) %>%
  mutate(station = factor(station, levels = c('Pine Island',
                                              'San Sebastian',
                                              'Fort Matanzas',
                                              'Pellicer Creek'))) %>%
  rename(temp_min = temp,
         sal_min = sal)


# yearly maximum aggregatded data -----------------------------------------
fun_max <- function(x) max(x, na.rm = TRUE)

pi_max <- pi %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "Pine Island")

ss_max <- ss %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "San Sebastian")

fm_max <- fm %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "Fort Matanzas")

pc_max <- pc %>%
  SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "Pellicer Creek")

# bind all stations into one dataframe
wq_max_dat <- bind_rows(pi_max, ss_max, fm_max, pc_max) %>%
  mutate(station = factor(station, levels = c('Pine Island',
                                              'San Sebastian',
                                              'Fort Matanzas',
                                              'Pellicer Creek'))) %>%
  rename(temp_max = temp,
         sal_max = sal)


# merge yearly minimum and maximum data -----------------------------------

wq_min_max <- left_join(wq_min_dat, wq_max_dat, by = c("datetimestamp", "station"))


# export dataframes as .RData ---------------------------------------------

# export means, max, mins, and joint max-min
save(wq_mean_dat, file = here('output', 'data', 'wq_mean_dat.RData'))


rm(list = ls())



