library(here)
source(here('R', '00_loadpackages.R'))


# load-wrangle-export-wq-dat ----------------------------------------------


### import data with `SWMPr::import_local()` and then clean it with `SWMPr::qaqc()` to screen observations
### check what the flags mean used in the `SWMPr::qaqc()` fxn here:  https://cdmo.baruch.sc.edu/data/qaqc.cfm.
### only data starting 2003-01-01
### aggregate to monthly averages for temp and sal
### add in station name (for combining)

# pi <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmpiwq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31") %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "Pine Island")
# 
# ss <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmsswq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31") %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "San Sebastian")
# 
# fm <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmfmwq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31") %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "Fort Matanzas")
# 
# pc <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmpcwq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31") %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "Pellicer Creek")
# 
# # bind all stations into one dataframe
# wq_dat <- bind_rows(pi, ss, fm, pc) %>%
#   mutate(station = factor(station, levels = c('Pine Island',
#                                               'San Sebastian',
#                                               'Fort Matanzas',
#                                               'Pellicer Creek')))
# # export combined dataframe as .RData
# save(wq_dat, file = here('output', 'data', 'wq_dat.RData'))
# 
# rm(list = ls())

# start from here with wq -------------------------------------------------

load(here('output', 'data', 'wq_dat.RData'))

# linetype
wq_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = sal, linetype = station)) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        strip.background = element_rect(fill = "transparent")) +
  labs(x = "", y = "Salinity, psu", linetype = "")

# color
wq_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = sal, color = station)) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        strip.background = element_rect(fill = "transparent")) +
  labs(x = "", y = "Salinity, psu", color = "")

# linetype
wq_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = temp, linetype = station)) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        strip.background = element_rect(fill = "transparent")) +
  labs(x = "", y = "Temperature, Celsius", linetype = "")

# color
wq_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = temp, color = station), linewidth = 1) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        strip.background = element_rect(fill = "transparent")) +
  labs(x = "", y = "Temperature, Celsius", color = "")

# MET ---------------------------------------------------------------------

fun_in <- function(x) {sum(x, na.rm = TRUE)}

# read in data

MET <- SWMPr::import_local(path = here::here('data',
                                             'swmp'), 
                           station_code = 'gtmpcmet') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 


MET_mo <- MET %>% 
  aggreswmp(by = "months", FUN = fun_in, params = c('totprcp')) %>% 
  filter(datetimestamp > "2002-12-31")

# save(MET_mo, file = here('output', 'data', 'MET_mo.RData'))

# rainfall graph
MET %>% 
  aggreswmp(by = "years", FUN = fun_in, params = c('totprcp')) %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  mutate(rainfall_cm = totprcp/10) %>% 
ggplot() +
  geom_col(aes(x = datetimestamp, y = rainfall_cm), fill = "#0075AC",
           width = 150) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  geom_hline(yintercept = 119.834, linetype = "dashed") +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black")) +
  labs(x = "", y = "Rainfall, cm")

