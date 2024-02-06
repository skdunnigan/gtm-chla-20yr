library(here)
source(here('R', '00_loadpackages.R'))


# load-wrangle-export-wq-dat ----------------------------------------------
# this is used to make monthly average plots

### import data with `SWMPr::import_local()` and then clean it with `SWMPr::qaqc()` to screen observations
### check what the flags mean used in the `SWMPr::qaqc()` fxn here:  https://cdmo.baruch.sc.edu/data/qaqc.cfm.
### only data starting 2003-01-01


# pi <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmpiwq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31") 
# 
# ss <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmsswq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31")
# 
# fm <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmfmwq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31") 
# 
# pc <- SWMPr::import_local(path = here::here('data',
#                                             'SWMP'),
#                           station_code = 'gtmpcwq') %>%
#   SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>%
#   filter(datetimestamp > "2002-12-31")
# 
# 
# # monthly avg aggregated data ---------------------------------------------
# ### aggregate to monthly averages for temp and sal
# ### add in station name (for combining)
# pi_mean <- pi %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "Pine Island")
# 
# ss_mean <- ss %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "San Sebastian")
# 
# fm_mean <- fm %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "Fort Matanzas")
# 
# pc_mean <- pc %>%
#   SWMPr::aggreswmp(by = "months", params = c('temp', 'sal')) %>%
#   mutate(station = "Pellicer Creek")
# 
# # bind all stations into one dataframe
# wq_mean_dat <- bind_rows(pi_mean, ss_mean, fm_mean, pc_mean) %>%
#   mutate(station = factor(station, levels = c('Pine Island',
#                                               'San Sebastian',
#                                               'Fort Matanzas',
#                                               'Pellicer Creek')))
# 
# # yearly minimum aggregated data -----------------------------------------
# 
# fun_min <- function(x) min(x, na.rm = TRUE)
# 
# pi_min <- pi %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "Pine Island")
# 
# ss_min <- ss %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "San Sebastian")
# 
# fm_min <- fm %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "Fort Matanzas")
# 
# pc_min <- pc %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_min, params = c('temp', 'sal')) %>% mutate(station = "Pellicer Creek")
# 
# # bind all stations into one dataframe
# wq_min_dat <- bind_rows(pi_min, ss_min, fm_min, pc_min) %>% 
#   mutate(station = factor(station, levels = c('Pine Island',
#                                               'San Sebastian',
#                                               'Fort Matanzas',
#                                               'Pellicer Creek'))) %>% 
#   rename(temp_min = temp, 
#          sal_min = sal)
# 
# 
# # yearly maximum aggregatded data -----------------------------------------
# fun_max <- function(x) max(x, na.rm = TRUE)
# 
# pi_max <- pi %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "Pine Island")
# 
# ss_max <- ss %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "San Sebastian")
# 
# fm_max <- fm %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "Fort Matanzas")
# 
# pc_max <- pc %>% 
#   SWMPr::aggreswmp(by = "years", FUN = fun_max, params = c('temp', 'sal')) %>% mutate(station = "Pellicer Creek")
# 
# # bind all stations into one dataframe
# wq_max_dat <- bind_rows(pi_max, ss_max, fm_max, pc_max) %>% 
#   mutate(station = factor(station, levels = c('Pine Island',
#                                               'San Sebastian',
#                                               'Fort Matanzas',
#                                               'Pellicer Creek'))) %>% 
#   rename(temp_max = temp, 
#          sal_max = sal)
# 
# 
# # merge yearly minimum and maximum data -----------------------------------
# 
# wq_min_max <- left_join(wq_min_dat, wq_max_dat, by = c("datetimestamp", "station"))
# 
# 
# # export dataframes as .RData ---------------------------------------------
# 
# # export means, max, mins, and joint max-min
# save(wq_mean_dat, file = here('output', 'data', 'wq_mean_dat.RData'))
# save(wq_max_dat, file = here('output', 'data', 'wq_max_dat.RData'))
# save(wq_min_dat, file = here('output', 'data', 'wq_min_dat.RData'))
# save(wq_min_max, file = here('output', 'data', 'wq_min_max.RData'))
# 
# rm(list = ls())

# start from here with wq -------------------------------------------------

load(here('output', 'data', 'wq_mean_dat.RData'))
load(here('output', 'data', 'wq_max_dat.RData'))
load(here('output', 'data', 'wq_min_dat.RData'))
load(here('output', 'data', 'wq_min_max.RData'))

# monthly avg temperature 
a <- wq_mean_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = temp, color = station), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "top") +
  labs(x = "", y = "Temperature, Celsius", color = "")

# monthly avg sal
b <- wq_mean_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = sal, color = station), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Salinity, psu", color = "")

# stacked plot
a / b

# annual min and max plot
temp_max <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = temp_max), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Max. Temperature, Celsius", color = "") 

temp_min <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = temp_min), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Min. Temperature, Celsius", color = "")
  
sal_max <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = sal_max), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Max. Salinity, PSU", color = "")

sal_min <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = sal_min), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Min. Salinity, PSU", color = "")


a <- (temp_max + labs(title = "A")) + (sal_max + labs(title = "B")) 
b <- (temp_min + labs(title = "C")) + (sal_min + labs(title = "D"))

a / b 


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
MET_yr <- MET %>% 
  aggreswmp(by = "years", FUN = fun_in, params = c('totprcp')) %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  mutate(rainfall_cm = totprcp/10,
         dev.avg = rainfall_cm - 119.832)
# rainfall graph
MET_yr %>% 
ggplot() +
  geom_col(aes(x = datetimestamp, y = rainfall_cm), fill = "#0075AC",
           width = 150) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  geom_hline(yintercept = 119.834, linetype = "dashed") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black")) +
  labs(x = "", y = "Rainfall, cm")

MET_yr %>% 
  ggplot(aes(x = datetimestamp)) +
  geom_col(aes(y = dev.avg), , fill = "#0075AC",
           width = 150) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black")) +
  labs(x = "", y = "Deviation from Average Rainfall, cm") +
  annotate("text",
           label = "Wetter", 
           x = as.Date("2003-01-01"),
           y = 20) +
  annotate("text",
           label = "Drier", 
           x = as.Date("2003-01-01"),
           y = -30)

