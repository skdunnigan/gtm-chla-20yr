# load chl-a data

if(!require(here)){ install.packages("here") } ;  library(here) # easy paths
if(!require(dplyr)){ install.packages("dplyr") } ;  library(dplyr) # left_join
if(!require(tidyr)){ install.packages("tidyr") } ;  library(tidyr) # pipe operator %>% 
if(!require(ggplot2)){install.packages("ggplot2")} ; library(ggplot2) # plotting
if(!require(lubridate)){ install.packages("lubridate") } ;  library(tidyr) # pipe operator %>% 
if(!require(janitor)){ install.packages("janitor") } ;  library(janitor) # clean names
if(!require(readxl)){ install.packages("readxl") } ;  library(readxl) # clean names
if(!require(wqtrends)){ 
  options(repos = c(
    tbeptech = 'https://tbep-tech.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
  install.packages('wqtrends')} ; library(wqtrends) # models and plots for trends

if(!require(mgcv)){ install.packages("mgcv") } ;  library(mgcv)
if(!require(patchwork)) { install.packages("patchwork") }  ; library(patchwork)

if(!require(changepoint)) { install.packages("changepoint")}; library(changepoint)

if(!require(strucchange)) { install.packages('strucchange')}; library(strucchange)

nms <- names(read_excel(here::here('data',
                                   'All_inclusive_NUT',
                                   'gtmnut2002-2023_QC_zeros-corrected.xlsx'), 
                        n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

NUT <- readxl::read_xlsx(here::here('data',
                                    'All_inclusive_NUT',
                                    'gtmnut2002-2023_QC_zeros-corrected.xlsx'),
                         col_types = c("text", 
                                       "date", 
                                       "numeric", 
                                       "numeric", 
                                       "text", 
                                       class2)) %>% # specify how to read in these columns
  janitor::clean_names()

# clean environment
rm(nms, class, class2)

glimpse(NUT)

chla <- NUT %>% 
  filter(!is.na(rep)) %>% # remove "S" reps in dataset
  select(station_code, 2:5, chla_n, f_chla_n) %>% # keep only chla data
  rename(datetimestamp = date_time_stamp) %>% # clean name
  mutate(date = lubridate::date(datetimestamp)) %>% # create date variable from POSIXct
  filter(monitoring_program == 1) %>% # keep only grab samples (remove DIEL)
  select(-monitoring_program, -rep) # remove columns once done

chla$chla_n[chla$chla_n < 0.55] <- 0.55 # replace all values below nominal base mdl of 0.55 with the base mdl 0.55

chla_dat <- chla %>% 
  filter(!grepl(c("<-3>"), f_chla_n) & 
           !grepl(c("<1>"), f_chla_n) &
           !grepl(c("<-4>"), f_chla_n)) %>% # remove rejected and suspect data
  select(-f_record, -f_chla_n) %>% # remove qc columns after filtering
  group_by(station_code, date) %>% # group by station and date to average duplicates
  summarise(value = mean(chla_n, na.rm = T)) %>% # avg duplicates
  ungroup() %>% 
  mutate(doy = lubridate::yday(date), # day of the year
         cont_year = lubridate::decimal_date(date), # date in decimal time
         yr = lubridate::year(date), # year
         mo = lubridate::month(date, label = TRUE), # month
         param = "chla") %>% # add param name
  rename(station = station_code) %>% # clean variable name
  filter(yr > 2002 & yr < 2023) # only keep data from 2003-2022

pc2013 <- NUT %>% 
  filter(!is.na(rep)) %>% # remove "S" reps in dataset
  select(station_code, 2:5, chla_n, f_chla_n) %>% # keep only chla data
  rename(datetimestamp = date_time_stamp) %>% # clean name
  mutate(date = lubridate::date(datetimestamp)) %>%  # create date variable from POSIXct
  filter(station_code == "gtmpcnut") %>% 
  filter(grepl("2013-07-18 07:00", datetimestamp) | 
           grepl("2013-07-18 09:30", datetimestamp)) %>% 
  group_by(station_code, date) %>% 
  summarize(value = mean(chla_n)) %>% 
  ungroup()

### come back to 08 2015 given the high values
# pc2015 <- NUT %>% 
#             filter(!is.na(rep)) %>% # remove "S" reps in dataset
#             select(station_code, 2:5, chla_n, f_chla_n) %>% # keep only chla data
#             rename(datetimestamp = date_time_stamp) %>% # clean name
#             mutate(date = lubridate::date(datetimestamp)) %>%  # create date variable from POSIXct
#             filter(station_code == "gtmpcnut") %>% 
#             filter(grepl("2015-08", datetimestamp)) 

pc2018 <- NUT %>% 
  filter(!is.na(rep)) %>% # remove "S" reps in dataset
  select(station_code, 2:5, chla_n, f_chla_n) %>% # keep only chla data
  rename(datetimestamp = date_time_stamp) %>% # clean name
  mutate(date = lubridate::date(datetimestamp)) %>%  # create date variable from POSIXct
  filter(station_code == "gtmpcnut") %>% 
  filter(grepl("2018-11-06 10:30", datetimestamp)) %>% 
  select(station_code, date, chla_n) %>% 
  rename(value = chla_n)

# in 2022 they were not collected on the same day as the grab samples.
# cannot use 2022-07
pc2022 <- NUT %>% 
  filter(!is.na(rep)) %>% # remove "S" reps in dataset
  select(station_code, 2:5, chla_n, f_chla_n) %>% # keep only chla data
  rename(datetimestamp = date_time_stamp) %>% # clean name
  mutate(date = lubridate::date(datetimestamp)) %>%  # create date variable from POSIXct
  filter(station_code == "gtmpcnut") %>% 
  filter(grepl("2022-04-18 11:00", datetimestamp) |
           grepl("2022-08-22 09:00", datetimestamp)) %>% 
  select(station_code, date, chla_n) %>% 
  rename(value = chla_n)

pcISCO <- bind_rows(pc2013, pc2018, pc2022) %>% 
  mutate(doy = lubridate::yday(date), # day of the year
         cont_year = lubridate::decimal_date(date), # date in decimal time
         yr = lubridate::year(date), # year
         mo = lubridate::month(date, label = TRUE), # month
         param = "chla") %>% # add param name
  rename(station = station_code)

rm(pc2013, pc2018, pc2022)

chla_dat <- bind_rows(chla_dat, pcISCO)

save(chla_dat, file = here("output", "data", "chla.RData"))
