# read in GTMNERR SWMP nutrient data, tidy, and export as swmp.Rdata

library(here)
source(here('R', '00_loadpackages.R'))


# load nutrient data ------------------------------------------------------

nms <- names(read_excel(here::here('data',
                                   'SWMP',
                                   'gtmnut2002-2023_QC_zeros-corrected.xlsx'), 
                        n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

NUT <- readxl::read_xlsx(here::here('data',
                                    'SWMP',
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


# wrangle and tidy --------------------------------------------------------

# only keep chlorophyll data
# clean up variable names, convert datetime into a date (only sampled monthly), keep only grab samples (remove diel), 
# only keep essential columns: station code, f_record, chla_n, f_chla_n, and date

chla <- NUT %>% 
  filter(!is.na(rep)) %>% # remove "S" reps in dataset
  select(station_code, 2:5, chla_n, f_chla_n) %>% # keep only chla data
  rename(datetimestamp = date_time_stamp) %>% # clean name
  mutate(date = lubridate::date(datetimestamp)) %>% # create date variable from POSIXct
  filter(monitoring_program == 1) %>% # keep only grab samples (remove DIEL)
  select(-monitoring_program, -rep, -datetimestamp) # remove columns once done

# replace all values below nominal base mdl of 0.55 with the base mdl 0.55
chla$chla_n[chla$chla_n < 0.55] <- 0.55 

# filter data to remove data that is flagged rejected and suspect

chla_dat <- chla %>% 
  filter(!grepl(c("<-3>"), f_chla_n) & 
           !grepl(c("<1>"), f_chla_n) &
           !grepl(c("<-4>"), f_chla_n)) %>% # remove rejected and suspect data
  select(-f_record, -f_chla_n) %>%  # remove qc columns after filtering
  rename(value = chla_n)

# use data from the diel sampling at PC to fill in gaps in PC monthly grab sample
# all data were collected within one hour of the original grab sample

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

# combine all PC diel sampler-related data 
pcISCO <- bind_rows(pc2013, pc2018, pc2022)

# merge PC diel data with entire chla series

swmp_dat <- bind_rows(chla_dat, pcISCO) %>% # combine data frames with the missing PC values
  rename(station = station_code) %>% # rename station code for ease
  group_by(station, date) %>%  # group by station and date
  summarize(value = mean(value, na.rm = T)) %>% # average all monthly replicates together for one monthly value
  ungroup() # ungroup dataset


# export as .RData file ---------------------------------------------------

save(swmp_dat, file = here("output", "data", "swmp.RData"))

# clear environment
rm(list = ls())
