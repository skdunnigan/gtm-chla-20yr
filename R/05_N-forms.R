# using methods based on Cloern et al. 2020
# Nitrogen partitioning
# By SWMP Station

# # load data
# source(here::here('R', '00_loadpackages.R'))
# source(here::here('R', '02.1_load_wrangle_NUT.R'))

n_dat <- 
  NUT_monthly %>% 
  filter(datetimestamp >= "2002-01-01 00:30:00") %>% 
  select(station_code, datetimestamp, nh4f, no23f, tkn, tknf, chla_n, tss, po4f, tp) %>% 
  rename(dip = po4f) %>% 
  mutate(NH4uM = nh4f * (1000/14.01),
         NO23uM = no23f * (1000/14.01),
         TKNuM = tkn * (1000/14.01),
         TKNFuM = tknf * (1000/14.01),
         DIPuM = dip * (1000/30.97),
         TPuM = tp * (1000/30.97),
         DIN = no23f + nh4f,
         TN = tkn + no23f,
         DON = tknf - nh4f,
         PN = TN - (DIN + DON),
         DINuM = NO23uM + NH4uM,
         TNuM = TKNuM + NO23uM,
         DONuM = TKNFuM - NH4uM,
         PNuM = TNuM - (DINuM + DONuM))

stations <- unique(n_dat$station_code)

for (i in 1:length(stations)){
  
  tempdf <- lm(PNuM ~ tss + chla_n, data = (na.exclude(n_dat %>% 
                                                         filter(station_code == stations[i] & PNuM > 0)))
  )
  
  print(stations[i])
  print(summary(tempdf))
  print(broom::tidy(tempdf))
  # print(broom::glance(tempdf))
  
  N_sed <- summary(tempdf)$coefficients[2,1]
  N_phyt <- summary(tempdf)$coefficients[3,1]
  N_res <- summary(tempdf)$coefficients[1,1]
  
  df <- n_dat %>% filter(station_code == stations[i]) %>% 
    mutate(N_sed = N_sed*tss,
           N_phyt = N_phyt*chla_n,
           N_res = N_res)
  
  name <- paste0(stations[i]) 
  assign(paste0("calc", "_", name), df)
  
  rm(tempdf, name, i, df, N_phyt, N_sed, N_res)
}

n_dat2 <- as.data.frame(bind_rows(`calc_San Sebastian`, 
                                  `calc_Pellicer Creek`, 
                                  `calc_Pine Island`, 
                                  `calc_Fort Matanzas`)
                        ) %>% 
  mutate(N_limit = 100*(DINuM/(DINuM + 1.6)),
         P_limit = 100*(DIPuM/(DIPuM + 0.24)),
         TN_TP = TN/tp,
         TN_TPuM = TNuM/TPuM,
         DIN_DIPuM = DINuM/DIPuM,
         DIN_DIP = DIN/dip,
         N_sed_mg = N_sed * 14.01/1000,
         N_phyt_mg = N_phyt * 14.01/1000, 
         N_eff = chla_n/DIN,
         N_effuM = chla_n/DINuM,
         P_eff = chla_n/dip,
         P_effuM = chla_n/DIPuM)

rm(`calc_San Sebastian`, 
   `calc_Pellicer Creek`, 
   `calc_Pine Island`, 
   `calc_Fort Matanzas`,
   stations)

# resume at step 5
# # calculate min, max, and mean of each parameter
# sites_calc <- sites %>%
#   select(-DATE) %>%
#   group_by(STATION_CODE) %>%
#   summarise(across(where(is.numeric), list(min = min, max = max, med = median, mean = mean), na.rm = TRUE))
# 
# count <- sites %>%
#   group_by(STATION_CODE) %>%
#   summarise(across(everything(), ~ n()))
# 
# # replace columns in "count" with a _N to identify them as a count
# colnames(count) <- paste(colnames(count), sep = "_", "N")
# count <- count %>% rename(STATION_CODE = STATION_CODE_N) %>% select(-DATE_N)
# all <- sites_calc %>%
#   left_join(count, by = "STATION_CODE") %>%
#   mutate(Nitrogen_eff = CHLA_N_mean/DIN_mean,
#          Nitrogen_effuM = CHLA_N_mean/DINuM_mean,
#          Phosphorus_eff = CHLA_N_mean/DIP_mean,
#          Phosphorus_effuM = CHLA_N_mean/DIPuM_mean)
# 
# n_dat2 %>% select(-)