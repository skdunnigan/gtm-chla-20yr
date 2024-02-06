# Run Generalized Additive Models and export fit and summary information 
library(here)
source(here('R', '00_loadpackages.R'))

# load swmp chla data
load(file = here("output", "data", "swmp.RData")) # load prepped data file


# look at distribution of data --------------------------------------------

# raw
swmp_dat %>% 
  mutate(station = toupper(station),
         station = factor(station, 
                          levels = c("GTMPINUT",
                                     "GTMSSNUT",
                                     "GTMFMNUT",
                                     "GTMPCNUT"))) %>% 
  ggplot(aes(x = value, y = station, fill = station, color = station)) +
  ggdist::stat_slab(aes(thickness = after_stat(pdf*n)), alpha = 0.8, scale = 0.7) +
  ggdist::stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +
  scale_fill_okabeito() +
  scale_color_okabeito() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Chlorophyll-a (\U00B5g/L)",
       y = "",
       title = "Identity") 

# log10 transformed
swmp_dat %>% 
  mutate(station = toupper(station),
         station = factor(station, 
                          levels = c("GTMPINUT",
                                     "GTMSSNUT",
                                     "GTMFMNUT",
                                     "GTMPCNUT"))) %>%
  ggplot(aes(x = log10(value), y = station, fill = station)) +
  ggdist::stat_slab(aes(thickness = after_stat(pdf*n)), alpha = 0.8, scale = 0.7) +
  ggdist::stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Chlorophyll-a (\U00B5g/L)",
       y = "",
       title = "Log10") 


# prepare for GAM models --------------------------------------------------

# add variables to prep for GAM model analysis

dat <- swmp_dat %>% 
        mutate(station = toupper(station), # capitalize all stations
               doy = lubridate::yday(date), # day of the year
               cont_year = lubridate::decimal_date(date), # date in decimal time
               yr = lubridate::year(date), # year
               mo = lubridate::month(date, label = TRUE), # month
               param = "chla") %>% # add param name
        filter(yr > 2002 & yr < 2023)

# check out data
glimpse(dat)

# create models for each site and export them

# pine island -------------------------------------------------------------


pi.mod <- dat %>%
          filter(station %in% "GTMPINUT") # only keep pine island data

pi.mod <- wqtrends::anlz_gam(pi.mod, trans = "log10")

pdf(file = here('output', 'models', 'pi-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(pi.mod)
dev.off()

pi.sum <- anlz_smooth(pi.mod) %>% mutate(station = "GTMPINUT")
pi.fit <- anlz_fit(pi.mod) %>% mutate(station = "GTMPINUT")

save(pi.mod, file = here('output', 'models', 'pi.mod.RData'))

# san sebastian -----------------------------------------------------------


ss.mod <- dat %>%
  filter(station %in% "GTMSSNUT") # only keep gtmssnut data

ss.mod <- wqtrends::anlz_gam(ss.mod, trans = "log10")

pdf(file = here('output', 'models', 'ss-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(ss.mod)
dev.off()

ss.sum <- anlz_smooth(ss.mod) %>% mutate(station = "GTMSSNUT")
ss.fit <- anlz_fit(ss.mod) %>% mutate(station = "GTMSSNUT")

save(ss.mod, file = here('output', 'models', 'ss.mod.RData'))

# fort matanzas -----------------------------------------------------------

fm.mod <- dat %>%
  filter(station %in% "GTMFMNUT") # only keep fm data

fm.mod <- wqtrends::anlz_gam(fm.mod, trans = "log10")

pdf(file = here('output', 'models', 'fm-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(fm.mod)
dev.off()

fm.sum <- anlz_smooth(fm.mod) %>% mutate(station = "GTMFMNUT")
fm.fit <- anlz_fit(fm.mod) %>% mutate(station = "GTMFMNUT")

save(fm.mod, file = here('output', 'models', 'fm.mod.RData'))


# pellicer creek ----------------------------------------------------------

pc.mod <- dat %>%
  filter(station %in% "GTMPCNUT") # only keep pc data

pc.mod <- wqtrends::anlz_gam(pc.mod, trans = "log10")

pdf(file = here('output', 'models', 'pc-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(pc.mod)
dev.off()

pc.sum <- anlz_smooth(pc.mod) %>% mutate(station = "GTMPCNUT")
pc.fit <- anlz_fit(pc.mod) %>% mutate(station = "GTMPCNUT")

save(pc.mod, file = here('output', 'models', 'pc.mod.RData'))


# combine summary and model fit -------------------------------------------

sum <- bind_rows(pi.sum,
                  ss.sum,
                  fm.sum,
                  pc.sum) %>% select(station, 1:5)
fit <- bind_rows(pi.fit,
                 ss.fit,
                 fm.fit,
                 pc.fit) %>% select(station, 1:3)

write.csv(sum, file = here('output', 'models', 'gam.summary.csv'))
write.csv(fit, file = here('output', 'models', 'gam.fit.csv'))

dev.off()
rm(list = ls())
