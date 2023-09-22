load(file = here("output", "data", "dat.RData")) # load prepped data file

# look at distribution of data 
a <- dat %>% 
        mutate(station = factor(station, 
                                levels = c("GTMPINUT",
                                           "JXTR17",
                                           "GTMSSNUT",
                                           "JXTR21",
                                           "GTMFMNUT",
                                           "MRT",
                                           "GTMPCNUT"))) %>% 
        ggplot(aes(x = value, y = station, fill = station)) +
        ggdist::stat_slab(aes(thickness = after_stat(pdf*n)), alpha = 0.8, scale = 0.7) +
        ggdist::stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +
        scale_fill_okabeito() +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Chlorophyll-a (\U00B5g/L)",
             y = "",
             title = "Identity") 
ggsave(a, filename = here('output', 'qaqc', 'identity-distribution.png'))

a <- dat %>% 
  mutate(station = factor(station, 
                          levels = c("GTMPINUT",
                                     "JXTR17",
                                     "GTMSSNUT",
                                     "JXTR21",
                                     "GTMFMNUT",
                                     "MRT",
                                     "GTMPCNUT"))) %>% 
  ggplot(aes(x = log10(value), y = station, fill = station)) +
  ggdist::stat_slab(aes(thickness = after_stat(pdf*n)), alpha = 0.8, scale = 0.7) +
  ggdist::stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +
  scale_fill_okabeito() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Chlorophyll-a (\U00B5g/L)",
       y = "",
       title = "Log10") 
ggsave(a, filename = here('output', 'qaqc', 'log10-distribution.png'))

rm(a)

rm(list = ls())
