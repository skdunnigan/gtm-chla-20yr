library(here)
source(here('R', '00_loadpackages.R'))


# load data ---------------------------------------------------------------
# load all data that has had the missing values filled in with the GAM predictions
load(here('output', 'data', 'pi.RData'))
load(here('output', 'data', 'ss.RData'))
load(here('output', 'data', 'fm.RData'))
load(here('output', 'data', 'pc.RData'))


# calculate fulcrums and bind into one df ---------------------------------

fulc_tbl <- function(dat){
  
  ts <- ts(as.data.frame(dat %>%
                           ungroup() %>% 
                           select(date, value) %>%
                           arrange(date) %>%
                           select(value)
  ),
  start = c(2003, 1),
  end = c(2022, 12),
  frequency = 12
  )
  
  
  print(phenoPhase(ts))
}

pi <- fulc_tbl(dat = mdat.pi) %>% mutate(site = "PI", dev.avg = fulcrum - (mean(pi$fulcrum, na.rm = T))) 
ss <- fulc_tbl(dat = mdat.ss) %>% mutate(site = "SS", dev.avg = fulcrum - (mean(ss$fulcrum, na.rm = T)))
fm <- fulc_tbl(dat = mdat.fm) %>% mutate(site = "FM", dev.avg = fulcrum - (mean(fm$fulcrum, na.rm = T)))
pc <- fulc_tbl(dat = mdat.pc) %>% mutate(site = "PC", dev.avg = fulcrum - (mean(pc$fulcrum, na.rm = T)))

fulcrums <- bind_rows(pi, ss, fm, pc) %>% 
  mutate(date = as.Date(paste0(year, "-","01", "-", "01")))


# fulcrums plots ----------------------------------------------------------

inlet <- tribble(
  ~site, ~type,
  "FM", "Marine-Influenced",
  "PC", "Freshwater-Influenced",
  "PI", "Freshwater-Influenced",
  "SS", "Marine-Influenced"
)

fulcrums %>% 
  left_join(inlet, by = "site") %>% 
  mutate(site = factor(site, levels = c('PI', 'SS', 'FM', 'PC'))) %>% 
  ggplot(aes(x = date, y = fulcrum, group = site)) +
  geom_point(aes(shape = site, color = type), size = 3) +
  geom_hline(aes(yintercept = 6.74), color = "gray20", linetype = "dashed") +
  scale_x_date(date_minor_breaks = "years", date_labels = "%Y") +
  scale_color_manual(values = c("black", "gray70")) +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(size = 12),
        legend.position = "top") +
  labs(x = "", 
       y = "Fulcrums of Cumulative Chl-a",
       shape = "Site",
       color = "Site Type")

rm(list = ls())
