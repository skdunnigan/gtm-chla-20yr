# create variability figures using decomp function from cloern and jassby 2010

library(here)
source(here('R', '00_loadpackages.R'))
source(here('R', '04.1_variability-fxns.R')) # load decomposition function from cloern and jassby 2010 supplemental material


# load data ---------------------------------------------------------------
# load all data that has had the missing values filled in with the GAM predictions
load(here('output', 'data', 'pi.RData'))
load(here('output', 'data', 'ss.RData'))
load(here('output', 'data', 'fm.RData'))
load(here('output', 'data', 'pc.RData'))

# variability figures -----------------------------------------------------
## create variability figure (from Cloern and Jassby 2010)
var_fig(dat = mdat.pi, site = "PI", save = F) 

var_fig(dat = mdat.ss, site = "SS", save = F)

var_fig(dat = mdat.fm, site = "FM", save = F) 

var_fig(dat = mdat.pc, site = "PC", save = F) 


# variability sd tables ---------------------------------------------------

pi <- sd_fxn(dat = mdat.pi, station = "GTMPINUT")
ss <- sd_fxn(dat = mdat.ss, station = "GTMSSNUT")
fm <- sd_fxn(dat = mdat.fm, station = "GTMFMNUT")
pc <- sd_fxn(dat = mdat.pc, station = "GTMPCNUT")

sd <- bind_rows(pi, ss, fm, pc) %>% select(4, 1:3)

write.csv(sd, file = here('output', 'data', 'sd-variability.csv'))

# annual variability for manuscript ---------------------------------------

pi <- ann_var_fig(mdat.pi)
ss <- ann_var_fig(mdat.ss)
fm <- ann_var_fig(mdat.fm)
pc <- ann_var_fig(mdat.pc)

multi <- (pi + theme(axis.text.x = element_blank()) + labs(title = "A")) / 
  (ss + theme(axis.text.x = element_blank()) + labs(title = "B")) / 
  (fm + theme(axis.text.x = element_blank()) + labs(title = "C")) / 
  (pc + labs(title = "D"))

multi

ggsave(multi, filename = here('output', 'figures', 'finals', 'figure4.png'),
       dpi = 600, units = "in",
       width = 6.5, height = 7)

rm(list=ls())


# seasonal variability for manuscript -------------------------------------

pi <- seas_var_fig(mdat.pi)
ss <- seas_var_fig(mdat.ss)
fm <- seas_var_fig(mdat.fm)
pc <- seas_var_fig(mdat.pc)

multi <- (pi + theme(axis.text.x = element_blank()) + labs(title = "A")) / 
  (ss + theme(axis.text.x = element_blank()) + labs(title = "B")) / 
  (fm + theme(axis.text.x = element_blank()) + labs(title = "C")) / 
  (pc + labs(title = "D"))

multi

ggsave(multi, filename = here('output', 'figures', 'finals', 'figure6.png'),
       dpi = 600, units = "in",
       width = 6.5, height = 7)

rm(list=ls())



