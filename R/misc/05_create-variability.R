# create variability figures using decomp function from cloern and jassby 2010

library(here)
source(here('R', '00_loadpackages.R'))
source(here('R', 'misc', '04_variability-fxns.R')) # load decomposition function from cloern and jassby 2010 supplemental material


# load data ---------------------------------------------------------------
# load all data that has had the missing values filled in with the GAM predictions
load(here('output', 'data', 'pi.RData'))
load(here('output', 'data', 'j17.RData'))
load(here('output', 'data', 'ss.RData'))
load(here('output', 'data', 'j21.RData'))
load(here('output', 'data', 'fm.RData'))
load(here('output', 'data', 'mrt.RData'))
load(here('output', 'data', 'pc.RData'))



# variability figures -----------------------------------------------------
## create variability figure (from Cloern and Jassby 2010)
var_fig(dat = mdat.pi, site = "GTMPINUT") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'GTMPINUT-variability.png'))

var_fig(dat = mdat.j17, site = "JXTR17") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'JXTR17-variability.png'))

var_fig(dat = mdat.ss, site = "GTMSSNUT") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'GTMSSNUT-variability.png'))

var_fig(dat = mdat.j21, site = "JXTR21") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'JXTR21-variability.png'))

var_fig(dat = mdat.fm, site = "GTMFMNUT") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'GTMFMNUT-variability.png'))

var_fig(dat = mdat.mrt, site = "MRT") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'MRT-variability.png'))

var_fig(dat = mdat.pc, site = "GTMPCNUT") %>% 
  ggsave(file = here('output', 'figures', 'variability', 'GTMPCNUT-variability.png'))


# variability sd tables ---------------------------------------------------

pi <- sd_fxn(dat = mdat.pi, station = "GTMPINUT")
j17 <- sd_fxn(dat = mdat.j17, station = "JXTR17")
ss <- sd_fxn(dat = mdat.ss, station = "GTMSSNUT")
j21 <- sd_fxn(dat = mdat.j21, station = "JXTR21")
fm <- sd_fxn(dat = mdat.fm, station = "GTMFMNUT")
mrt <- sd_fxn(dat = mdat.mrt, station = "MRT")
pc <- sd_fxn(dat = mdat.pc, station = "GTMPCNUT")

sd <- bind_rows(pi, j17, ss, j21, fm, mrt, pc) %>% select(4, 1:3)

write.csv(sd, file = here('output', 'data', 'sd-variability.csv'))

rm(list=ls())
