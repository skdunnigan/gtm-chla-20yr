# combine monthly total rainfall, chla, avg temp, avg sal, mei for correlation

library(here)
source(here('R', '00_loadpackages.R'))
load(here('output', 'data', 'wq_dat.RData'))
load(here('output', 'data', 'comb.RData'))
load(here('output', 'data', 'MET_mo.RData'))
load(here('output', 'data', 'mei.RData'))