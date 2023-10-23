library(here)
source(here('R', '00_loadpackages.R'))


# load data ---------------------------------------------------------------
# load all data that has had the missing values filled in with the GAM predictions
load(here('output', 'data', 'pi.RData'))
load(here('output', 'data', 'j17.RData'))
load(here('output', 'data', 'ss.RData'))
load(here('output', 'data', 'j21.RData'))
load(here('output', 'data', 'fm.RData'))
load(here('output', 'data', 'mrt.RData'))
load(here('output', 'data', 'pc.RData'))

dat <- mdat.fm


# run lm on fulcrums

fulc_lm <- function(dat){
  
  # filename <- paste0(station,"-fulcrum.png")
  
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
  
  
  x <- phenoPhase(ts)
  y <- lm(fulcrum ~ year, data = x)
  # broom::tidy(y)
  # broom::glance(y)
  y
  
}

fulc_lm(dat = mdat.pi) %>% broom::glance()
fulc_lm(dat = mdat.j17) %>% broom::glance()
fulc_lm(dat = mdat.ss) %>% broom::glance()
fulc_lm(dat = mdat.j21) %>% broom::glance()
fulc_lm(dat = mdat.fm) %>% broom::glance()
fulc_lm(dat = mdat.mrt) %>% broom::glance()
fulc_lm(dat = mdat.pc) %>% broom::glance()
