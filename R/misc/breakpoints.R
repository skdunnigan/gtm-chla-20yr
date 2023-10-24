library(here)
source(here('R', '00_loadpackages.R'))

# load data
load(here('output', 'data', 'pi.RData'))
load(here('output', 'data', 'j17.RData'))
load(here('output', 'data', 'ss.RData'))
load(here('output', 'data', 'j21.RData'))
load(here('output', 'data', 'fm.RData'))
load(here('output', 'data', 'mrt.RData'))
load(here('output', 'data', 'pc.RData'))

# create mtx timeseries object for breakpoints

br_fxn <- function(dat){

ts2 <- dat %>% 
  mutate(
    mo = lubridate::month(date, label = T)
  ) %>% 
  select(yr, mo, value) %>% 
  pivot_wider(values_from = 'value', names_from = 'mo') %>% 
  arrange(yr) %>% 
  select(-yr) %>% 
  ts(start = 2003, end = 2022, frequency = 1)

# calculate breakpoints
br <- breakpoints(ts2 ~ 1)

}

# br$breakpoints
# br$datatsp
# br$RSS
# 
# # summary
# summary(br)
# 
# # confidence interval
# confint(br)
# 
# coef(br)
# 
# fitted(br)
# 
# breakdates(br)

br_fxn(dat = mdat.pi) %>% summary()
br_fxn(dat = mdat.pi) %>% breakdates()
br_fxn(dat = mdat.pi) %>% confint()

br_fxn(dat = mdat.j17) %>% summary()
br_fxn(dat = mdat.j17) %>% breakdates()
br_fxn(dat = mdat.j17) %>% confint()

br_fxn(dat = mdat.ss) %>% summary()
br_fxn(dat = mdat.ss) %>% breakdates()

br_fxn(dat = mdat.j21) %>% summary()
br_fxn(dat = mdat.j21) %>% breakdates()

br_fxn(dat = mdat.fm) %>% summary()
br_fxn(dat = mdat.fm) %>% breakdates()

br_fxn(dat = mdat.mrt) %>% summary()
br_fxn(dat = mdat.mrt) %>% breakdates()

br_fxn(dat = mdat.pc) %>% summary()
br_fxn(dat = mdat.pc) %>% breakdates()